# ============================================================
# 4_sector_lstm.r — pooled vs sector vs single-target LSTM (plan Step 5)
# ============================================================
# Inputs : 1_data/processed/haver_yoy_q.csv, 1_data/haver_series_map.csv,
#          1_data/processed/sector_targets.csv, wdi_all_long.csv,
#          3_outputs/sector_lstm/rhs_sector_sets.csv   (from 3_rhs_selection.r)
# Outputs: 3_outputs/sector_lstm/{pooled,sector,single}/
#   val_predictions.csv  walk-forward 2020-2025, long: model, target, cutoff,
#                        target_year, actual, pred
#   nowcast.csv          predictions for target years without an actual yet
#   cv_scores.csv        hyperparameter CV score per model and config
#   checkpoints/*.rds    one per model (a re-run skips finished models)
#
# Setups (all: 12-quarter window, own prior-year value per output head,
# fold normalisation, 4-config CV on pre-2020 folds, 5-seed average)
#   pooled : 1 model, 50 targets, union of the five sector predictor sets
#   sector : 5 models, 10 targets each, that sector's predictor set
#   single : 50 models, 1 target each, its sector's predictor set
#
# Usage  : Rscript 2_code/4_sector_lstm.r [sector] [pooled] [single]
#          (no argument = all three, in that order). Several hours on CPU;
#          keep the computer awake — sleep pauses the run.
# ============================================================

script_dir <- local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a))) else
    tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) "2_code")
})
source(file.path(script_dir, "0_functions.R"))
n_threads <- Sys.getenv("SECTOR_LSTM_THREADS", unset = "")   # e.g. 2 for parallel runs
setup_keras(threads = if (nzchar(n_threads)) n_threads else NULL)

project_root   <- get_project_root()
processed_path <- file.path(project_root, "1_data", "processed")
out_root       <- file.path(project_root, "3_outputs", "sector_lstm")

TRAIN_START   <- 2000L
VAL_START     <- 2020L
NOWCAST_YEARS <- 2025:2026
WINDOW        <- 12L        # quarters
MIN_OBS       <- 4L         # a series counts as observed with >= 4 quarters in the window
MIN_SHARE     <- 0.5        # keep a sample if >= 50% of its series are observed
SEEDS         <- c(11L, 22L, 33L, 44L, 55L)
CV_SEED       <- 11L
CV_YEARS      <- TRAIN_START:(VAL_START - 1L)
GRID <- expand.grid(lstm_units = c(16L, 32L), dropout = c(0.2, 0.4),
                    dense_units = 16L, lr = 5e-4, stringsAsFactors = FALSE)
EPOCHS <- 100L; PATIENCE <- 15L; BATCH <- 16L
CV_EPOCHS <- 60L; CV_PATIENCE <- 10L

args   <- commandArgs(trailingOnly = TRUE)
QUICK  <- "--quick" %in% args     # smoke test: 1 seed, 1 config, few epochs, sector A only
# --part=k/n : run only the k-th of n slices of each setup's models (parallel runs);
#              CSVs are written only once every model of a setup has a checkpoint
part_arg <- sub("^--part=", "", grep("^--part=", args, value = TRUE))
PART <- if (length(part_arg)) as.integer(strsplit(part_arg, "/")[[1]]) else c(1L, 1L)
setup_args <- intersect(c("sector", "pooled", "single"), args)
SETUPS <- if (length(setup_args)) setup_args else c("sector", "pooled", "single")
stopifnot(length(SETUPS) > 0)
if (QUICK) {
  SEEDS <- 11L; GRID <- GRID[1, , drop = FALSE]; EPOCHS <- 5L; CV_EPOCHS <- 3L
  if (!length(setup_args)) SETUPS <- "sector"
  out_root_sets <- out_root
  out_root <- file.path(project_root, "3_outputs", "sector_lstm_quicktest")
}

# ------------------------------------------------------------
# 1. Data
# ------------------------------------------------------------
hv_map  <- read_csv(file.path(project_root, "1_data", "haver_series_map.csv"),
                    show_col_types = FALSE) %>% filter(keep)
yoy     <- read_csv(file.path(processed_path, "haver_yoy_q.csv"), show_col_types = FALSE) %>%
  mutate(date = as.Date(date))
targets <- read_csv(file.path(processed_path, "sector_targets.csv"), show_col_types = FALSE)
wdi     <- read_csv(file.path(processed_path, "wdi_all_long.csv"), show_col_types = FALSE)
sets    <- read_csv(file.path(if (QUICK) out_root_sets else out_root, "rhs_sector_sets.csv"),
                    show_col_types = FALSE)

hv_lagged    <- apply_pub_lags(yoy, setNames(hv_map$pub_lag_q, hv_map$code))   # lags first
targets_wide <- build_target_wide(wdi, targets)
last_actual  <- vapply(targets$code, function(t) max(targets_wide$year[!is.na(targets_wide[[t]])]),
                       numeric(1))

set_for <- function(s) sets$code[sets$sector == s]
models <- list(
  pooled = list(pooled = list(targets = targets$code, inputs = set_for("Pooled"))),
  sector = setNames(lapply(LETTERS[1:5], function(s)
    list(targets = targets$code[targets$sector == s], inputs = set_for(s))), LETTERS[1:5]),
  single = setNames(lapply(seq_len(nrow(targets)), function(i)
    list(targets = targets$code[i], inputs = set_for(targets$sector[i]))), targets$code)
)
stopifnot(length(models$pooled$pooled$inputs) == n_distinct(sets$code[sets$sector != "Pooled"]),
          all(lengths(lapply(models$sector, `[[`, "targets")) == 10),
          length(models$single) == 50)
if (QUICK) { models$sector <- models$sector["A"]; models$single <- models$single[1] }

# ------------------------------------------------------------
# 2. One model: sequences -> CV -> walk-forward -> nowcast
# ------------------------------------------------------------
run_model <- function(setup, id, spec) {
  ck_dir <- file.path(out_root, setup, "checkpoints"); dir.create(ck_dir, recursive = TRUE,
                                                                  showWarnings = FALSE)
  ck <- file.path(ck_dir, paste0(gsub("[^A-Za-z0-9]", "_", id), ".rds"))
  if (file.exists(ck)) { message(sprintf("[%s %s] checkpoint found, skipping", setup, id))
                         return(readRDS(ck)) }
  t0 <- Sys.time()
  seqs <- build_lstm_sequences(hv_lagged, spec$inputs, targets_wide, spec$targets,
                               window = WINDOW, min_obs = MIN_OBS, min_share = MIN_SHARE,
                               years = TRAIN_START:2100, extra_years = NOWCAST_YEARS)
  # shape + leakage assertions
  stopifnot(identical(dim(seqs$X)[2:3], c(WINDOW, length(spec$inputs))),
            ncol(seqs$Y) == length(spec$targets), min(seqs$pairs$target_year) >= TRAIN_START)
  message(sprintf("[%s %s] X = [%s], targets = %d, years %d-%d", setup, id,
                  paste(dim(seqs$X), collapse = " x "), length(spec$targets),
                  min(seqs$pairs$target_year), max(seqs$pairs$target_year)))

  cv <- run_lstm_cv(seqs, hv_lagged, GRID, CV_YEARS, prior_mode = "head_own", norm = "fold",
                    epochs = CV_EPOCHS, patience = CV_PATIENCE, batch_size = BATCH,
                    seed = CV_SEED, verbose = FALSE)
  stopifnot(all(seqs$pairs$target_year[unlist(lapply(cv$folds, `[[`, "va"))] < VAL_START))
  wf <- run_walk_forward_lstm(seqs, hv_lagged, cv$best, val_start = VAL_START, seeds = SEEDS,
                              prior_mode = "head_own", norm = "fold", epochs = EPOCHS,
                              patience = PATIENCE, batch_size = BATCH, verbose = FALSE)
  nc <- run_lstm_nowcast(seqs, hv_lagged, cv$best, NOWCAST_YEARS, seeds = SEEDS,
                         prior_mode = "head_own", norm = "fold", epochs = EPOCHS,
                         patience = PATIENCE, batch_size = BATCH)

  act <- seqs$Y[wf$val_idx, , drop = FALSE]
  val <- bind_rows(lapply(spec$targets, function(t) tibble(
    setup = setup, model = id, target = t, cutoff = wf$val_pairs$cutoff,
    target_year = wf$val_pairs$target_year, actual = act[, t],
    pred = ifelse(is.nan(wf$pred[, t]), NA_real_, wf$pred[, t])))) %>%
    filter(!is.na(actual))
  now <- if (is.null(nc)) tibble() else bind_rows(lapply(spec$targets, function(t) tibble(
    setup = setup, model = id, target = t, cutoff = nc$pairs$cutoff,
    target_year = nc$pairs$target_year, pred = nc$pred[, t]))) %>%
    filter(target_year > last_actual[target])
  if (!nrow(val) || all(is.na(val$pred)))   # never cache a failed model as finished
    stop(sprintf("[%s %s] no walk-forward predictions produced; checkpoint not saved", setup, id))
  res <- list(val = val, nowcast = now,
              cv = cv$scores %>% mutate(setup = setup, model = id, .before = 1),
              best = cv$best, n_samples = nrow(seqs$pairs),
              minutes = as.numeric(difftime(Sys.time(), t0, units = "mins")))
  saveRDS(res, ck)
  message(sprintf("[%s %s] done in %.1f min (best: units=%d, dropout=%.1f)", setup, id,
                  res$minutes, cv$best$lstm_units, cv$best$dropout))
  res
}

# ------------------------------------------------------------
# 3. Run setups and write outputs
# ------------------------------------------------------------
ck_path <- function(setup, id)
  file.path(out_root, setup, "checkpoints", paste0(gsub("[^A-Za-z0-9]", "_", id), ".rds"))

for (setup in SETUPS) {
  ids  <- names(models[[setup]])
  mine <- ids[(seq_along(ids) - 1L) %% PART[2] == (PART[1] - 1L)]
  message("\n==== Setup: ", setup, " (", length(mine), " of ", length(ids), " models, part ",
          PART[1], "/", PART[2], ") ====")
  invisible(lapply(mine, function(id) run_model(setup, id, models[[setup]][[id]])))
  if (!all(file.exists(vapply(ids, function(id) ck_path(setup, id), character(1))))) {
    message("Setup ", setup, ": other parts still running; CSVs not written yet")
    next
  }
  res <- lapply(ids, function(id) readRDS(ck_path(setup, id)))
  od <- file.path(out_root, setup)
  write_csv(bind_rows(lapply(res, `[[`, "val")),     file.path(od, "val_predictions.csv"))
  write_csv(bind_rows(lapply(res, `[[`, "nowcast")), file.path(od, "nowcast.csv"))
  write_csv(bind_rows(lapply(res, `[[`, "cv")),      file.path(od, "cv_scores.csv"))
  message(sprintf("Setup %s written: %.0f min in total", setup,
                  sum(vapply(res, `[[`, numeric(1), "minutes"))))
}
