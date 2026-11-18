# ============================================================
# 3_rhs_selection.r — choose the Haver predictors (RHS) for each sector (CP3)
# ============================================================
# Inputs : 1_data/processed/haver_yoy_q.csv, 1_data/haver_series_map.csv,
#          1_data/processed/sector_targets.csv, wdi_all_long.csv
# Outputs: 3_outputs/sector_lstm/
#   rhs_sector_sets.csv        FINAL predictor set per sector + pooled union
#   rhs_target_picks.csv       each target's top LASSO picks (cluster level)
#   rhs_selection.csv          per sector x cluster summary (all candidates)
#   rhs_selection_freq.csv     per target x candidate frequency (non-zero)
#   rhs_clusters.csv           near-duplicate groups (|r| > CLUSTER_R)
#   rhs_stepwise.csv           forward stepwise (BIC) picks per target
#   rhs_robustness.csv         LASSO vs stepwise overlap (Jaccard) per target
#   rhs_selection_heatmap.png  frequency heatmap, selected predictors x targets
#
# Method (revised at CP3, 2026-11-17: "per-target union")
#   1. Selection sample: target years 2000-2019 only (asserted: no 2020+).
#   2. Features: YTD mean (at each quarterly cutoff) of every lagged, YoY-
#      transformed Haver candidate in the sector pool (>= MIN_CAND_YEARS years
#      of real data), plus the target's own prior-year value and the cutoff
#      quarter (both unpenalised).
#   3. Near-duplicates: candidates whose quarterly YoY series correlate with
#      |r| > CLUSTER_R over 2000-2019 form one cluster (leader = longest
#      history). All members stay in the LASSO; a cluster counts as picked in a
#      draw if ANY member is non-zero. Nothing is removed from the Haver map.
#   4. LASSO stability selection per target: cv.glmnet (lambda.1se), folds
#      grouped by year, N_DRAWS subsamples of 80% of years.
#   5. Each target contributes its top TOP_PER_TARGET clusters with frequency
#      >= PICK_FREQ, represented by the cluster's most-selected member. The
#      sector set is the union, ranked by #targets then max frequency, capped
#      at MAX_RHS.
#   6. If a sector has fewer than MIN_RHS LASSO picks, it is topped up to
#      CORE_FILL_TO with an economic core set (labelled source = "core").
#   7. Robustness: forward stepwise with BIC on one row per year (Q4 cutoff),
#      cluster leaders only, at most STEP_MAX additions; Jaccard overlap with
#      the target's LASSO set (all clusters with frequency >= PICK_FREQ).
#   8. Pooled model inputs = union of the five sector sets.
# ============================================================

script_dir <- local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a))) else
    tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) "2_code")
})
source(file.path(script_dir, "0_functions.R"))
suppressPackageStartupMessages({ library(glmnet); library(MASS, exclude = "select"); library(ggplot2) })

project_root   <- get_project_root()
processed_path <- file.path(project_root, "1_data", "processed")
out_dir        <- file.path(project_root, "3_outputs", "sector_lstm")
ckpt_dir       <- file.path(out_dir, "checkpoints")
dir.create(ckpt_dir, recursive = TRUE, showWarnings = FALSE)

SEL_YEARS      <- 2000:2019
MIN_CAND_YEARS <- 8
CLUSTER_R      <- 0.95
CLUSTER_MIN_N  <- 30      # quarters of overlap needed to cluster a pair
SET_MAX_R      <- 0.90    # max |r| between two predictors in a final sector set
N_DRAWS        <- 50
SUBSAMPLE_FRAC <- 0.8
N_FOLDS        <- 5
PICK_FREQ      <- 0.50
TOP_PER_TARGET <- 2
MAX_RHS        <- 12
MIN_RHS        <- 4
CORE_FILL_TO   <- 6
STEP_MAX       <- 6
SEED           <- 2026
CKPT_TAG       <- "v2"

# Economic core sets (fallback only; order = priority)
CORE <- list(
  A = c("N293GVQ@emergela", "N293DVM@emergela", "N293DVE@emergela", "N293OCN@emergela",
        "N293DVNI@emergela", "N293VAC@emergela"),
  B = c("N293PC@emergela", "N293RTAR@emergela", "N293XUSV@emergela", "N293FCP@emergela",
        "N293LFR@emergela", "N293PFTT@emergela"),
  C = c("N293IXD@emergela", "N293IMD@emergela", "N293PFTT@emergela", "N293ILJN@emergela",
        "N293ACTR@emergela", "N293VAC@emergela"),
  D = c("N293ELUR@emergela", "N293ELE@emergela", "N293ELLF@emergela", "N293EWMR@emergela",
        "N293EM@emergela", "N293GVQ@emergela"),
  E = c("N293PC@emergela", "N293PWMF@emergela", "N293EWMR@emergela", "N293NCPC@emergela",
        "N293ACTR@emergela", "N293ELUR@emergela", "N293EM@emergela")
)

# ------------------------------------------------------------
# 1. Data
# ------------------------------------------------------------
hv_map  <- read_csv(file.path(project_root, "1_data", "haver_series_map.csv"),
                    show_col_types = FALSE) %>% filter(keep)
yoy     <- read_csv(file.path(processed_path, "haver_yoy_q.csv"), show_col_types = FALSE) %>%
  mutate(date = as.Date(date))
targets <- read_csv(file.path(processed_path, "sector_targets.csv"), show_col_types = FALSE)
wdi     <- read_csv(file.path(processed_path, "wdi_all_long.csv"), show_col_types = FALSE)
stopifnot(all(hv_map$code %in% names(yoy)), nrow(targets) == 50,
          all(unlist(CORE) %in% hv_map$code))

# Publication lags BEFORE any YTD averaging (leakage check 3)
hv_lagged    <- apply_pub_lags(yoy, setNames(hv_map$pub_lag_q, hv_map$code))
targets_wide <- build_target_wide(wdi, targets)

yr_of  <- as.integer(format(hv_lagged$date, "%Y"))
in_win <- yr_of %in% SEL_YEARS
cand_years <- vapply(hv_map$code, function(cd)
  length(unique(yr_of[in_win & !is.na(hv_lagged[[cd]])])), integer(1))
stopifnot(all(cand_years[unlist(CORE)] >= MIN_CAND_YEARS))

sector_pool <- function(s) {
  hv_map$code[vapply(strsplit(hv_map$sector, ";"), function(x) s %in% x, logical(1)) &
                cand_years[hv_map$code] >= MIN_CAND_YEARS]
}
safe_names <- function(x) gsub("[^A-Za-z0-9_]", "_", x)

# Leader clustering of near-duplicates on quarterly YoY, 2000-2019
cluster_pool <- function(pool) {
  Q  <- as.matrix(hv_lagged[in_win, pool, drop = FALSE])
  cm <- suppressWarnings(cor(Q, use = "pairwise.complete.obs"))
  nm <- crossprod(!is.na(Q))
  ord <- pool[order(-cand_years[pool], pool)]
  leader <- setNames(rep(NA_character_, length(pool)), pool)
  leaders <- character(0)
  for (cd in ord) {
    if (length(leaders)) {
      r <- abs(cm[cd, leaders]); ok <- nm[cd, leaders] >= CLUSTER_MIN_N & !is.na(r)
      hit <- which(ok & r > CLUSTER_R)
      if (length(hit)) { leader[cd] <- leaders[hit[which.max(r[hit])]]; next }
    }
    leader[cd] <- cd; leaders <- c(leaders, cd)
  }
  leader
}

# ------------------------------------------------------------
# 2. Per-sector selection
# ------------------------------------------------------------
run_sector <- function(s) {
  ck <- file.path(ckpt_dir, sprintf("rhs_%s_%s.rds", CKPT_TAG, s))
  if (file.exists(ck)) { message("Sector ", s, ": loaded checkpoint"); return(readRDS(ck)) }

  pool  <- sector_pool(s)
  tg    <- targets$code[targets$sector == s]
  lead  <- cluster_pool(pool)
  feats <- build_bridge_features(hv_lagged, pool, targets_wide, target_cols = tg,
                                 prior_cols = tg, years = SEL_YEARS)
  stopifnot(nrow(feats$pairs) > 0, max(feats$pairs$target_year) < 2020)   # leakage check 1
  message(sprintf("Sector %s: %d candidates in %d clusters, %d rows (%d-%d)", s, length(pool),
                  length(unique(lead)), nrow(feats$pairs),
                  min(feats$pairs$target_year), max(feats$pairs$target_year)))

  ytd_cols <- paste0("ytd_", pool)
  clusters <- unique(lead)
  freq_rows <- list(); step_rows <- list(); robust_rows <- list()

  for (t in tg) {
    y  <- feats$Y[, t]; ok <- !is.na(y)
    X  <- cbind(feats$X[ok, ytd_cols, drop = FALSE],
                prior_own = feats$X[ok, paste0("prior_", t)],
                quarter   = feats$X[ok, "quarter"])
    y  <- y[ok]; yrs <- feats$pairs$target_year[ok]; qtr <- feats$X[ok, "quarter"]
    st <- col_stats(X); Xs <- standardise_with(X, st$means, st$sds)
    pf <- c(rep(1, length(ytd_cols)), 0, 0)
    uy <- sort(unique(yrs))

    # -- LASSO stability selection
    hit_m <- setNames(numeric(length(pool)), pool)
    hit_c <- setNames(numeric(length(clusters)), clusters)
    n_ok <- 0
    for (b in seq_len(N_DRAWS)) {
      sy  <- sample(uy, floor(SUBSAMPLE_FRAC * length(uy)))
      rws <- yrs %in% sy
      fold_of_year <- setNames(sample(rep_len(seq_len(N_FOLDS), length(sy))), sy)
      fit <- tryCatch(cv.glmnet(Xs[rws, , drop = FALSE], y[rws], alpha = 1,
                                foldid = unname(fold_of_year[as.character(yrs[rws])]),
                                penalty.factor = pf, standardize = FALSE),
                      error = function(e) NULL)
      if (is.null(fit)) next
      nz <- as.matrix(coef(fit, s = "lambda.1se"))[ytd_cols, 1] != 0
      hit_m <- hit_m + nz
      hit_c <- hit_c + (clusters %in% unique(lead[pool[nz]]))
      n_ok <- n_ok + 1
    }
    fm <- if (n_ok) hit_m / n_ok else hit_m
    fc <- if (n_ok) hit_c / n_ok else hit_c
    freq_rows[[t]] <- tibble(sector = s, target = t, code = pool, cluster = unname(lead[pool]),
                             freq = as.numeric(fm), cluster_freq = as.numeric(fc[lead[pool]]),
                             n_draws = n_ok, n_years = length(uy))

    # -- Forward stepwise, BIC, one row per year (Q4 cutoff), cluster leaders only
    q4 <- qtr == 4
    lead_cols <- paste0("ytd_", clusters)
    df <- as.data.frame(Xs[q4, c(lead_cols, "prior_own"), drop = FALSE])
    names(df) <- safe_names(names(df)); df$y <- y[q4]
    lower <- y ~ prior_own
    upper <- as.formula(paste("y ~", paste(setdiff(names(df), "y"), collapse = " + ")))
    sfit <- tryCatch(stepAIC(lm(lower, data = df), scope = list(lower = lower, upper = upper),
                             direction = "forward", steps = STEP_MAX, trace = 0,
                             k = log(nrow(df))),
                     error = function(e) NULL)
    picked <- if (is.null(sfit)) character(0) else
      setdiff(attr(terms(sfit), "term.labels"), "prior_own")
    picked_cl <- clusters[match(picked, safe_names(lead_cols))]
    step_rows[[t]] <- tibble(sector = s, target = t, order = seq_along(picked_cl),
                             cluster = picked_cl)

    lasso_cl <- clusters[fc >= PICK_FREQ]
    un <- union(lasso_cl, picked_cl)
    robust_rows[[t]] <- tibble(sector = s, target = t, n_years = length(uy),
                               n_lasso = length(lasso_cl), n_stepwise = length(picked_cl),
                               n_overlap = length(intersect(lasso_cl, picked_cl)),
                               jaccard = if (length(un)) length(intersect(lasso_cl, picked_cl)) /
                                 length(un) else NA_real_)
    message(sprintf("  %-22s years=%2d  lasso clusters(freq>=%.1f)=%2d  stepwise=%d  overlap=%d",
                    t, length(uy), PICK_FREQ, length(lasso_cl), length(picked_cl),
                    robust_rows[[t]]$n_overlap))
  }
  res <- list(freq = bind_rows(freq_rows), step = bind_rows(step_rows),
              robust = bind_rows(robust_rows), pool = pool,
              clusters = tibble(sector = s, code = pool, cluster = unname(lead[pool])))
  saveRDS(res, ck)
  res
}

set.seed(SEED)
res <- lapply(setNames(LETTERS[1:5], LETTERS[1:5]), run_sector)
freq_all    <- bind_rows(lapply(res, `[[`, "freq"))
step_all    <- bind_rows(lapply(res, `[[`, "step"))
robust_all  <- bind_rows(lapply(res, `[[`, "robust"))
cluster_all <- bind_rows(lapply(res, `[[`, "clusters"))

# ------------------------------------------------------------
# 3. Per-target picks -> sector sets
# ------------------------------------------------------------
# cluster-level frequency per target, represented by its most-selected member
cl_freq <- freq_all %>%
  group_by(sector, target, cluster) %>%
  arrange(desc(freq), .by_group = TRUE) %>%
  summarise(cluster_freq = first(cluster_freq), rep_code = first(code),
            rep_freq = first(freq), .groups = "drop")

target_picks <- cl_freq %>%
  filter(cluster_freq >= PICK_FREQ) %>%
  group_by(sector, target) %>%
  arrange(desc(cluster_freq), desc(rep_freq), .by_group = TRUE) %>%
  slice_head(n = TOP_PER_TARGET) %>%
  mutate(pick_rank = row_number()) %>% ungroup()

# Representative of a cluster within a sector = member with highest total freq
sector_rep <- freq_all %>%
  group_by(sector, cluster, code) %>% summarise(tot = sum(freq), .groups = "drop") %>%
  group_by(sector, cluster) %>% slice_max(tot, n = 1, with_ties = FALSE) %>% ungroup() %>%
  select(sector, cluster, rep_code = code)

lasso_sets <- target_picks %>%
  group_by(sector, cluster) %>%
  summarise(n_targets = n(), max_freq = max(cluster_freq),
            targets = paste(target, collapse = "; "), .groups = "drop") %>%
  left_join(sector_rep, by = c("sector", "cluster")) %>%
  group_by(sector) %>%
  arrange(desc(n_targets), desc(max_freq), .by_group = TRUE) %>%
  ungroup() %>%
  transmute(sector, code = rep_code, cluster, source = "lasso", n_targets, max_freq, targets)

# Final de-duplication of each sector set: drop a pick whose quarterly YoY
# correlates |r| > SET_MAX_R with a higher-ranked pick (e.g. EOP vs AVG of the
# same exchange rate, which can fall just below CLUSTER_R)
dedupe_set <- function(df) {
  if (nrow(df) < 2) return(df)
  Q  <- as.matrix(hv_lagged[in_win, df$code, drop = FALSE])
  cm <- suppressWarnings(cor(Q, use = "pairwise.complete.obs"))
  keep <- logical(nrow(df))
  for (i in seq_len(nrow(df))) {
    prev <- which(keep)
    keep[i] <- !length(prev) || !any(abs(cm[i, prev]) > SET_MAX_R, na.rm = TRUE)
  }
  df[keep, ]
}
lasso_sets <- lasso_sets %>% group_by(sector) %>% group_modify(~ dedupe_set(.x)) %>%
  slice_head(n = MAX_RHS) %>% ungroup()

fill_core <- function(s) {
  cur <- lasso_sets %>% filter(sector == s)
  if (nrow(cur) >= MIN_RHS) return(cur)
  cl_of <- cluster_all %>% filter(sector == s)
  have_cl <- cur$cluster
  add <- character(0)
  for (cd in CORE[[s]]) {
    if (nrow(cur) + length(add) >= CORE_FILL_TO) break
    cl <- cl_of$cluster[match(cd, cl_of$code)]          # NA if not in this sector's pool
    if (!is.na(cl) && cl %in% have_cl) next
    add <- c(add, cd); have_cl <- c(have_cl, if (is.na(cl)) cd else cl)
  }
  bind_rows(cur, tibble(sector = s, code = add,
                        cluster = coalesce(cl_of$cluster[match(add, cl_of$code)], add),
                        source = "core", n_targets = 0L, max_freq = NA_real_, targets = ""))
}
sets <- bind_rows(lapply(LETTERS[1:5], fill_core)) %>%
  group_by(sector) %>% mutate(rank = row_number()) %>% ungroup() %>%
  left_join(hv_map %>% select(code, short_name, desc, pub_lag_q, transform), by = "code")
pooled <- sets %>% distinct(code, .keep_all = TRUE) %>%
  transmute(sector = "Pooled", code, short_name, desc, pub_lag_q, transform,
            source = "union", rank = row_number())
sets_out <- bind_rows(sets, pooled)
stopifnot(all(table(sets$sector) >= MIN_RHS), all(table(sets$sector) <= MAX_RHS))

# Sector x cluster summary over all candidates
summary_tbl <- cl_freq %>%
  group_by(sector, cluster) %>%
  summarise(avg_freq = mean(cluster_freq), max_freq = max(cluster_freq),
            n_targets_pick = sum(cluster_freq >= PICK_FREQ),
            top_target = target[which.max(cluster_freq)], .groups = "drop") %>%
  left_join(sector_rep, by = c("sector", "cluster")) %>%
  left_join(cluster_all %>% count(sector, cluster, name = "n_members"), by = c("sector", "cluster")) %>%
  left_join(hv_map %>% select(rep_code = code, short_name), by = "rep_code") %>%
  mutate(selected = paste(sector, rep_code) %in% paste(sets$sector, sets$code)) %>%
  arrange(sector, desc(max_freq))

write_csv(sets_out, file.path(out_dir, "rhs_sector_sets.csv"))
write_csv(target_picks %>% left_join(hv_map %>% select(rep_code = code, short_name), by = "rep_code"),
          file.path(out_dir, "rhs_target_picks.csv"))
write_csv(summary_tbl, file.path(out_dir, "rhs_selection.csv"))
write_csv(freq_all %>% filter(freq > 0 | cluster_freq > 0) %>%
            left_join(hv_map %>% select(code, short_name), by = "code") %>%
            arrange(sector, target, desc(cluster_freq), desc(freq)),
          file.path(out_dir, "rhs_selection_freq.csv"))
write_csv(cluster_all %>% left_join(hv_map %>% select(code, short_name), by = "code") %>%
            arrange(sector, cluster), file.path(out_dir, "rhs_clusters.csv"))
write_csv(step_all %>% left_join(hv_map %>% select(cluster = code, short_name), by = "cluster"),
          file.path(out_dir, "rhs_stepwise.csv"))
write_csv(robust_all, file.path(out_dir, "rhs_robustness.csv"))

# ------------------------------------------------------------
# 4. Heatmap: selected predictors x targets (cluster frequency)
# ------------------------------------------------------------
SECTOR_LABELS <- c(A = "A  Macro", B = "B  Financial", C = "C  Trade & external",
                   D = "D  Labor", E = "E  Poverty")
tlabel <- targets %>% transmute(target = code, tlab = str_trunc(name, 38))
grid <- sets %>% select(sector, code, cluster, rank, source, short_name) %>%
  inner_join(targets %>% select(sector, target = code), by = "sector", relationship = "many-to-many") %>%
  left_join(cl_freq %>% select(sector, target, cluster, cluster_freq),
            by = c("sector", "target", "cluster")) %>%
  mutate(cluster_freq = coalesce(cluster_freq, 0)) %>%
  left_join(tlabel, by = "target") %>%
  mutate(pred = paste0(str_trunc(short_name, 34), ifelse(source == "core", " (core)", ""),
                       " [", rank, "]"),
         sector = SECTOR_LABELS[sector]) %>%
  mutate(pred = reorder(pred, -rank))

ramp <- c("#cde2fb", "#9ec5f4", "#6da7ec", "#3987e5", "#256abf", "#184f95", "#0d366b")
p <- ggplot(grid, aes(x = tlab, y = pred, fill = cluster_freq)) +
  geom_tile(colour = "#fcfcfb", linewidth = 0.6) +
  scale_fill_gradientn(colours = ramp, limits = c(0, 1), name = "LASSO selection\nfrequency") +
  facet_wrap(~ sector, scales = "free", ncol = 2) +
  labs(x = NULL, y = NULL,
       title = "Haver predictors selected for each sector",
       subtitle = sprintf(paste0("Share of %d year-subsamples in which LASSO (lambda.1se) kept the predictor ",
                                 "or a near-duplicate (|r| > %.2f); target years 2000-2019. ",
                                 "(core) = economic fallback. [n] = rank."), N_DRAWS, CLUSTER_R)) +
  theme_minimal(base_size = 9) +
  theme(plot.background = element_rect(fill = "#fcfcfb", colour = NA),
        panel.grid      = element_blank(),
        axis.text.x     = element_text(angle = 40, hjust = 1, colour = "#52514e"),
        axis.text.y     = element_text(colour = "#52514e"),
        strip.text      = element_text(face = "bold", hjust = 0, colour = "#1a1a19"),
        plot.title      = element_text(face = "bold", colour = "#1a1a19"),
        plot.subtitle   = element_text(colour = "#52514e"),
        legend.title    = element_text(colour = "#52514e"))
ggsave(file.path(out_dir, "rhs_selection_heatmap.png"), p, width = 14, height = 12, dpi = 150,
       bg = "#fcfcfb")

# ------------------------------------------------------------
# 5. CP3 summary
# ------------------------------------------------------------
options(width = 220)
message("\n==== Sector predictor sets ====")
print(sets %>% mutate(short_name = str_trunc(short_name, 42), max_freq = round(max_freq, 2)) %>%
        select(sector, rank, short_name, source, n_targets, max_freq, targets), n = Inf)
message("Pooled input set: ", nrow(pooled), " predictors")
message("\n==== LASSO vs stepwise (BIC, one row per year) ====")
print(robust_all %>% group_by(sector) %>%
        summarise(targets_with_lasso_pick = sum(n_lasso > 0),
                  median_jaccard = median(jaccard, na.rm = TRUE),
                  mean_lasso = mean(n_lasso), mean_stepwise = mean(n_stepwise)))
