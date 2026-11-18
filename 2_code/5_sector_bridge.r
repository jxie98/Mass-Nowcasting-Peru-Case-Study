# ============================================================
# 5_sector_bridge.r — LASSO and OLS bridge equations (plan Step 6)
# ============================================================
# Inputs : same as 4_sector_lstm.r (Haver YoY, map, targets, WDI, sector sets)
# Outputs: 3_outputs/bridge/
#   val_predictions.csv  walk-forward 2020-2025, long: model (lasso / ols),
#                        target, cutoff, target_year, actual, pred
#   nowcast.csv          predictions for target years without an actual yet
#   lasso_coefficients.csv  final-year LASSO coefficients per target
#
# Design (same information set as the LSTMs, one equation per target)
#   * Features: YTD mean of each predictor in the target's SECTOR set at the
#     quarterly cutoff (lags applied first), the target's own prior-year
#     value, cutoff quarter, COVID shock and rebound dummies.
#   * Sample: target years 2000 onward, walk-forward from 2020 (train on
#     years < Y, standardise with training statistics only).
#   * LASSO: cv.glmnet, lambda.min (as in the Macro bridge).
#   * OLS: plain least squares on the same features; columns constant in the
#     training window (e.g. COVID dummies before 2020) are dropped.
#   * Nowcasts: the walk-forward prediction for target years after the last
#     actual (model trained on all earlier years).
#   Caveat for the blog: the COVID dummies are 0 throughout training when
#   predicting 2020, so the bridge cannot anticipate the 2020 shock.
# ============================================================

script_dir <- local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a))) else
    tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) "2_code")
})
source(file.path(script_dir, "0_functions.R"))
suppressPackageStartupMessages(library(glmnet))

project_root   <- get_project_root()
processed_path <- file.path(project_root, "1_data", "processed")
out_dir        <- file.path(project_root, "3_outputs", "bridge")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

TRAIN_START   <- 2000L
VAL_START     <- 2020L
NOWCAST_YEARS <- 2025:2026
SEED          <- 42L

hv_map  <- read_csv(file.path(project_root, "1_data", "haver_series_map.csv"),
                    show_col_types = FALSE) %>% filter(keep)
yoy     <- read_csv(file.path(processed_path, "haver_yoy_q.csv"), show_col_types = FALSE) %>%
  mutate(date = as.Date(date))
targets <- read_csv(file.path(processed_path, "sector_targets.csv"), show_col_types = FALSE)
wdi     <- read_csv(file.path(processed_path, "wdi_all_long.csv"), show_col_types = FALSE)
sets    <- read_csv(file.path(project_root, "3_outputs", "sector_lstm", "rhs_sector_sets.csv"),
                    show_col_types = FALSE)

hv_lagged    <- apply_pub_lags(yoy, setNames(hv_map$pub_lag_q, hv_map$code))
targets_wide <- build_target_wide(wdi, targets)
last_actual  <- vapply(targets$code, function(t) max(targets_wide$year[!is.na(targets_wide[[t]])]),
                       numeric(1))

# Walk-forward OLS on standardised features (training-window statistics only)
run_walk_forward_ols <- function(X, y, pairs, val_start) {
  val_idx <- which(pairs$target_year >= val_start)
  pred <- rep(NA_real_, length(val_idx))
  for (yr in sort(unique(pairs$target_year[val_idx]))) {
    tr <- which(pairs$target_year < yr & !is.na(y)); te <- which(pairs$target_year == yr)
    if (length(tr) < 5L) next
    st  <- col_stats(X[tr, , drop = FALSE])
    vary <- apply(X[tr, , drop = FALSE], 2, function(v) sd(v, na.rm = TRUE) > 0)
    vary[is.na(vary)] <- FALSE
    Xtr <- standardise_with(X[tr, vary, drop = FALSE], st$means[vary], st$sds[vary])
    Xte <- standardise_with(X[te, vary, drop = FALSE], st$means[vary], st$sds[vary])
    fit <- lm.fit(cbind(1, Xtr), y[tr])
    b <- fit$coefficients; b[is.na(b)] <- 0
    pred[match(te, val_idx)] <- as.vector(cbind(1, Xte) %*% b)
  }
  pred
}

val_rows <- list(); coef_rows <- list()
for (i in seq_len(nrow(targets))) {
  t <- targets$code[i]; s <- targets$sector[i]
  inputs <- sets$code[sets$sector == s]
  f <- build_bridge_features(hv_lagged, inputs, targets_wide, target_cols = t, prior_cols = t,
                             years = TRAIN_START:2100, extra_years = NOWCAST_YEARS)
  stopifnot(min(f$pairs$target_year) >= TRAIN_START,
            ncol(f$X) == length(inputs) + 4L)           # YTD inputs + prior + quarter + 2 COVID
  lasso <- run_walk_forward_bridge(f$X, f$Y, f$pairs, val_start = VAL_START,
                                   lambda = "lambda.min", seed = SEED)
  ols   <- run_walk_forward_ols(f$X, f$Y[, 1], f$pairs, VAL_START)
  vp <- lasso$val_pairs
  base <- tibble(sector = s, target = t, cutoff = vp$cutoff, target_year = vp$target_year,
                 actual = f$Y[lasso$val_idx, 1])
  val_rows[[t]] <- bind_rows(base %>% mutate(model = "lasso", pred = lasso$pred[, 1]),
                             base %>% mutate(model = "ols",   pred = ols))
  if (nrow(lasso$coefs)) coef_rows[[t]] <- lasso$coefs %>% mutate(sector = s, .before = 1)
}
all_pred <- bind_rows(val_rows)

val <- all_pred %>% filter(!is.na(actual))
now <- all_pred %>% filter(target_year > last_actual[target]) %>% select(-actual)
coefs <- bind_rows(coef_rows) %>%
  left_join(hv_map %>% transmute(predictor = paste0("ytd_", code), short_name), by = "predictor")

write_csv(val,   file.path(out_dir, "val_predictions.csv"))
write_csv(now,   file.path(out_dir, "nowcast.csv"))
write_csv(coefs, file.path(out_dir, "lasso_coefficients.csv"))

# Quick look: RMSE relative to random walk, headline window 2022+
rw <- val %>% distinct(target, target_year) %>%
  mutate(rw = mapply(function(t, y) targets_wide[[t]][match(y - 1L, targets_wide$year)],
                     target, target_year))
smry <- val %>% left_join(rw, by = c("target", "target_year")) %>%
  filter(target_year >= 2022) %>%
  group_by(sector, model, target) %>%
  summarise(rel_rmse = sqrt(mean((pred - actual)^2, na.rm = TRUE)) /
              sqrt(mean((rw - actual)^2, na.rm = TRUE)), .groups = "drop") %>%
  group_by(sector, model) %>%
  summarise(median_rel_rmse = median(rel_rmse, na.rm = TRUE),
            share_beat_rw = mean(rel_rmse < 1, na.rm = TRUE), .groups = "drop")
options(width = 160)
message("\n==== Bridge vs random walk, 2022-2025 (relative RMSE < 1 = beats RW) ====")
print(smry, n = Inf)
