# ============================================================
# 0_functions_check.r — do the generic functions in 0_functions.R
# reproduce the Macro package results?
# ============================================================
# Reads (never writes) Coding Package (Macro):
#   1_data/processed/haver_df.csv, wdi_df.csv
#   3_outputs/bridge_results (all years)/val_predictions.csv   (3b bridge)
#   3_outputs/comparison_bridge_vs_lstm (all years)/rmse_comparison.csv
# Checks
#   1. Bridge walk-forward predictions identical (seed 42, same call order)
#   2. Random walk + three-window RMSE identical to the comparison script
#   3. (only with --lstm, ~30 min) LSTM in the Macro configuration (36-month
#      window, all priors in the sequence, global input normalisation, Macro
#      24-config CV grid incl. COVID years, seed 42): walk-forward RMSE within
#      LSTM_TOL of Macro 3_outputs/CV_results/val_predictions.csv. Macro set
#      no seeds and did not save its chosen hyperparameters, so only an
#      approximate match is possible.
# Writes: 3_outputs/regression_check/bridge_check.csv, lstm_check.csv,
#         lstm_cv_scores.csv (Sector package)
# Usage : Rscript 2_code/0_functions_check.r [--lstm]
# ============================================================

script_dir <- local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a))) else
    tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) "2_code")
})
source(file.path(script_dir, "0_functions.R"))
suppressPackageStartupMessages(library(glmnet))

project_root <- get_project_root()
macro_root   <- file.path(dirname(project_root), "Coding Package (Macro)")
out_dir      <- file.path(project_root, "3_outputs", "regression_check")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

# ---- Macro configuration (copied from 3b_bridge_all_years.r) ----------
INPUT_COLS  <- c("manuf_ipi", "biz_confidence", "exports_nom", "imports_nom",
                 "int_reserves", "cpi", "exchange_rate", "copper_price",
                 "cement_prod", "imports_real", "gva_agri", "retail_sales")
TARGET_COLS <- c("gdp", "consu_govt", "invest", "exports", "imports",
                 "consu_total", "gnp", "reserves_months")
pub_lag <- c(manuf_ipi = 2, biz_confidence = 1, exports_nom = 2, imports_nom = 2,
             int_reserves = 1, cpi = 1, exchange_rate = 1, copper_price = 1,
             cement_prod = 2, imports_real = 2, gva_agri = 3, retail_sales = 2)

haver <- read_csv(file.path(macro_root, "1_data/processed/haver_df.csv"),
                  show_col_types = FALSE) %>% mutate(date = as.Date(date))
wdi   <- read_csv(file.path(macro_root, "1_data/processed/wdi_df.csv"),
                  show_col_types = FALSE) %>% rename(year = date) %>% arrange(year)
# Macro uses simple % growth (x / lag(x) - 1) * 100 for these targets
wdi_growth <- wdi %>% mutate(across(all_of(TARGET_COLS), ~ (.x / lag(.x) - 1) * 100))

# ---- 1. Bridge -------------------------------------------------------
hv_lagged <- apply_pub_lags(haver, pub_lag)
feats <- build_bridge_features(hv_lagged, INPUT_COLS, wdi_growth, TARGET_COLS)
stopifnot(ncol(feats$X) == 23)
wf <- run_walk_forward_bridge(feats$X, feats$Y, feats$pairs, val_start = 2020L,
                              lambda = "lambda.min", seed = 42)

ref <- read.csv(file.path(macro_root, "3_outputs/bridge_results (all years)/val_predictions.csv"))
ref$cutoff <- as.Date(ref$cutoff)
new <- cbind(wf$val_pairs, setNames(as.data.frame(wf$pred), paste0("pred_", TARGET_COLS)))
key_new <- paste(new$cutoff, new$target_year); key_ref <- paste(ref$cutoff, ref$target_year)
stopifnot(setequal(key_new, key_ref))
ref <- ref[match(key_new, key_ref), ]

cmp <- bind_rows(lapply(TARGET_COLS, function(t) tibble(
  target = t,
  max_abs_diff_pred   = max(abs(new[[paste0("pred_", t)]] - ref[[paste0("pred_", t)]]), na.rm = TRUE),
  max_abs_diff_actual = max(abs(feats$Y[wf$val_idx, t] - ref[[paste0("actual_", t)]]), na.rm = TRUE))))
write_csv(cmp, file.path(out_dir, "bridge_check.csv"))
print(cmp)
bridge_ok <- all(cmp$max_abs_diff_pred < 1e-8 & cmp$max_abs_diff_actual < 1e-8)

# ---- 2. Random walk + three-window RMSE ------------------------------
rw <- random_walk_pred(wf$val_pairs, wdi_growth, TARGET_COLS)
actual <- feats$Y[wf$val_idx, , drop = FALSE]
to_long <- function(pred_df, model) {
  bind_rows(lapply(TARGET_COLS, function(t) tibble(
    target_year = pred_df$target_year, target = t, model = model,
    error = pred_df[[paste0("pred_", t)]] - actual[, t])))
}
rm_new <- eval_three_windows(bind_rows(to_long(new, "LASSO Bridge"), to_long(rw, "Random walk")))
ref_rm <- read.csv(file.path(macro_root,
                             "3_outputs/comparison_bridge_vs_lstm (all years)/rmse_comparison.csv"))
chk <- rm_new %>%
  select(window, target, model, rmse) %>%
  pivot_wider(names_from = model, values_from = rmse) %>%
  mutate(window = as.character(window)) %>%
  inner_join(ref_rm %>% select(window, target, bridge_rmse, rw_rmse), by = c("window", "target"))
stopifnot(nrow(chk) == 3 * length(TARGET_COLS))
# reference CSV is rounded to 4 dp
eval_ok <- all(abs(round(chk$`LASSO Bridge`, 4) - chk$bridge_rmse) < 1e-4 &
               abs(round(chk$`Random walk`, 4) - chk$rw_rmse) < 1e-4)

message("\nBridge predictions identical to Macro 3b : ", bridge_ok)
message("Three-window RMSE identical to Macro 4   : ", eval_ok)
stopifnot(bridge_ok, eval_ok)

# ---- 3. LSTM (optional: --lstm) -------------------------------------
if ("--lstm" %in% commandArgs(trailingOnly = TRUE)) {
  LSTM_TOL <- 0.10   # mean RMSE across targets within +/-10% of Macro
  setup_keras()
  seqs <- build_lstm_sequences(hv_lagged, INPUT_COLS, wdi_growth, TARGET_COLS,
                               window = 36L, min_obs = 12L, min_share = 1)
  message(sprintf("LSTM sequences: [%s], %d-%d", paste(dim(seqs$X), collapse = " x "),
                  min(seqs$pairs$target_year), max(seqs$pairs$target_year)))
  grid <- expand.grid(lstm_units = c(32L, 64L, 128L), dense_units = c(16L, 32L),
                      dropout = c(0.1, 0.3), lr = c(1e-3, 5e-4), stringsAsFactors = FALSE)
  cv_years <- c(seqs$pairs$target_year[seqs$pairs$target_year < 2020], 2020L, 2021L)
  cv <- run_lstm_cv(seqs, hv_lagged, grid, cv_years, prior_mode = "sequence_all",
                    norm = "global", targets_wide = wdi_growth, seed = 42L)
  write_csv(cv$scores, file.path(out_dir, "lstm_cv_scores.csv"))
  message("Best config: ", paste(names(cv$best), cv$best, sep = "=", collapse = ", "))
  wfl <- run_walk_forward_lstm(seqs, hv_lagged, cv$best, val_start = 2020L, seeds = 42L,
                               prior_mode = "sequence_all", norm = "global",
                               targets_wide = wdi_growth)

  ref_l <- read.csv(file.path(macro_root, "3_outputs/CV_results/val_predictions.csv"))
  ref_l$cutoff <- as.Date(ref_l$cutoff)
  key_l <- paste(wfl$val_pairs$cutoff, wfl$val_pairs$target_year)
  ref_l <- ref_l[match(key_l, paste(ref_l$cutoff, ref_l$target_year)), ]
  stopifnot(!anyNA(ref_l$target_year))
  act <- seqs$Y[wfl$val_idx, , drop = FALSE]
  lstm_cmp <- bind_rows(lapply(TARGET_COLS, function(t) tibble(
    target     = t,
    rmse_new   = sqrt(mean((wfl$pred[, t] - act[, t])^2, na.rm = TRUE)),
    rmse_macro = sqrt(mean((ref_l[[paste0("pred_", t)]] - act[, t])^2, na.rm = TRUE))))) %>%
    mutate(ratio = rmse_new / rmse_macro)
  write_csv(lstm_cmp, file.path(out_dir, "lstm_check.csv"))
  print(lstm_cmp)
  mean_ratio <- mean(lstm_cmp$rmse_new) / mean(lstm_cmp$rmse_macro)
  lstm_ok <- abs(mean_ratio - 1) <= LSTM_TOL
  message(sprintf("LSTM mean RMSE ratio new/Macro = %.3f (tolerance +/-%.0f%%): %s",
                  mean_ratio, 100 * LSTM_TOL, lstm_ok))
}
