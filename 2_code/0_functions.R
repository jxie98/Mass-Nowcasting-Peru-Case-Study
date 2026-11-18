# ============================================================
# 0_functions.R — shared function library (Sector package)
# ============================================================
# Sourced by every numbered script. Contains no global state.
#
# Section 1: project paths
# Section 2: Haver quarterly helpers (parse, transform)
# Section 3: annual WDI target transforms
# Section 4: bridge (YTD features + walk-forward LASSO), generalised from
#            Macro 3b_bridge_all_years.r — reproduces it exactly
# Section 5: evaluation (random walk, three windows), from Macro
#            4_compare_bridge_lstm.r
# Section 6: (to come) LSTM functions generalised from Macro
#            2_multi-task_LSTM.r
# ============================================================

suppressPackageStartupMessages({
  library(readxl)
  library(dplyr)
  library(tidyr)
  library(stringr)
  library(readr)
})

# ------------------------------------------------------------
# 1. Project root — works under RStudio, Rscript and source()
# ------------------------------------------------------------
# Root = parent of the 2_code/ folder that holds the calling script.
get_project_root <- function() {
  # (a) Rscript path/to/script.r
  file_arg <- grep("^--file=", commandArgs(trailingOnly = FALSE), value = TRUE)
  if (length(file_arg) == 1) {
    p <- normalizePath(sub("^--file=", "", file_arg), winslash = "/")
    return(dirname(dirname(p)))
  }
  # (b) source("path/to/script.r")
  for (i in rev(seq_len(sys.nframe()))) {
    f <- sys.frame(i)$ofile
    if (!is.null(f)) return(dirname(dirname(normalizePath(f, winslash = "/"))))
  }
  # (c) RStudio editor
  p <- tryCatch(rstudioapi::getSourceEditorContext()$path, error = function(e) "")
  if (nzchar(p)) return(dirname(dirname(normalizePath(p, winslash = "/"))))
  # (d) working directory is the project root or 2_code/
  wd <- normalizePath(getwd(), winslash = "/")
  if (basename(wd) == "2_code") return(dirname(wd))
  if (dir.exists(file.path(wd, "2_code"))) return(wd)
  stop("Cannot locate project root; run from the project folder or 2_code/.")
}

# ------------------------------------------------------------
# 2a. Read a Haver DLX export (one sheet, series in rows)
# ------------------------------------------------------------
# Layout: row 1 = header (col 1 = frequency tag e.g. "19801 *Q",
# metadata cols .DESC .T1 ..., then YYYYP period columns);
# row 2 = ".excel_last" (Excel serial dates, discarded).
# Returns list(meta = tibble, values = numeric matrix [series x period],
#              periods = character YYYYP, dates = quarter-end Date).
read_haver_quarterly <- function(path, sheet = "Data") {
  x <- suppressMessages(read_excel(path, sheet = sheet, col_names = FALSE,
                                   .name_repair = "minimal"))
  hdr <- as.character(unlist(x[1, ]))
  hdr[1] <- "code"
  names(x) <- hdr
  x <- x[-(1:2), ]                                   # drop header + .excel_last

  period_cols <- hdr[str_detect(hdr, "^[0-9]{5}$")]  # YYYYQ
  meta_cols   <- setdiff(hdr, period_cols)
  meta <- x[, meta_cols]
  names(meta) <- sub("^[.]", "", tolower(names(meta)))  # .DESC -> desc

  vals <- sapply(x[, period_cols], function(v) suppressWarnings(as.numeric(v)))
  vals <- matrix(vals, nrow = nrow(x), dimnames = list(meta$code, period_cols))

  yr <- as.integer(substr(period_cols, 1, 4))
  q  <- as.integer(substr(period_cols, 5, 5))
  stopifnot(all(q %in% 1:4))
  dates <- as.Date(sprintf("%d-%02d-01", yr, q * 3)) |>
    (\(d) as.Date(format(d + 31, "%Y-%m-01")) - 1)()   # quarter-end

  list(meta = meta, values = vals, periods = period_cols, dates = dates)
}

# ------------------------------------------------------------
# 2b. Per-series coverage statistics
# ------------------------------------------------------------
series_coverage <- function(values, periods) {
  nn <- !is.na(values)
  first_i <- apply(nn, 1, function(r) if (any(r)) min(which(r)) else NA_integer_)
  last_i  <- apply(nn, 1, function(r) if (any(r)) max(which(r)) else NA_integer_)
  gap <- vapply(seq_len(nrow(values)), function(i) {
    if (is.na(first_i[i])) return(NA_real_)
    mean(!nn[i, first_i[i]:last_i[i]])
  }, numeric(1))
  tibble(
    code        = rownames(values),
    n_obs       = rowSums(nn),
    first_q     = periods[first_i],
    last_q      = periods[last_i],
    first_year  = as.integer(substr(periods[first_i], 1, 4)),
    last_year   = as.integer(substr(periods[last_i], 1, 4)),
    gap_share   = gap
  )
}

# ------------------------------------------------------------
# 2c. Stationary YoY transforms on a quarterly vector
# ------------------------------------------------------------
#   yoy_pct         100 * log(x_t / x_{t-4})           (strictly positive levels)
#   yoy_diff        x_t - x_{t-4}                       (rates, ratios, %, diffusion idx)
#   yoy_diff_scaled 100 * (x_t - x_{t-4}) / mean|x_{t-4..t-1}|  (levels that can be <= 0,
#                   e.g. balances / flows)
yoy_transform <- function(x, transform, lag = 4) {
  xl <- dplyr::lag(x, lag)
  switch(transform,
    yoy_pct  = ifelse(x > 0 & xl > 0, 100 * log(x / xl), NA_real_),
    yoy_diff = x - xl,
    yoy_diff_scaled = {
      s <- zoo::rollapply(abs(x), width = lag, FUN = mean, align = "right",
                          fill = NA, na.rm = TRUE)
      s <- dplyr::lag(s, 1)
      ifelse(is.finite(s) & s > 0, 100 * (x - xl) / s, NA_real_)
    },
    stop("Unknown transform: ", transform)
  )
}

# Default transform from Haver metadata + value sign
default_transform <- function(data_type, desc, values_row) {
  diffusion <- str_detect(desc, regex("50\\+|confidence|expectation|diffusion|business conf|bus exp",
                                      ignore_case = TRUE))
  if (data_type %in% c("%", "RATIO") || diffusion) return("yoy_diff")
  if (any(values_row <= 0, na.rm = TRUE))          return("yoy_diff_scaled")
  "yoy_pct"
}

# Readable, unique short names from Haver descriptions
make_short_names <- function(desc) {
  s <- desc |>
    str_remove("^Peru: ") |>
    str_remove("\\s*\\((NSA|SA|EOP|AVG)[^)]*\\)\\s*$") |>
    str_replace_all("Central Government|Central Govt|Cen Govt|Cent Govt", "cg") |>
    str_replace_all("General Government|General Govt|Gen Govt", "gg") |>
    str_replace_all("Domestic|Dom ", "dom ") |>
    str_replace_all("Monetary Aggregates|Mon Agg", "money") |>
    str_replace_all("[^A-Za-z0-9]+", "_") |>
    str_to_lower() |>
    str_remove_all("^_|_$") |>
    str_trunc(40, ellipsis = "")
  make.unique(s, sep = "_")
}

# ============================================================
# 3. Annual WDI target transforms
# ============================================================
# how = "level" (modelled as is), "pct" (100*log growth), "pp" (first
# difference). Differences are taken between consecutive years only; a gap
# in the series gives NA rather than a multi-year change.
transform_annual <- function(year, value, how) {
  o <- order(year); year <- year[o]; value <- value[o]
  consec <- c(FALSE, diff(year) == 1)
  prev   <- c(NA, head(value, -1))
  y <- switch(how,
    level = value,
    pct   = ifelse(consec & value > 0 & prev > 0, 100 * log(value / prev), NA_real_),
    pp    = ifelse(consec, value - prev, NA_real_),
    stop("Unknown target transform: ", how))
  tibble(year = year, y = y)
}

# Wide year x target table of transformed targets.
#   wdi_long: code, year, value;  spec: code, transform
build_target_wide <- function(wdi_long, spec) {
  wdi_long %>%
    filter(code %in% spec$code) %>%
    inner_join(spec %>% select(code, transform), by = "code") %>%
    group_by(code) %>%
    group_modify(~ transform_annual(.x$year, .x$value, .x$transform[1])) %>%
    ungroup() %>%
    pivot_wider(names_from = code, values_from = y) %>%
    arrange(year) %>%
    select(year, all_of(spec$code))
}

# ============================================================
# 4. Bridge: publication lags, YTD features, walk-forward LASSO
# ============================================================

# Shift each named column down by its lag (rows = periods). lag_vec is a
# named numeric vector; columns not named are left unchanged.
apply_pub_lags <- function(df, lag_vec) {
  for (col in names(lag_vec))
    df[[col]] <- dplyr::lag(df[[col]], n = lag_vec[[col]])
  df
}

# Flat feature matrix for every (cutoff, target_year) pair.
#   hv_lagged    : data frame with `date` + input columns, lags ALREADY applied
#   input_cols   : Haver columns to average
#   targets_wide : data frame `year` + target columns (already transformed)
#   target_cols  : columns of targets_wide used as targets
#   prior_cols   : columns of targets_wide used as prior-year features
#   cutoff_months: months that are cutoffs (3, 6, 9, 12)
# Features: ytd_<input> (mean from Jan 1 of target year to cutoff, NaN -> 0),
#           prior_<target> (value in year-1, NA -> 0), quarter, covid_shock,
#           covid_rebound.  Mirrors Macro 3b_bridge_all_years.r STEP 2.
build_bridge_features <- function(hv_lagged, input_cols, targets_wide,
                                  target_cols, prior_cols = target_cols,
                                  cutoff_months = c(3L, 6L, 9L, 12L),
                                  covid_years = c(2020L, 2021L),
                                  rebound_years = c(2021L, 2022L),
                                  years = NULL, extra_years = NULL) {
  stopifnot(all(input_cols %in% names(hv_lagged)),
            all(c(target_cols, prior_cols) %in% names(targets_wide)))
  lookup <- split(targets_wide, targets_wide$year)
  valid_years <- targets_wide$year[rowSums(!is.na(targets_wide[target_cols])) > 0]
  if (!is.null(years)) valid_years <- intersect(valid_years, years)
  valid_years <- sort(unique(c(valid_years, extra_years)))   # e.g. nowcast years

  hv_dates <- as.Date(hv_lagged$date)
  hv_mat   <- as.matrix(hv_lagged[, input_cols, drop = FALSE])
  cut_ok   <- as.integer(format(hv_dates, "%m")) %in% cutoff_months

  pairs <- do.call(rbind, lapply(valid_years, function(yr) {
    cuts <- sort(hv_dates[cut_ok & as.integer(format(hv_dates, "%Y")) == yr])
    if (!length(cuts)) return(NULL)
    data.frame(cutoff = cuts, target_year = yr)
  }))

  feat_names <- c(paste0("ytd_", input_cols), paste0("prior_", prior_cols),
                  "quarter", "covid_shock", "covid_rebound")
  X <- matrix(NA_real_, nrow(pairs), length(feat_names), dimnames = list(NULL, feat_names))
  Y <- matrix(NA_real_, nrow(pairs), length(target_cols), dimnames = list(NULL, target_cols))
  keep <- logical(nrow(pairs))

  for (i in seq_len(nrow(pairs))) {
    cutoff <- pairs$cutoff[i]; yr <- pairs$target_year[i]
    m <- hv_dates >= as.Date(paste0(yr, "-01-01")) & hv_dates <= cutoff
    if (!any(m)) next
    ytd <- colMeans(hv_mat[m, , drop = FALSE], na.rm = TRUE)
    ytd[is.nan(ytd)] <- 0
    pr  <- lookup[[as.character(yr - 1L)]]
    prv <- if (!is.null(pr)) as.numeric(pr[prior_cols]) else rep(0, length(prior_cols))
    prv[is.na(prv)] <- 0
    X[i, ] <- c(ytd, prv,
                ceiling(as.integer(format(cutoff, "%m")) / 3L),
                as.integer(yr %in% covid_years), as.integer(yr %in% rebound_years))
    tr <- lookup[[as.character(yr)]]
    if (!is.null(tr)) Y[i, ] <- as.numeric(tr[target_cols])
    keep[i] <- TRUE
  }
  list(X = X[keep, , drop = FALSE], Y = Y[keep, , drop = FALSE],
       pairs = pairs[keep, , drop = FALSE])
}

# Column statistics and standardisation (NA -> 0, i.e. the training mean)
col_stats <- function(X) {
  mu <- colMeans(X, na.rm = TRUE)
  sd <- apply(X, 2, sd, na.rm = TRUE)
  sd[sd == 0 | is.na(sd)] <- 1
  list(means = mu, sds = sd)
}
standardise_with <- function(X, means, sds) {
  Xs <- sweep(sweep(X, 2, means, "-"), 2, sds, "/")
  Xs[is.na(Xs)] <- 0
  Xs
}

# Walk-forward LASSO bridge (one model per target). For each validation year
# Y: train on target_year < Y (standardised with training stats only), pick
# lambda by cv.glmnet, predict Y. Then refit on all rows for the nowcast.
# Mirrors Macro 3b STEP 3 (same call order -> identical results for a seed).
run_walk_forward_bridge <- function(X, Y, pairs, val_start = 2020L,
                                    alpha = 1, nfolds = 5L,
                                    lambda = "lambda.min", seed = 42,
                                    penalty_factor = NULL, verbose = FALSE) {
  if (!is.null(seed)) set.seed(seed)
  pf <- if (is.null(penalty_factor)) rep(1, ncol(X)) else penalty_factor
  fit_one <- function(Xs, y) {
    ok <- !is.na(y); n <- sum(ok)
    if (n < max(4L, nfolds)) return(NULL)
    nf <- min(nfolds, max(2L, floor(n / 2L)))
    tryCatch(glmnet::cv.glmnet(Xs[ok, , drop = FALSE], y[ok], alpha = alpha,
                               nfolds = nf, standardize = FALSE, penalty.factor = pf),
             error = function(e) NULL)
  }
  val_idx   <- which(pairs$target_year >= val_start)
  val_years <- sort(unique(pairs$target_year[val_idx]))
  pred <- matrix(NA_real_, length(val_idx), ncol(Y), dimnames = list(NULL, colnames(Y)))

  for (yr in val_years) {
    tr <- which(pairs$target_year < yr); te <- which(pairs$target_year == yr)
    if (length(tr) < 5L || !length(te)) next
    st  <- col_stats(X[tr, , drop = FALSE])
    Xtr <- standardise_with(X[tr, , drop = FALSE], st$means, st$sds)
    Xte <- standardise_with(X[te, , drop = FALSE], st$means, st$sds)
    for (k in seq_len(ncol(Y))) {
      fit <- fit_one(Xtr, Y[tr, k])
      if (is.null(fit)) next
      pred[match(te, val_idx), k] <- as.vector(predict(fit, newx = Xte, s = fit[[lambda]]))
    }
    if (verbose) message("bridge walk-forward: ", yr, " done")
  }

  st_all <- col_stats(X)
  Xall   <- standardise_with(X, st_all$means, st_all$sds)
  final  <- setNames(vector("list", ncol(Y)), colnames(Y))
  coefs  <- list()
  for (k in seq_len(ncol(Y))) {
    y <- Y[, k]; if (sum(!is.na(y)) < 5L) next
    fit <- fit_one(Xall, y); if (is.null(fit)) next
    final[[k]] <- fit
    cf <- as.matrix(coef(fit, s = fit[[lambda]]))
    cf <- cf[cf[, 1] != 0, , drop = FALSE]
    if (nrow(cf)) coefs[[colnames(Y)[k]]] <-
      data.frame(target = colnames(Y)[k], predictor = rownames(cf), coef = cf[, 1])
  }
  list(pred = pred, val_idx = val_idx, val_pairs = pairs[val_idx, , drop = FALSE],
       final_models = final, coefs = bind_rows(coefs), stats_all = st_all,
       lambda = lambda)
}

# ============================================================
# 5. Evaluation
# ============================================================

# Random walk: predict the target's value in year-1 for each (cutoff, year)
random_walk_pred <- function(pairs, targets_wide, target_cols) {
  rw <- pairs[, c("cutoff", "target_year")]
  for (t in target_cols)
    rw[[paste0("pred_", t)]] <- targets_wide[[t]][match(rw$target_year - 1L, targets_wide$year)]
  rw
}

EVAL_WINDOWS <- list(
  "All years (2020-2025)"             = 2020:2100,
  "Excl. COVID (2022-2025)"           = 2022:2100,
  "Excl. COVID + rebound (2023-2025)" = 2023:2100
)

# long: target_year, target, model, error -> RMSE by window x target x model
eval_three_windows <- function(long, windows = EVAL_WINDOWS) {
  bind_rows(lapply(names(windows), function(w) {
    long %>%
      filter(target_year %in% windows[[w]], !is.na(error)) %>%
      group_by(target, model) %>%
      summarise(rmse = sqrt(mean(error^2)), n = n(), .groups = "drop") %>%
      mutate(window = w)
  })) %>% mutate(window = factor(window, levels = names(windows)))
}

# ============================================================
# 6. LSTM (generalised from Macro 2_multi-task_LSTM.r)
# ============================================================
# Two configurations:
#   prior_mode = "sequence_all": prior-year values of ALL targets are appended
#                to every time step of the sequence (Macro design)
#   prior_mode = "head_own"    : each target's own prior-year value enters only
#                its own output head (Sector design, agreed at CP3)
#   norm = "global": input / prior statistics from the full sample (Macro)
#   norm = "fold"  : input statistics from Haver rows dated before the
#                    predicted year; prior / target statistics from the
#                    training rows only (no leakage)
# Targets are always normalised with training-row statistics (as in Macro 4d).

setup_keras <- function(env = "r-keras", threads = NULL) {
  Sys.setenv(TF_CPP_MIN_LOG_LEVEL = "2")
  if (!is.null(threads))   # cap CPU threads when several runs share the machine
    Sys.setenv(TF_NUM_INTRAOP_THREADS = threads, TF_NUM_INTEROP_THREADS = 1,
               OMP_NUM_THREADS = threads)
  reticulate::use_virtualenv(env, required = TRUE)
  suppressPackageStartupMessages(library(keras3))
  invisible(TRUE)
}

# Raw (un-normalised) sequences for every (cutoff, target_year) pair.
#   hv          : data frame `date` + input columns, publication lags APPLIED
#   window      : periods in the look-back (36 monthly / 12 quarterly)
#   min_obs     : a series counts as observed if it has >= min_obs non-NA
#                 values in the window
#   min_share   : keep a pair only if >= min_share of series are observed
#                 (1 = all series, the Macro rule)
#   years       : restrict target years (e.g. 2000:2100)
#   extra_years : target years to include even without any actual (nowcasts)
build_lstm_sequences <- function(hv, input_cols, targets_wide, target_cols, window,
                                 min_obs, min_share = 1,
                                 cutoff_months = c(3L, 6L, 9L, 12L),
                                 years = NULL, extra_years = NULL,
                                 covid_years = c(2020L, 2021L),
                                 rebound_years = c(2021L, 2022L)) {
  stopifnot(all(input_cols %in% names(hv)), all(target_cols %in% names(targets_wide)))
  lookup <- split(targets_wide, targets_wide$year)
  yrs <- targets_wide$year[rowSums(!is.na(targets_wide[target_cols])) > 0]
  if (!is.null(years)) yrs <- intersect(yrs, years)
  yrs <- sort(unique(c(yrs, extra_years)))

  hv_dates <- as.Date(hv$date)
  hv_mat   <- as.matrix(hv[, input_cols, drop = FALSE])
  is_cut   <- as.integer(format(hv_dates, "%m")) %in% cutoff_months
  hv_year  <- as.integer(format(hv_dates, "%Y"))
  pairs <- do.call(rbind, lapply(yrs, function(yr) {
    cuts <- sort(hv_dates[is_cut & hv_year == yr])
    if (!length(cuts)) NULL else data.frame(cutoff = cuts, target_year = yr)
  }))

  n <- nrow(pairs); ni <- length(input_cols); nt <- length(target_cols)
  X <- array(NA_real_, c(n, window, ni))
  Y <- matrix(NA_real_, n, nt, dimnames = list(NULL, target_cols))
  P <- matrix(NA_real_, n, nt, dimnames = list(NULL, target_cols))
  keep <- logical(n)
  for (i in seq_len(n)) {
    e <- match(pairs$cutoff[i], hv_dates); s <- e - window + 1L
    if (is.na(e) || s < 1L) next
    w <- hv_mat[s:e, , drop = FALSE]
    if (mean(colSums(!is.na(w)) >= min_obs) < min_share) next
    X[i, , ] <- w
    pr <- lookup[[as.character(pairs$target_year[i] - 1L)]]
    if (!is.null(pr)) P[i, ] <- as.numeric(pr[target_cols])
    tr <- lookup[[as.character(pairs$target_year[i])]]
    if (!is.null(tr)) Y[i, ] <- as.numeric(tr[target_cols])
    keep[i] <- TRUE
  }
  pairs <- pairs[keep, , drop = FALSE]
  month <- as.integer(format(as.Date(pairs$cutoff), "%m"))
  aux <- cbind(quarter = month / 3 / 4,
               covid   = as.integer(pairs$target_year %in% covid_years),
               rebound = as.integer(pairs$target_year %in% rebound_years))
  list(X = X[keep, , , drop = FALSE], Y = Y[keep, , drop = FALSE],
       P = P[keep, , drop = FALSE], aux = aux, pairs = pairs,
       input_cols = input_cols, target_cols = target_cols, window = window)
}

# Masked MSE: NaN targets contribute nothing (Macro 3b, unchanged)
masked_mse <- function(y_true, y_pred) {
  y_true <- op_cast(y_true, "float32"); y_pred <- op_cast(y_pred, "float32")
  mask   <- op_cast(op_logical_not(op_isnan(y_true)), "float32")
  y_clean <- op_where(op_isnan(y_true), op_zeros_like(y_true), y_true)
  sq <- op_square(y_clean - y_pred) * mask
  op_sum(sq) / op_maximum(op_sum(mask), op_cast(1.0, "float32"))
}

# Shared LSTM encoder + one dense head per target (Macro 3c, generalised)
build_lstm_model <- function(window, n_seq, n_targets, lstm_units, dense_units,
                             dropout, lr, prior_mode = c("head_own", "sequence_all")) {
  prior_mode <- match.arg(prior_mode)
  seq_in <- keras_input(shape = c(window, n_seq), name = "sequence_input")
  aux_in <- keras_input(shape = c(3L), name = "aux_input")
  enc <- seq_in |>
    layer_lstm(units = lstm_units, name = "lstm_encoder") |>
    layer_dropout(rate = dropout, name = "lstm_dropout")
  ctx <- layer_concatenate(list(enc, aux_in), name = "context")
  prior_in <- if (prior_mode == "head_own")
    lapply(seq_len(n_targets), function(k) keras_input(shape = c(1L), name = paste0("prior_", k)))
  else list()
  outs <- lapply(seq_len(n_targets), function(k) {
    h <- if (prior_mode == "head_own")
      layer_concatenate(list(ctx, prior_in[[k]]), name = paste0("ctx_", k)) else ctx
    h |>
      layer_dense(units = dense_units, activation = "relu", name = paste0("dense_", k)) |>
      layer_dense(units = 1L, name = paste0("output_", k))
  })
  m <- keras_model(inputs = c(list(seq_in, aux_in), prior_in), outputs = outs)
  # a single-output model takes the loss directly (a one-element named list fails)
  loss <- if (n_targets == 1L) masked_mse else
    setNames(rep(list(masked_mse), n_targets), paste0("output_", seq_len(n_targets)))
  m |> compile(optimizer = optimizer_adam(learning_rate = lr), loss = loss)
  m
}

# Normalisation context for one training set
#   hv: lagged Haver data (for input statistics); tr: training row indices
lstm_norm_context <- function(seqs, hv, tr, norm = c("fold", "global"),
                              before = NULL, targets_wide = NULL) {
  norm <- match.arg(norm)
  hv_rows <- if (norm == "global" || is.null(before)) seq_len(nrow(hv)) else
    which(as.Date(hv$date) < before)
  st_in <- col_stats(as.matrix(hv[hv_rows, seqs$input_cols, drop = FALSE]))
  st_in$means[is.nan(st_in$means)] <- 0
  st_y <- col_stats(seqs$Y[tr, , drop = FALSE])
  st_y$means[is.nan(st_y$means)] <- 0
  st_p <- if (norm == "global") {
    s <- col_stats(as.matrix(targets_wide[, seqs$target_cols, drop = FALSE]))
    s$means[is.nan(s$means)] <- 0; s
  } else st_y
  list(input = st_in, y = st_y, prior = st_p)
}

lstm_make_x <- function(seqs, idx, ctx, prior_mode) {
  X <- seqs$X[idx, , , drop = FALSE]
  for (j in seq_along(seqs$input_cols))
    X[, , j] <- (X[, , j] - ctx$input$means[j]) / ctx$input$sds[j]
  X[is.na(X)] <- 0
  P <- sweep(sweep(seqs$P[idx, , drop = FALSE], 2, ctx$prior$means, "-"), 2, ctx$prior$sds, "/")
  P[is.na(P)] <- 0
  if (prior_mode == "sequence_all") {
    Pb <- array(rep(P, each = 1), c(length(idx), ncol(P)))
    Pseq <- aperm(array(rep(Pb, times = seqs$window), c(length(idx), ncol(P), seqs$window)),
                  c(1, 3, 2))
    X <- array(c(X, Pseq), c(length(idx), seqs$window, dim(X)[3] + ncol(P)))
    list(sequence_input = X, aux_input = seqs$aux[idx, , drop = FALSE])
  } else {
    c(list(sequence_input = X, aux_input = seqs$aux[idx, , drop = FALSE]),
      setNames(lapply(seq_len(ncol(P)), function(k) matrix(P[, k], ncol = 1L)),
               paste0("prior_", seq_len(ncol(P)))))
  }
}

lstm_make_y <- function(seqs, idx, ctx) {
  Yn <- sweep(sweep(seqs$Y[idx, , drop = FALSE], 2, ctx$y$means, "-"), 2, ctx$y$sds, "/")
  ys <- lapply(seq_len(ncol(Yn)), function(k) {
    v <- Yn[, k]; v[is.na(v)] <- NaN; matrix(v, ncol = 1L) })
  if (length(ys) == 1L) return(ys[[1]])   # single output: plain matrix, not a named list
  setNames(ys, paste0("output_", seq_len(ncol(Yn))))
}

# Train one model (one seed) with early stopping on es_idx; predict pr_idx
# on the raw target scale.
lstm_fit_predict <- function(seqs, fit_idx, es_idx, pr_idx, ctx, cfg, seed,
                             prior_mode, epochs, patience, batch_size) {
  keras3::set_random_seed(seed)
  n_seq <- length(seqs$input_cols) + if (prior_mode == "sequence_all") ncol(seqs$Y) else 0L
  m <- build_lstm_model(seqs$window, n_seq, ncol(seqs$Y), cfg$lstm_units, cfg$dense_units,
                        cfg$dropout, cfg$lr, prior_mode)
  es <- callback_early_stopping(monitor = "val_loss", patience = patience,
                                restore_best_weights = TRUE, verbose = 0L)
  hist <- m |> fit(x = lstm_make_x(seqs, fit_idx, ctx, prior_mode),
                   y = lstm_make_y(seqs, fit_idx, ctx),
                   validation_data = list(lstm_make_x(seqs, es_idx, ctx, prior_mode),
                                          lstm_make_y(seqs, es_idx, ctx)),
                   epochs = epochs, batch_size = batch_size, callbacks = list(es), verbose = 0L)
  raw <- m |> predict(lstm_make_x(seqs, pr_idx, ctx, prior_mode), verbose = 0L)
  if (!is.list(raw)) raw <- list(raw)
  pn <- matrix(do.call(cbind, lapply(raw, as.vector)), nrow = length(pr_idx),
               dimnames = list(NULL, seqs$target_cols))
  rm(m); invisible(gc())
  list(pred = sweep(sweep(pn, 2, ctx$y$sds, "*"), 2, ctx$y$means, "+"),
       epochs_run = length(hist$metrics$val_loss))
}

# Expanding-window CV over cv_years to pick hyperparameters (Macro 3.5).
# Score = mean over targets of per-target RMSE (raw scale) averaged over folds.
run_lstm_cv <- function(seqs, hv, grid, cv_years, n_folds = 3L, fold_size = 2L,
                        prior_mode = "head_own", norm = "fold", targets_wide = NULL,
                        epochs = 60L, patience = 10L, batch_size = 16L, seed = 42L,
                        verbose = TRUE) {
  ty <- seqs$pairs$target_year
  yrs <- sort(unique(ty[ty %in% cv_years])); ny <- length(yrs)
  folds <- Filter(Negate(is.null), lapply(seq_len(n_folds), function(f) {
    ve <- ny - (f - 1L) * fold_size; vs <- ve - fold_size + 1L
    if (vs < 2L) return(NULL)
    list(tr = which(ty %in% yrs[seq_len(vs - 1L)]), va = which(ty %in% yrs[vs:ve]),
         first_val = yrs[vs])
  }))
  res <- lapply(seq_len(nrow(grid)), function(g) {
    cfg <- grid[g, ]
    fold_rmse <- t(vapply(folds, function(fd) {
      ctx <- lstm_norm_context(seqs, hv, fd$tr, norm,
                               before = as.Date(paste0(fd$first_val, "-01-01")),
                               targets_wide = targets_wide)
      out <- tryCatch(lstm_fit_predict(seqs, fd$tr, fd$va, fd$va, ctx, cfg, seed, prior_mode,
                                       epochs, patience, batch_size), error = function(e) NULL)
      if (is.null(out)) return(rep(NA_real_, ncol(seqs$Y)))
      sqrt(colMeans((seqs$Y[fd$va, , drop = FALSE] - out$pred)^2, na.rm = TRUE))
    }, numeric(ncol(seqs$Y))))
    pt <- colMeans(fold_rmse, na.rm = TRUE)
    if (verbose) message(sprintf("  cv config %d/%d: mean RMSE %.4f", g, nrow(grid),
                                 mean(pt, na.rm = TRUE)))
    tibble(config = g, mean_rmse = mean(pt[is.finite(pt)]))
  })
  scores <- bind_cols(grid, bind_rows(res))
  best_i <- which.min(scores$mean_rmse)
  if (!length(best_i)) best_i <- 1L          # every fold failed: fall back to first config
  list(scores = scores, best = grid[best_i, , drop = FALSE], folds = folds)
}

# Walk-forward: one fresh model per validation year and seed, trained on
# target_year < Y with the last int_val_years held out for early stopping.
# Predictions are averaged over seeds.
run_walk_forward_lstm <- function(seqs, hv, cfg, val_start = 2020L, seeds = 42L,
                                  prior_mode = "head_own", norm = "fold",
                                  targets_wide = NULL, int_val_years = 2L,
                                  epochs = 100L, patience = 15L, batch_size = 16L,
                                  verbose = TRUE) {
  ty <- seqs$pairs$target_year
  has_y <- rowSums(!is.na(seqs$Y)) > 0
  val_idx <- which(ty >= val_start & has_y)
  pred_seeds <- array(NA_real_, c(length(val_idx), ncol(seqs$Y), length(seeds)),
                      dimnames = list(NULL, seqs$target_cols, paste0("seed_", seeds)))
  for (yr in sort(unique(ty[val_idx]))) {
    tr <- which(ty < yr & has_y); te <- which(ty == yr)
    tr_yrs <- sort(unique(ty[tr])); iv <- tail(tr_yrs, int_val_years)
    fit_idx <- tr[ty[tr] < min(iv)]; es_idx <- tr[ty[tr] %in% iv]
    if (length(fit_idx) < 8L || !length(es_idx)) {
      if (verbose) message(sprintf("  walk-forward %d: skipped (too few training rows)", yr))
      next
    }
    ctx <- lstm_norm_context(seqs, hv, tr, norm, before = as.Date(paste0(yr, "-01-01")),
                             targets_wide = targets_wide)
    for (s in seq_along(seeds)) {
      out <- tryCatch(lstm_fit_predict(seqs, fit_idx, es_idx, te, ctx, cfg, seeds[s],
                                       prior_mode, epochs, patience, batch_size),
                      error = function(e) { message("  fit error: ", conditionMessage(e)); NULL })
      if (!is.null(out)) pred_seeds[match(te, val_idx), , s] <- out$pred
    }
    if (verbose) message(sprintf("  walk-forward %d: fit=%d es=%d pred=%d", yr,
                                 length(fit_idx), length(es_idx), length(te)))
  }
  list(pred = apply(pred_seeds, c(1, 2), mean, na.rm = TRUE), pred_seeds = pred_seeds,
       val_idx = val_idx, val_pairs = seqs$pairs[val_idx, , drop = FALSE])
}

# Final model(s) on every row with an actual; predict rows in nowcast_years
run_lstm_nowcast <- function(seqs, hv, cfg, nowcast_years, seeds = 42L,
                             prior_mode = "head_own", norm = "fold",
                             targets_wide = NULL, int_val_years = 2L,
                             epochs = 100L, patience = 15L, batch_size = 16L) {
  ty <- seqs$pairs$target_year
  tr <- which(rowSums(!is.na(seqs$Y)) > 0)
  pr <- which(ty %in% nowcast_years)
  if (!length(pr)) return(NULL)
  iv <- tail(sort(unique(ty[tr])), int_val_years)
  fit_idx <- tr[ty[tr] < min(iv)]; es_idx <- tr[ty[tr] %in% iv]
  ctx <- lstm_norm_context(seqs, hv, tr, norm, before = NULL, targets_wide = targets_wide)
  preds <- lapply(seeds, function(s)
    tryCatch(lstm_fit_predict(seqs, fit_idx, es_idx, pr, ctx, cfg, s, prior_mode,
                              epochs, patience, batch_size)$pred, error = function(e) NULL))
  preds <- Filter(Negate(is.null), preds)
  if (!length(preds)) return(NULL)
  list(pred = Reduce(`+`, preds) / length(preds), pairs = seqs$pairs[pr, , drop = FALSE])
}
