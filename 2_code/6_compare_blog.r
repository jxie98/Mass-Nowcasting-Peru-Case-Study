# ============================================================
# 6_compare_blog.r — comparison tables and blog figures (plan Step 7)
# ============================================================
# Inputs : 3_outputs/sector_lstm/{pooled,sector,single}/val_predictions.csv, nowcast.csv
#          3_outputs/bridge/val_predictions.csv, nowcast.csv
#          1_data/processed/sector_targets.csv, wdi_all_long.csv
# Outputs: 3_outputs/blog/
#   metrics_by_target.csv   RMSE, MAE and both relative to the naive forecast,
#                           per target x model x window
#   metrics_by_sector.csv   per sector (and "All") x model x window:
#                           geometric-mean relative RMSE / MAE (headline metric),
#                           median relative RMSE / MAE, share of targets beating
#                           the naive forecast
#   metrics_by_cutoff.csv   geometric-mean relative RMSE / MAE by cutoff quarter
#   nowcasts.csv            latest nowcast per target and model + last official value
#   headline_numbers.csv    every number quoted in the blog, with its source
#   fig1_lstm_setups.png        pooled vs sector vs single, all 50 targets, 2 windows
#   fig2_lstm_vs_bridge.png     pooled LSTM vs bridge by sector, 2 windows
#   fig3_nowcasts.png           latest nowcasts vs last official value
#   fig4_predictors.png         predictor heatmap (copy from 3_rhs_selection.r)
#   fig5_by_cutoff.png          error by the quarter the nowcast is made
#   fig6_setups_by_sector.png   annex: pooled vs sector vs single by sector
#   fig7-11_nowcasts_<sector>.png  one chart per sector: official value vs
#                               nowcast of each method, 10 indicator panels
#
# Metric: for each target, RMSE (MAE) over its quarterly nowcasts divided by
# the RMSE (MAE) of the naive forecast (= last year's value of the target, as
# modelled). < 1 beats the naive forecast. Ratios are averaged across targets
# with a geometric mean: targets are in different units, and a few targets
# whose naive error is near zero give ratios of 8-15 that would dominate an
# arithmetic mean. A second benchmark, the historical average of the target
# over 2000..year-1, is reported alongside.
# Windows: 2020-25 (incl. COVID) and 2022-25 (excl. COVID, headline). The
# 2023-25 window in EVAL_WINDOWS (0_functions.R) is not reported here.
# ============================================================

script_dir <- local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a))) else
    tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) "2_code")
})
source(file.path(script_dir, "0_functions.R"))
suppressPackageStartupMessages(library(ggplot2))

project_root   <- get_project_root()
processed_path <- file.path(project_root, "1_data", "processed")
lstm_dir       <- file.path(project_root, "3_outputs", "sector_lstm")
bridge_dir     <- file.path(project_root, "3_outputs", "bridge")
out_dir        <- file.path(project_root, "3_outputs", "blog")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

HEADLINE <- "Excl. COVID (2022-2025)"
WINDOWS  <- EVAL_WINDOWS[c("All years (2020-2025)", "Excl. COVID (2022-2025)")]
WINDOW_LABELS <- c("All years (2020-2025)"             = "2020-25 (incl. COVID)",
                   "Excl. COVID (2022-2025)"           = "2022-25 (excl. COVID)")
SECTOR_LABELS <- c(A = "Macro", B = "Financial", C = "Trade & external",
                   D = "Labor", E = "Poverty")
MODEL_LABELS <- c(pooled = "LSTM: one pooled model (50 indicators)",
                  sector = "LSTM: one model per sector (5 models)",
                  single = "LSTM: one model per indicator (50 models)",
                  lasso  = "Bridge equation (LASSO)",
                  ols    = "Bridge equation (OLS)",
                  hmean  = "Historical average")

# ---- palette + theme (dataviz reference palette, light mode) ----------
# Colour follows the model in every figure: pooled = blue, sector = orange,
# single = aqua, bridge = yellow; benchmarks in neutral ink.
COL <- list(s1 = "#2a78d6", s2 = "#eb6834", s3 = "#1baf7a", s4 = "#eda100",
            surface = "#fcfcfb", ink = "#0b0b0b", ink2 = "#52514e", muted = "#898781",
            grid = "#e1e0d9", axis = "#c3c2b7")
MODEL_COL <- setNames(c(COL$s1, COL$s2, COL$s3, COL$s4, COL$muted),
                      MODEL_LABELS[c("pooled", "sector", "single", "lasso", "hmean")])
theme_blog <- function() {
  theme_minimal(base_size = 12, base_family = "sans") +
    theme(plot.background  = element_rect(fill = COL$surface, colour = NA),
          panel.background = element_rect(fill = COL$surface, colour = NA),
          panel.grid.major.y = element_blank(), panel.grid.minor = element_blank(),
          panel.grid.major.x = element_line(colour = COL$grid, linewidth = 0.3),
          axis.text  = element_text(colour = COL$ink2), axis.title = element_text(colour = COL$ink2),
          plot.title = element_text(colour = COL$ink, face = "bold"),
          plot.subtitle = element_text(colour = COL$ink2),
          plot.caption  = element_text(colour = COL$muted, hjust = 0),
          plot.title.position = "plot", plot.caption.position = "plot",
          legend.position = "top", legend.justification = "left",
          legend.text = element_text(colour = COL$ink2), legend.title = element_blank(),
          strip.text = element_text(colour = COL$ink, face = "bold", hjust = 0),
          panel.spacing = unit(1.2, "lines"))
}
save_fig <- function(p, name, w = 9, h = 5.5)
  ggsave(file.path(out_dir, name), p, width = w, height = h, dpi = 200, bg = COL$surface)
naive_line <- function() geom_vline(xintercept = 1, colour = COL$axis, linewidth = 0.6)
CAPTION_SRC <- "Source: authors' calculations with Haver Analytics and World Development Indicators data."

# ------------------------------------------------------------
# 1. Load predictions
# ------------------------------------------------------------
targets <- read_csv(file.path(processed_path, "sector_targets.csv"), show_col_types = FALSE)
wdi     <- read_csv(file.path(processed_path, "wdi_all_long.csv"), show_col_types = FALSE)
targets_wide <- build_target_wide(wdi, targets)

read_need <- function(f) {
  if (!file.exists(f)) stop("Missing ", f, " - run the model scripts first.")
  read_csv(f, show_col_types = FALSE)
}
lstm_val <- bind_rows(lapply(c("pooled", "sector", "single"), function(s)
  read_need(file.path(lstm_dir, s, "val_predictions.csv")))) %>%
  transmute(model = setup, target, cutoff, target_year, actual, pred)
bridge_val <- read_need(file.path(bridge_dir, "val_predictions.csv")) %>%
  transmute(model, target, cutoff, target_year, actual, pred)
val <- bind_rows(lstm_val, bridge_val) %>%
  left_join(targets %>% select(target = code, sector), by = "target")

# Every model must cover the same (target, cutoff) rows with identical actuals
key_ok <- val %>% group_by(target, cutoff) %>%
  summarise(n_models = n_distinct(model), act_sd = sd(actual), .groups = "drop")
stopifnot(all(key_ok$n_models == n_distinct(val$model)),
          all(is.na(key_ok$act_sd) | key_ok$act_sd < 1e-8))

# Benchmarks: naive (last year's value) and historical average (2000..year-1)
bench_val <- function(t, y, how) {
  v <- targets_wide[[t]]; yr <- targets_wide$year
  if (how == "rw") v[match(y - 1L, yr)] else mean(v[yr >= 2000 & yr < y], na.rm = TRUE)
}
base <- val %>% distinct(target, cutoff, target_year, actual, sector)
bench <- bind_rows(
  base %>% mutate(model = "rw",    pred = mapply(bench_val, target, target_year, "rw")),
  base %>% mutate(model = "hmean", pred = mapply(bench_val, target, target_year, "hmean")))
long <- bind_rows(val, bench) %>% mutate(error = pred - actual,
                                         quarter = as.integer(format(as.Date(cutoff), "%m")) / 3)

# ------------------------------------------------------------
# 2. Metrics
# ------------------------------------------------------------
gmean <- function(x) { x <- x[is.finite(x) & x > 0]; exp(mean(log(x))) }

err_by <- function(d, ...) d %>% filter(!is.na(error)) %>% group_by(target, model, ...) %>%
  summarise(rmse = sqrt(mean(error^2)), mae = mean(abs(error)), n = n(), .groups = "drop")
rel_to_rw <- function(e, ...) e %>% filter(model != "rw") %>%
  left_join(e %>% filter(model == "rw") %>% select(target, ..., rw_rmse = rmse, rw_mae = mae),
            by = c("target", sapply(rlang::ensyms(...), rlang::as_string))) %>%
  mutate(rel_rmse = rmse / rw_rmse, rel_mae = mae / rw_mae)

by_target <- bind_rows(lapply(names(WINDOWS), function(w)
  err_by(long %>% filter(target_year %in% WINDOWS[[w]])) %>% mutate(window = w))) %>%
  rel_to_rw(window) %>%
  left_join(targets %>% select(target = code, sector, name), by = "target") %>%
  relocate(window, sector, target, name, model)

summ <- function(d) d %>% summarise(
  gmean_rel_rmse = gmean(rel_rmse), gmean_rel_mae = gmean(rel_mae),
  median_rel_rmse = median(rel_rmse, na.rm = TRUE), median_rel_mae = median(rel_mae, na.rm = TRUE),
  share_beat_rw = mean(rel_rmse < 1, na.rm = TRUE), n_targets = sum(!is.na(rel_rmse)),
  .groups = "drop")
by_sector <- bind_rows(summ(by_target %>% group_by(window, sector, model)),
                       summ(by_target %>% group_by(window, model)) %>% mutate(sector = "All")) %>%
  arrange(factor(window, levels = names(WINDOWS)), sector, model)

by_cutoff <- err_by(long %>% filter(target_year %in% WINDOWS[[HEADLINE]]), quarter) %>%
  rel_to_rw(quarter) %>% group_by(model, quarter) %>%
  summarise(gmean_rel_rmse = gmean(rel_rmse), gmean_rel_mae = gmean(rel_mae), .groups = "drop") %>%
  mutate(window = HEADLINE)

write_csv(by_target, file.path(out_dir, "metrics_by_target.csv"))
write_csv(by_sector, file.path(out_dir, "metrics_by_sector.csv"))
write_csv(by_cutoff, file.path(out_dir, "metrics_by_cutoff.csv"))
unlink(file.path(out_dir, "metrics_rmse_mae_by_sector.csv"))   # superseded by metrics_by_sector.csv

# ------------------------------------------------------------
# 3. Nowcasts (latest cutoff per target) vs last official value
# ------------------------------------------------------------
now <- bind_rows(
  bind_rows(lapply(c("pooled", "sector", "single"), function(s)
    read_need(file.path(lstm_dir, s, "nowcast.csv")))) %>% transmute(model = setup, target, cutoff, target_year, pred),
  read_need(file.path(bridge_dir, "nowcast.csv")) %>% transmute(model, target, cutoff, target_year, pred)) %>%
  group_by(model, target, target_year) %>% slice_max(cutoff, n = 1) %>% ungroup()
last_off <- bind_rows(lapply(targets$code, function(t) {
  y <- max(targets_wide$year[!is.na(targets_wide[[t]])])
  tibble(target = t, last_year = y, last_value = targets_wide[[t]][targets_wide$year == y])
}))
nowcasts <- now %>% left_join(last_off, by = "target") %>%
  left_join(targets %>% select(target = code, sector, name, transform), by = "target")
write_csv(nowcasts, file.path(out_dir, "nowcasts.csv"))

# ------------------------------------------------------------
# 4. Figures
# ------------------------------------------------------------
lab_models <- function(d, keep) d %>% filter(model %in% keep) %>%
  mutate(model = factor(MODEL_LABELS[model], levels = MODEL_LABELS[keep]))
lab_windows <- function(d) d %>%
  mutate(window_lab = factor(WINDOW_LABELS[as.character(window)], levels = rev(WINDOW_LABELS)))
metric_long <- function(d) d %>%
  pivot_longer(c(gmean_rel_rmse, gmean_rel_mae), names_to = "metric", values_to = "value") %>%
  mutate(metric = factor(ifelse(metric == "gmean_rel_rmse", "RMSE", "MAE"), levels = c("RMSE", "MAE")))
LSTMS <- c("pooled", "sector", "single")

# Fig 1: the three LSTM designs, all 50 indicators, every window, RMSE and MAE
f1 <- by_sector %>% filter(sector == "All") %>% lab_models(LSTMS) %>% lab_windows() %>% metric_long()
p1 <- ggplot(f1, aes(x = value, y = window_lab, colour = model)) +
  naive_line() +
  geom_point(size = 3.6, position = position_dodge(width = 0.55)) +
  facet_wrap(~metric, nrow = 1) +
  scale_colour_manual(values = MODEL_COL) +
  scale_x_continuous(limits = c(0.55, 1.0)) +
  guides(colour = guide_legend(ncol = 1)) +
  labs(x = "Error relative to a naive forecast (1 = repeat last year's value; lower is better)",
       y = NULL, title = "One pooled LSTM is as accurate as 5 or 50 separate models",
       subtitle = "Average nowcast error across 50 Peru indicators, by test period",
       caption = paste("Geometric mean across indicators of each model's error divided by the naive forecast's error.",
                       CAPTION_SRC, sep = "\n")) +
  theme_blog()
save_fig(p1, "fig1_lstm_setups.png", h = 5.2)

# Fig 2: pooled LSTM vs bridge by sector, every window (RMSE)
sec_levels <- rev(unname(SECTOR_LABELS))
f2 <- by_sector %>% filter(sector != "All") %>% lab_models(c("pooled", "lasso")) %>%
  mutate(sector_lab = factor(SECTOR_LABELS[sector], levels = sec_levels),
         window_lab = factor(WINDOW_LABELS[as.character(window)], levels = WINDOW_LABELS))
p2 <- ggplot(f2, aes(x = gmean_rel_rmse, y = sector_lab, colour = model)) +
  naive_line() +
  geom_point(size = 3.4, position = position_dodge(width = 0.5)) +
  facet_wrap(~window_lab, nrow = 1) +
  scale_colour_manual(values = MODEL_COL) +
  scale_x_continuous(limits = c(0.5, 1.8), breaks = c(0.5, 1, 1.5)) +
  labs(x = "RMSE relative to a naive forecast (lower is better)", y = NULL,
       title = "The LSTM beats a bridge equation in every sector",
       subtitle = "Average nowcast error by sector, Peru",
       caption = paste("Bridge: one LASSO regression per indicator on the same Haver predictors.", CAPTION_SRC,
                       sep = "\n")) +
  theme_blog()
save_fig(p2, "fig2_lstm_vs_bridge.png", w = 9.5, h = 5)

# Fig 3: latest nowcasts vs last official value, first target of each sector
head_t <- targets %>% group_by(sector) %>% slice(1) %>% ungroup() %>% pull(code)
f3 <- nowcasts %>% filter(target %in% head_t) %>% lab_models(c("pooled", "lasso")) %>%
  group_by(target) %>% filter(target_year == max(target_year)) %>% ungroup() %>%
  mutate(lab = str_wrap(paste0(SECTOR_LABELS[sector], ": ", name, " (", target_year, ")"), 42))
f3_off <- f3 %>% distinct(lab, last_year, last_value)
p3 <- ggplot(f3, aes(y = lab)) +
  geom_vline(xintercept = 0, colour = COL$axis, linewidth = 0.4) +
  geom_point(data = f3_off, aes(x = last_value), shape = 21, size = 3.6, colour = COL$ink2,
             fill = COL$surface, stroke = 1) +
  geom_point(aes(x = pred, colour = model), size = 3.6, position = position_dodge(width = 0.45)) +
  scale_colour_manual(values = MODEL_COL) +
  labs(x = "Value as modelled (growth %, or change in percentage points)", y = NULL,
       title = "Where Peru is heading: latest nowcasts",
       subtitle = "Filled dots: nowcast from the latest Haver data. Open dot: last official WDI value.",
       caption = CAPTION_SRC) +
  theme_blog()
save_fig(p3, "fig3_nowcasts.png", w = 10, h = 5)

# Fig 4: predictor heatmap (made by 3_rhs_selection.r)
file.copy(file.path(lstm_dir, "rhs_selection_heatmap.png"),
          file.path(out_dir, "fig4_predictors.png"), overwrite = TRUE)

# Fig 5: error by cutoff quarter, pooled LSTM vs bridge (headline window)
f5 <- by_cutoff %>% lab_models(c("pooled", "lasso"))
p5 <- ggplot(f5, aes(x = quarter, y = gmean_rel_rmse, colour = model)) +
  geom_hline(yintercept = 1, colour = COL$axis, linewidth = 0.6) +
  geom_line(linewidth = 0.9) + geom_point(size = 3) +
  scale_colour_manual(values = MODEL_COL) +
  scale_x_continuous(breaks = 1:4, labels = c("End-March", "End-June", "End-September", "End-December")) +
  labs(x = "Nowcast made at", y = "RMSE relative to a naive forecast",
       title = "The LSTM beats the naive forecast from the first quarter; the bridge never does",
       subtitle = "Average across all 50 indicators, 2022-25, Peru",
       caption = CAPTION_SRC) +
  theme_blog() + theme(panel.grid.major.y = element_line(colour = COL$grid, linewidth = 0.3),
                       panel.grid.major.x = element_blank())
save_fig(p5, "fig5_by_cutoff.png")

# Fig 6 (annex): the three LSTM designs by sector, headline window
f6 <- by_sector %>% filter(window == HEADLINE, sector != "All") %>% lab_models(LSTMS) %>%
  mutate(sector_lab = factor(SECTOR_LABELS[sector], levels = sec_levels)) %>% metric_long()
p6 <- ggplot(f6, aes(x = value, y = sector_lab, colour = model)) +
  naive_line() +
  geom_point(size = 3.4, position = position_dodge(width = 0.55)) +
  facet_wrap(~metric, nrow = 1) +
  scale_colour_manual(values = MODEL_COL) +
  guides(colour = guide_legend(ncol = 1)) +
  labs(x = "Error relative to a naive forecast (lower is better)", y = NULL,
       title = "By sector, the three LSTM designs are close",
       subtitle = "Average nowcast error by sector, 2022-25, Peru",
       caption = CAPTION_SRC) +
  theme_blog()
save_fig(p6, "fig6_setups_by_sector.png", h = 5.5)

# Figs 7-11: official value vs nowcast of every method, one chart per sector
# (10 panels = 10 indicators). Test years 2020-25: nowcast made at end-December
# of the target year. Years without an official value yet (2025 for some
# indicators, 2026): latest available nowcast.
SHOW_FROM <- 2012L
VAL_START_PLOT <- 2020L
PLOT_MODELS <- c(LSTMS, "lasso")
UNIT <- c(level = "as published", pct = "% growth", pp = "change, pp")
pred_q4 <- val %>% filter(model %in% PLOT_MODELS, format(as.Date(cutoff), "%m") == "12") %>%
  select(model, target, target_year, pred)
pred_new <- nowcasts %>% filter(model %in% PLOT_MODELS) %>% select(model, target, target_year, pred)
stopifnot(nrow(inner_join(pred_q4, pred_new, by = c("model", "target", "target_year"))) == 0)
pred_plot <- bind_rows(pred_q4, pred_new) %>% lab_models(PLOT_MODELS)
actual_plot <- bind_rows(lapply(targets$code, function(t)
  tibble(target = t, target_year = targets_wide$year, actual = targets_wide[[t]]))) %>%
  filter(target_year >= SHOW_FROM, !is.na(actual))
panel_lab <- targets %>%
  transmute(target = code, sector,
            lab = paste0(str_wrap(str_remove(name, " \\(modeled ILO estimate\\)"), 48), "\n[",
                         UNIT[transform], "]"))
x_max <- max(pred_plot$target_year)

for (s in names(SECTOR_LABELS)) {
  lv <- panel_lab %>% filter(sector == s)
  act <- actual_plot %>% inner_join(lv, by = "target") %>% mutate(lab = factor(lab, levels = lv$lab))
  prd <- pred_plot %>% inner_join(lv, by = "target") %>% mutate(lab = factor(lab, levels = lv$lab))
  p <- ggplot() +
    annotate("rect", xmin = VAL_START_PLOT - 0.5, xmax = x_max + 0.5, ymin = -Inf, ymax = Inf,
             fill = "#f0efec") +
    geom_line(data = act, aes(target_year, actual), colour = COL$ink, linewidth = 0.7) +
    geom_point(data = act, aes(target_year, actual), colour = COL$ink, size = 1.3) +
    geom_line(data = prd, aes(target_year, pred, colour = model), linewidth = 0.5, alpha = 0.8) +
    geom_point(data = prd, aes(target_year, pred, colour = model), size = 1.9) +
    facet_wrap(~lab, ncol = 2, scales = "free_y") +
    scale_colour_manual(values = MODEL_COL) +
    scale_x_continuous(breaks = seq(SHOW_FROM, x_max, 2), limits = c(SHOW_FROM - 0.3, x_max + 0.5)) +
    scale_y_continuous(labels = scales::label_number(scale_cut = scales::cut_short_scale())) +
    guides(colour = guide_legend(ncol = 2)) +
    labs(x = NULL, y = NULL,
         title = paste0(SECTOR_LABELS[s], ": official values and nowcasts, Peru"),
         subtitle = paste0("Black line: official WDI value. Dots: nowcast by each method; shaded area = test period (",
                           VAL_START_PLOT, "-25) and ", x_max, " nowcast."),
         caption = paste("Nowcast made at end-December of each year; for years without an official value yet,",
                         "the latest available nowcast (end-September 2026 data).", CAPTION_SRC, sep = "\n")) +
    theme_blog() +
    theme(panel.grid.major.y = element_line(colour = COL$grid, linewidth = 0.3),
          panel.grid.major.x = element_blank(),
          strip.text = element_text(colour = COL$ink, face = "bold", hjust = 0, size = 9.5),
          axis.text = element_text(size = 8.5))
  save_fig(p, sprintf("fig%d_nowcasts_%s_%s.png", 6 + match(s, names(SECTOR_LABELS)), s,
                      tolower(gsub("[^A-Za-z]+", "_", SECTOR_LABELS[s]))), w = 10, h = 12.5)
}

# Superseded figure from the earlier version of this script
unlink(file.path(out_dir, "fig1_pooled_sector_single.png"))

# ------------------------------------------------------------
# 5. Headline numbers (every figure quoted in the blog comes from here)
# ------------------------------------------------------------
wide <- by_sector %>% select(window, sector, model, gmean_rel_rmse, gmean_rel_mae) %>%
  pivot_wider(names_from = model, values_from = c(gmean_rel_rmse, gmean_rel_mae))
count_beats <- function(win, a, b, metric) {
  w <- wide %>% filter(window == win, sector != "All")
  sum(w[[paste0(metric, "_", a)]] < w[[paste0(metric, "_", b)]])
}
nums <- bind_rows(
  by_sector %>% filter(sector == "All") %>%
    pivot_longer(c(gmean_rel_rmse, gmean_rel_mae, share_beat_rw), names_to = "key") %>%
    transmute(key, model, window, value),
  bind_rows(lapply(names(WINDOWS), function(w) bind_rows(lapply(c("gmean_rel_rmse", "gmean_rel_mae"), function(m) {
    allw <- wide %>% filter(window == w, sector == "All")
    tibble(window = w, model = "pooled",
           key = paste0(c("pooled_best_lstm_overall", "sectors_pooled_beats_sector",
                          "sectors_pooled_beats_single", "sectors_pooled_beats_lasso"), "__", m),
           value = c(as.numeric(allw[[paste0(m, "_pooled")]] <= min(allw[[paste0(m, "_sector")]],
                                                                   allw[[paste0(m, "_single")]])),
                     count_beats(w, "pooled", "sector", m), count_beats(w, "pooled", "single", m),
                     count_beats(w, "pooled", "lasso", m)))
  }))))
) %>% mutate(source = "metrics_by_sector.csv")
write_csv(nums, file.path(out_dir, "headline_numbers.csv"))

options(width = 160)
for (m in c("gmean_rel_rmse", "gmean_rel_mae")) {
  message("\n==== ", m, " (geometric mean, relative to naive forecast) ====")
  print(by_sector %>% filter(model != "ols") %>% select(window, sector, model, all_of(m)) %>%
          mutate(window = WINDOW_LABELS[as.character(window)], across(all_of(m), ~ round(.x, 3))) %>%
          pivot_wider(names_from = model, values_from = all_of(m), names_prefix = "m_"), n = Inf)
}
message("\n==== Pooled-LSTM win counts ====")
print(nums %>% filter(grepl("__", key)) %>% mutate(window = WINDOW_LABELS[window]) %>%
        select(window, key, value), n = Inf)
