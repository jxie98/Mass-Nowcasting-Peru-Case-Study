# =============================================================================
# 9_findings.r
# Blog exhibits for the four key findings. For each finding:
#   FindingN_<slug>_chart.png       chart for the post
#   FindingN_<slug>_table.png/.csv  formatted table (+ data)
#   FindingN_<slug>_flourish.csv    chart data in Flourish-ready (wide) layout
# Finding 2 also has an illustration (FindingN_<slug>_illustration.png).
#
# Errors are standardized: each indicator's RMSE/MAE divided by its 2000-19
# standard deviation (modelled units), then averaged over the 50 indicators
# (see 7_summary_table.r, whose per-indicator output is read here).
#
# Reads  3_outputs/blog/table1_summary_by_target.csv  (7_summary_table.r)
#        3_outputs/blog/flourish/fig*_nowcasts_*.csv  (8_flourish_data.r)
# Writes 3_outputs/blog/findings/
# =============================================================================
script_dir <- local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a))) else
    tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) "2_code")
})
source(file.path(script_dir, "0_functions.R"))
suppressPackageStartupMessages({ library(ggplot2); library(flextable); library(officer) })

project_root <- get_project_root()
blog_dir <- file.path(project_root, "3_outputs", "blog")
out_dir  <- file.path(blog_dir, "findings")
dir.create(out_dir, showWarnings = FALSE)

# ---- Labels, palette, theme (same as 6_compare_blog.r) ---------------------
WIN <- c("All years (2020-2025)" = "2020-25 (incl. COVID)",
         "Excl. COVID (2022-2025)" = "2022-25 (excl. COVID)")
M <- c(pooled = "LSTM: one pooled model",
       sector = "LSTM: one model per sector",
       single = "LSTM: one model per indicator",
       rw     = "Naive (last year's value)",
       lasso  = "Bridge equation (LASSO)")
N_MODELS <- c(pooled = 1, sector = 5, single = 50)
COL <- list(s1 = "#2a78d6", s2 = "#eb6834", s3 = "#1baf7a", s4 = "#eda100",
            surface = "#fcfcfb", ink = "#0b0b0b", ink2 = "#52514e", muted = "#898781",
            grid = "#e1e0d9", axis = "#c3c2b7", shade = "#f0efec")
MCOL <- setNames(c(COL$s1, COL$s2, COL$s3, COL$muted, COL$s4), M)
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
SRC <- "Source: authors' calculations with Haver Analytics and World Development Indicators data."
STD_NOTE <- "Errors in standard deviations of each indicator (2000-19), averaged over 50 indicators."
save_fig <- function(p, name, w = 9, h = 5.5)
  ggsave(file.path(out_dir, name), p, width = w, height = h, dpi = 200, bg = COL$surface)
csv_out <- function(d, name) write_csv(d, file.path(out_dir, name), na = "")

# flextable in the house style; num_cols formatted to `digits`
style_table <- function(d, title, note, num_cols, digits = 2, bold_cells = NULL) {
  ink <- "#1f1f1f"; rule <- "#bdbdbd"
  ft <- flextable(d) %>%
    colformat_double(j = num_cols, digits = digits) %>%
    font(fontname = "Arial", part = "all") %>% fontsize(size = 10, part = "all") %>%
    color(color = ink, part = "all") %>% bold(part = "header") %>%
    align(j = num_cols, align = "center", part = "all") %>%
    border_remove() %>%
    hline_top(border = fp_border(color = ink, width = 1.5), part = "header") %>%
    hline_bottom(border = fp_border(color = ink, width = 1), part = "header") %>%
    hline_bottom(border = fp_border(color = ink, width = 1.5), part = "body") %>%
    padding(padding.top = 4, padding.bottom = 4, part = "all") %>%
    autofit()
  if (!is.null(bold_cells)) for (b in bold_cells) ft <- bold(ft, i = b$i, j = b$j, part = "body")
  ft %>%
    add_header_lines(title) %>% fontsize(i = 1, size = 12, part = "header") %>%
    align(i = 1, align = "left", part = "header") %>%
    add_footer_lines(note) %>% add_footer_lines(SRC) %>%
    fontsize(size = 8, part = "footer") %>% color(color = "#6b6b6b", part = "footer")
}
save_table <- function(ft, d, stem) {
  save_as_image(ft, path = file.path(out_dir, paste0(stem, "_table.png")), res = 200)
  csv_out(d, paste0(stem, "_table.csv"))
}

# ---- Data -------------------------------------------------------------------
pt <- read_csv(file.path(blog_dir, "table1_summary_by_target.csv"), show_col_types = FALSE) %>%
  filter(window %in% names(WIN))
targets <- read_csv(file.path(project_root, "1_data", "processed", "sector_targets.csv"),
                    show_col_types = FALSE)
SECTORS <- c(A = "Macro", B = "Financial", C = "Trade & external", D = "Labor", E = "Poverty")

avg <- pt %>% group_by(window, model) %>%
  summarise(RMSE = mean(std_rmse), MAE = mean(std_mae), n = n(), .groups = "drop")
stopifnot(all(avg$n == 50))
avg_long <- avg %>% select(-n) %>%
  pivot_longer(c(RMSE, MAE), names_to = "metric", values_to = "value") %>%
  mutate(win = WIN[window], method = M[model])
v <- function(m, w, k) avg_long$value[avg_long$model == m & avg_long$window == w & avg_long$metric == k]
W20 <- names(WIN)[1]; W22 <- names(WIN)[2]

# =============================================================================
# Finding 1: all LSTM designs clearly outperform the benchmarks
# =============================================================================
stem <- "Finding1_LSTMs_beat_benchmarks"
d1 <- avg_long %>%
  mutate(method = factor(method, levels = rev(M)),
         win = factor(win, levels = WIN), metric = factor(metric, levels = c("RMSE", "MAE")))
p <- ggplot(d1, aes(value, method, fill = method)) +
  geom_col(width = 0.68) +
  geom_text(aes(label = sprintf("%.2f", value)), hjust = -0.15, size = 3.4, colour = COL$ink2) +
  facet_grid(metric ~ win) +
  # LSTMs in three blues, benchmarks in two greys (dark -> light)
  scale_fill_manual(values = setNames(c("#174a8b", "#2a78d6", "#86b6ec", "#5f5e5a", "#b9b8b1"), M),
                    guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.14))) +
  labs(x = "Average error (standard deviations; lower is better)", y = NULL,
       title = "All three LSTM designs beat both benchmarks",
       subtitle = "The bridge equation does worse than simply carrying forward last year's value",
       caption = paste(STD_NOTE, SRC, sep = "\n")) +
  theme_blog() + theme(strip.text.y = element_text(angle = 0))
save_fig(p, paste0(stem, "_chart.png"), w = 10, h = 6)

f1 <- avg_long %>% mutate(col = paste(metric, win)) %>% select(method, col, value) %>%
  pivot_wider(names_from = col, values_from = value) %>%
  arrange(match(method, M)) %>% rename(Method = method)
csv_out(f1, paste0(stem, "_flourish.csv"))

t1 <- avg_long %>%
  group_by(window, metric) %>%
  mutate(vs_naive = 100 * (value / value[model == "rw"] - 1)) %>% ungroup() %>%
  mutate(col = paste(metric, win)) %>%
  select(Method = method, col, value, vs_naive) %>%
  pivot_wider(names_from = col, values_from = c(value, vs_naive), names_glue = "{col}|{.value}") %>%
  arrange(match(Method, M))
t1 <- t1[, c("Method", as.vector(t(outer(c(paste("RMSE", WIN), paste("MAE", WIN)),
                                          c("value", "vs_naive"), paste, sep = "|"))))]
t1_print <- t1 %>% mutate(across(ends_with("vs_naive"),
  ~ ifelse(Method == M[["rw"]], "-", sprintf("%+.0f%%", .x))))
names(t1_print) <- c("Method", rep(c("Error", "vs naive"), 4))
ft <- flextable(t1_print %>% setNames(make.unique(names(t1_print)))) %>%
  set_header_labels(values = setNames(as.list(names(t1_print)), make.unique(names(t1_print)))) %>%
  add_header_row(values = c("", WIN, WIN), colwidths = c(1, 2, 2, 2, 2)) %>%
  add_header_row(values = c("", "RMSE", "MAE"), colwidths = c(1, 4, 4))
num1 <- names(ft$body$dataset)[c(2, 4, 6, 8)]
ft <- ft %>% colformat_double(j = num1, digits = 2) %>%
  font(fontname = "Arial", part = "all") %>% fontsize(size = 10, part = "all") %>%
  bold(part = "header") %>% align(j = 2:9, align = "center", part = "all") %>%
  border_remove() %>%
  hline_top(border = fp_border(color = "#1f1f1f", width = 1.5), part = "header") %>%
  hline(i = 1:2, j = 2:9, border = fp_border(color = "#bdbdbd"), part = "header") %>%
  hline_bottom(border = fp_border(color = "#1f1f1f", width = 1), part = "header") %>%
  hline(i = 3, border = fp_border(color = "#bdbdbd"), part = "body") %>%
  hline_bottom(border = fp_border(color = "#1f1f1f", width = 1.5), part = "body") %>%
  bold(i = 1, j = num1, part = "body") %>%
  width(j = 1, width = 2.6) %>% width(j = 2:9, width = 0.85) %>%
  padding(padding.top = 4, padding.bottom = 4, part = "all") %>%
  add_header_lines("Nowcast errors of the LSTM designs and the benchmarks") %>%
  fontsize(i = 1, size = 12, part = "header") %>% align(i = 1, align = "left", part = "header") %>%
  add_footer_lines(paste(STD_NOTE, "'vs naive' = % difference from the naive forecast's error (negative = more accurate).")) %>%
  add_footer_lines(SRC) %>% fontsize(size = 8, part = "footer") %>% color(color = "#6b6b6b", part = "footer")
save_as_image(ft, path = file.path(out_dir, paste0(stem, "_table.png")), res = 200)
csv_out(t1 %>% setNames(sub("\\|value$", "", sub("\\|vs_naive$", " - % vs naive", names(t1)))),
        paste0(stem, "_table.csv"))

# =============================================================================
# Finding 2: one model is enough
# =============================================================================
stem <- "Finding2_one_model_is_enough"
lstm <- names(N_MODELS)
d2 <- avg_long %>% filter(model %in% lstm) %>%
  mutate(method = factor(paste0(method, " (", N_MODELS[model], ifelse(model == "pooled", " model)", " models)")),
                         levels = rev(paste0(M[lstm], " (", N_MODELS, c(" model)", " models)", " models)")))),
         panel = factor(paste(metric, win), levels = c(paste("RMSE", WIN), paste("MAE", WIN))))
col2 <- setNames(c(COL$s1, COL$s2, COL$s3), levels(d2$method)[3:1])
p <- ggplot(d2, aes(value, method, colour = method)) +
  geom_segment(aes(x = 0, xend = value, yend = method), linewidth = 2, lineend = "round") +
  geom_point(size = 3.5) +
  geom_text(aes(label = sprintf("%.2f", value)), hjust = -0.45, size = 3.4, colour = COL$ink2) +
  facet_wrap(~panel, ncol = 2) +
  scale_colour_manual(values = col2, guide = "none") +
  scale_x_continuous(expand = expansion(mult = c(0, 0.18))) +
  labs(x = "Average error (standard deviations; lower is better)", y = NULL,
       title = "One pooled model is as accurate as 5 or 50 separate models",
       subtitle = "It has the lowest error on every measure, by 0.02-0.04 standard deviations (1-4 percent)",
       caption = paste(STD_NOTE, SRC, sep = "\n")) +
  theme_blog()
save_fig(p, paste0(stem, "_chart.png"), w = 10, h = 5.5)

# By sector (headline window, RMSE): the result is not driven by one sector
d2s <- pt %>% filter(model %in% lstm) %>%
  left_join(targets %>% select(target = code, sector), by = "target") %>%
  group_by(window, sector, model) %>%
  summarise(RMSE = mean(std_rmse), MAE = mean(std_mae), .groups = "drop") %>%
  mutate(Sector = paste(sector, SECTORS[sector]), Window = WIN[window], Method = M[model])

f2 <- d2 %>% mutate(Design = as.character(method), col = paste(metric, win)) %>%
  select(Design, `Number of models` = model, col, value) %>%
  mutate(`Number of models` = N_MODELS[`Number of models`]) %>%
  pivot_wider(names_from = col, values_from = value) %>% arrange(`Number of models`)
csv_out(f2, paste0(stem, "_flourish.csv"))
csv_out(d2s %>% select(Window, Sector, Method, RMSE) %>%
          pivot_wider(names_from = Method, values_from = RMSE) %>% arrange(Window, Sector),
        paste0(stem, "_by_sector_flourish.csv"))

t2 <- tibble(model = lstm) %>%
  mutate(Design = M[model], `Models to build and tune` = N_MODELS[model],
         `Indicators per model` = 50 / N_MODELS[model],
         `RMSE 2020-25` = sapply(model, v, W20, "RMSE"), `RMSE 2022-25` = sapply(model, v, W22, "RMSE"),
         `MAE 2020-25` = sapply(model, v, W20, "MAE"), `MAE 2022-25` = sapply(model, v, W22, "MAE")) %>%
  select(-model)
gap <- t2 %>% mutate(across(starts_with(c("RMSE", "MAE")), ~ .x - .x[1])) %>% slice(-1)
ft <- style_table(t2, "Three LSTM designs: one model is enough",
                  paste(STD_NOTE, "Lowest value in each column in bold."),
                  num_cols = names(t2)[4:7],
                  bold_cells = list(list(i = 1, j = 4:7))) %>%
  colformat_double(j = 2:3, digits = 0) %>% align(j = 2:3, align = "center", part = "all")
save_table(ft, t2, stem)

# Illustration: 1 vs 5 vs 50 models (each square = one model)
grid_pos <- function(n, ncol) tibble(k = seq_len(n) - 1) %>% mutate(x = k %% ncol, y = -(k %/% ncol))
ill <- bind_rows(
  grid_pos(1, 1)   %>% mutate(design = "pooled"),
  grid_pos(5, 5)   %>% mutate(design = "sector"),
  grid_pos(50, 10) %>% mutate(design = "single")) %>%
  mutate(design = factor(design, levels = lstm,
                         labels = c("One pooled model\n1 model, all 50 indicators",
                                    "One model per sector\n5 models, 10 indicators each",
                                    "One model per indicator\n50 models, 1 indicator each")))
err_lab <- tibble(design = factor(levels(ill$design), levels = levels(ill$design)),
                  lab = sprintf("RMSE %.2f  |  MAE %.2f\n(2022-25, excl. COVID)",
                                sapply(lstm, v, W22, "RMSE"), sapply(lstm, v, W22, "MAE")))
fill3 <- setNames(c(COL$s1, COL$s2, COL$s3), levels(ill$design))
p <- ggplot(ill) +
  geom_tile(aes(x, y, fill = design), width = 0.82, height = 0.82, colour = NA) +
  geom_text(data = err_lab, aes(x = 4.5, y = -6.4, label = lab), size = 4, colour = COL$ink,
            lineheight = 1.1) +
  facet_wrap(~design, nrow = 1) +
  scale_fill_manual(values = fill3, guide = "none") +
  coord_equal(xlim = c(-0.5, 9.5), ylim = c(-7.2, 0.5)) +
  labs(title = "Same accuracy, 50 times fewer models",
       subtitle = "Each square is one LSTM model that must be built, tuned and maintained",
       caption = paste(STD_NOTE, SRC, sep = "\n")) +
  theme_void(base_size = 12) +
  theme(plot.background = element_rect(fill = COL$surface, colour = NA),
        plot.title = element_text(face = "bold", colour = COL$ink),
        plot.subtitle = element_text(colour = COL$ink2, margin = margin(t = 4, b = 22)),
        plot.caption = element_text(colour = COL$muted, hjust = 0),
        strip.text = element_text(face = "bold", colour = COL$ink, size = 11, lineheight = 1.1,
                                  margin = margin(b = 8)),
        panel.spacing = unit(2, "lines"),
        plot.margin = margin(12, 12, 12, 12))
save_fig(p, paste0(stem, "_illustration.png"), w = 11, h = 5)

# =============================================================================
# Finding 3: results hold in turbulent and calmer periods
# =============================================================================
stem <- "Finding3_robust_across_periods"
d3 <- avg_long %>% mutate(win = factor(win, levels = WIN),
                          metric = factor(metric, levels = c("RMSE", "MAE")),
                          method = factor(method, levels = M))
p <- ggplot(d3, aes(win, value, group = method, colour = method)) +
  geom_line(linewidth = 1) + geom_point(size = 3) +
  geom_text(data = d3 %>% filter(win == WIN[2], model %in% c("pooled", "rw", "lasso")),
            aes(label = sprintf("%.2f", value)), hjust = -0.35, size = 3.3, show.legend = FALSE) +
  geom_text(data = d3 %>% filter(win == WIN[1], model %in% c("pooled", "rw", "lasso")),
            aes(label = sprintf("%.2f", value)), hjust = 1.35, size = 3.3, show.legend = FALSE) +
  geom_text(data = d3 %>% filter(model == "single") %>% group_by(metric) %>%
              summarise(value = mean(value) - 0.42, .groups = "drop"),
            aes(x = 1.5, y = value, label = "Three LSTM designs\n(lines overlap)"),
            inherit.aes = FALSE, size = 3.2, colour = COL$ink2, lineheight = 0.95) +
  facet_wrap(~metric) +
  scale_colour_manual(values = MCOL) +
  scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05))) +
  guides(colour = guide_legend(nrow = 2, byrow = TRUE)) +
  labs(x = NULL, y = "Average error (standard deviations)",
       title = "Errors are larger with the COVID-19 years, but the ranking holds",
       subtitle = "Pooled LSTM first, the other LSTMs close behind, then naive, then the bridge equation",
       caption = paste(STD_NOTE, SRC, sep = "\n")) +
  theme_blog() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = COL$grid, linewidth = 0.3))
save_fig(p, paste0(stem, "_chart.png"), w = 10, h = 6)

f3 <- avg_long %>% select(Method = method, metric, win, value) %>%
  mutate(col = paste(metric, win)) %>% select(-metric, -win) %>%
  pivot_wider(names_from = col, values_from = value) %>% arrange(match(Method, M))
csv_out(f3, paste0(stem, "_flourish.csv"))

rk <- avg_long %>% group_by(window, metric) %>% mutate(rank = rank(value)) %>% ungroup()
t3 <- rk %>% mutate(col = paste(metric, win)) %>%
  mutate(cell = sprintf("%d (%.2f)", as.integer(rank), value)) %>%
  select(Method = method, col, cell) %>%
  pivot_wider(names_from = col, values_from = cell) %>% arrange(match(Method, M)) %>%
  select(Method, all_of(c(paste("RMSE", WIN), paste("MAE", WIN))))
ratio <- avg %>% select(window, model, RMSE, MAE) %>%
  pivot_wider(names_from = window, values_from = c(RMSE, MAE)) %>%
  transmute(Method = M[model],
            `RMSE: 2020-25 / 2022-25` = .data[[paste0("RMSE_", W20)]] / .data[[paste0("RMSE_", W22)]],
            `MAE: 2020-25 / 2022-25`  = .data[[paste0("MAE_", W20)]] / .data[[paste0("MAE_", W22)]])
t3 <- t3 %>% left_join(ratio, by = "Method")
ft <- style_table(t3, "Rank (and error) of each method, by period and measure",
                  paste("Rank 1 = lowest error.", STD_NOTE,
                        "Last two columns: error including COVID-19 years relative to error excluding them."),
                  num_cols = names(t3)[6:7]) %>%
  align(j = 2:5, align = "center", part = "all")
save_table(ft, t3, stem)

# =============================================================================
# Finding 4: the LSTMs win by staying steady, not by calling turning points
# =============================================================================
stem <- "Finding4_steady_not_turning_points"
fl <- bind_rows(lapply(list.files(file.path(blog_dir, "flourish"), "^fig.*\\.csv$", full.names = TRUE),
                       read_csv, show_col_types = FALSE))
SHOW <- c("NY.GDP.MKTP.KD.ZG" = "GDP growth (%)",
          "FP.CPI.TOTL.ZG"    = "Inflation, consumer prices (%)",
          "PA.NUS.FCRF"       = "Exchange rate, soles per US$ (% change)",
          "SI.POV.UMIC"       = "Poverty rate at $8.30 a day (change, pp)")
col_pooled <- "LSTM: one pooled model (50 indicators)"
col_bridge <- "Bridge equation (LASSO)"
d4 <- fl %>% filter(`WDI code` %in% names(SHOW), Year >= 2015) %>%
  transmute(code = `WDI code`, Indicator = SHOW[`WDI code`], Year, Period,
            Official = `Official (WDI)`, `LSTM (pooled)` = .data[[col_pooled]],
            `Bridge equation` = .data[[col_bridge]])
stopifnot(n_distinct(d4$code) == 4)
d4l <- d4 %>% pivot_longer(c(Official, `LSTM (pooled)`, `Bridge equation`),
                           names_to = "series", values_to = "value") %>%
  filter(!is.na(value)) %>%
  mutate(series = factor(series, levels = c("Official", "LSTM (pooled)", "Bridge equation")),
         Indicator = factor(Indicator, levels = SHOW))
col4 <- c("Official" = COL$ink, "LSTM (pooled)" = COL$s1, "Bridge equation" = COL$s4)

# Episodes discussed in the text
EP <- tribble(
  ~code,               ~Year, ~Episode,
  "NY.GDP.MKTP.KD.ZG", 2020,  "COVID-19 collapse: LSTM misses it",
  "NY.GDP.MKTP.KD.ZG", 2021,  "Rebound: both miss it",
  "FP.CPI.TOTL.ZG",    2022,  "Inflation spike: bridge catches it",
  "PA.NUS.FCRF",       2021,  "Sol depreciation: bridge catches it",
  "NY.GDP.MKTP.KD.ZG", 2022,  "False alarm: bridge nowcasts ~30% growth",
  "SI.POV.UMIC",       2022,  "False alarm: bridge nowcasts poverty -23 pp")
t4 <- EP %>% left_join(d4, by = c("code", "Year")) %>%
  transmute(Indicator = SHOW[code], Year = as.integer(Year), Episode,
            Official, `LSTM (pooled)`, `Bridge equation`)
stopifnot(!anyNA(t4))

ann <- t4 %>% mutate(code = names(SHOW)[match(Indicator, SHOW)],
                     Indicator = factor(Indicator, levels = SHOW),
                     y = pmax(Official, `LSTM (pooled)`, `Bridge equation`))
p <- ggplot(d4l, aes(Year, value, colour = series)) +
  annotate("rect", xmin = 2019.5, xmax = 2026.5, ymin = -Inf, ymax = Inf, fill = COL$shade) +
  geom_hline(yintercept = 0, colour = COL$axis, linewidth = 0.4) +
  geom_vline(data = ann, aes(xintercept = Year), colour = COL$axis, linetype = "dashed",
             linewidth = 0.4, inherit.aes = FALSE) +
  geom_line(linewidth = 0.8) + geom_point(size = 1.9) +
  facet_wrap(~Indicator, ncol = 2, scales = "free_y") +
  scale_colour_manual(values = col4) +
  scale_x_continuous(breaks = seq(2015, 2026, 2)) +
  labs(x = NULL, y = NULL,
       title = "The LSTM stays close to the trend; the bridge reacts, sometimes wrongly",
       subtitle = "Official value vs end-of-year nowcast. Shaded: test period (2020-25) and 2026 nowcast. Dashed: episodes in the table.",
       caption = paste("Nowcast made at end-December of each year (2026: latest data, end-September 2026).", SRC, sep = "\n")) +
  theme_blog() +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.major.y = element_line(colour = COL$grid, linewidth = 0.3))
save_fig(p, paste0(stem, "_chart.png"), w = 10, h = 7)

csv_out(d4 %>% select(-code) %>% arrange(match(Indicator, SHOW), Year), paste0(stem, "_flourish.csv"))
ft <- style_table(t4, "Selected episodes: official value vs nowcast",
                  "Nowcast made at end-December of the year shown. Units as in the indicator name.",
                  num_cols = names(t4)[4:6], digits = 1) %>%
  colformat_int(j = "Year", big.mark = "")
save_table(ft, t4, stem)

# ---- Numbers quoted in the text --------------------------------------------
pct <- function(a, b) 100 * (a / b - 1)
cat(sprintf("F1 pooled vs naive RMSE: %.0f%% (2020-25), %.0f%% (2022-25); MAE: %.0f%%, %.0f%%\n",
            pct(v("pooled", W20, "RMSE"), v("rw", W20, "RMSE")), pct(v("pooled", W22, "RMSE"), v("rw", W22, "RMSE")),
            pct(v("pooled", W20, "MAE"), v("rw", W20, "MAE")), pct(v("pooled", W22, "MAE"), v("rw", W22, "MAE"))))
cat(sprintf("F1 bridge vs naive RMSE 2022-25: %+.0f%%\n", pct(v("lasso", W22, "RMSE"), v("rw", W22, "RMSE"))))
print(gap)
cat(sprintf("F3 pooled RMSE 2020-25 / 2022-25: %.2f\n", v("pooled", W20, "RMSE") / v("pooled", W22, "RMSE")))
print(rk %>% select(window, metric, model, rank) %>% pivot_wider(names_from = model, values_from = rank))
print(t4)
cat("Saved to", out_dir, "\n")
