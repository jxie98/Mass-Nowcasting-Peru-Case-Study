# =============================================================================
# 7_summary_table.r
# Summary table for the blog: standardized RMSE and MAE of the three LSTM
# designs and the two benchmarks (naive, bridge), all 50 indicators, two
# evaluation windows.
#
# Standardization: each indicator's RMSE and MAE are divided by the standard
# deviation of that indicator (in its modelled form: % growth or pp change)
# over 2000-2019, i.e. before the evaluation period. Errors are then in
# "standard deviations of the indicator" and are averaged (simple mean)
# across the 50 indicators.
#
# Reads 3_outputs/blog/metrics_by_target.csv (written by 6_compare_blog.r).
# Writes 3_outputs/blog/table1_summary.{png,docx,csv}.
# =============================================================================
script_dir <- local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a))) else
    tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) "2_code")
})
source(file.path(script_dir, "0_functions.R"))
suppressPackageStartupMessages({ library(flextable); library(officer) })

project_root   <- get_project_root()
processed_path <- file.path(project_root, "1_data", "processed")
blog_dir       <- file.path(project_root, "3_outputs", "blog")

SD_YEARS <- 2000:2019
WIN <- c("All years (2020-2025)" = "w20", "Excl. COVID (2022-2025)" = "w22")
ROWS <- tibble::tribble(
  ~model,   ~group,         ~label,
  "pooled", "LSTM designs", "One pooled model (all 50 indicators)",
  "sector", "LSTM designs", "One model per sector (5 models)",
  "single", "LSTM designs", "One model per indicator (50 models)",
  "rw",     "Benchmarks",   "Naive (last year's value)",
  "lasso",  "Benchmarks",   "Bridge equation (LASSO)"
)

# ---- Scale: SD of each indicator over 2000-2019 (modelled units) -----------
targets <- read_csv(file.path(processed_path, "sector_targets.csv"), show_col_types = FALSE)
wdi     <- read_csv(file.path(processed_path, "wdi_all_long.csv"), show_col_types = FALSE)
tw      <- build_target_wide(wdi, targets)
scale_sd <- tibble(target = targets$code,
                   sd = sapply(targets$code, function(t) sd(tw[[t]][tw$year %in% SD_YEARS], na.rm = TRUE)))
stopifnot(all(is.finite(scale_sd$sd) & scale_sd$sd > 0))

# ---- Per-indicator errors; naive taken from the rw_* columns ---------------
bt <- read_csv(file.path(blog_dir, "metrics_by_target.csv"), show_col_types = FALSE) %>%
  filter(window %in% names(WIN))
per_target <- bind_rows(
  bt %>% filter(model %in% ROWS$model) %>% select(window, target, model, rmse, mae),
  bt %>% distinct(window, target, rmse = rw_rmse, mae = rw_mae) %>% mutate(model = "rw")
) %>%
  inner_join(scale_sd, by = "target") %>%
  mutate(std_rmse = rmse / sd, std_mae = mae / sd)
stopifnot(all(count(per_target, window, model)$n == 50))

m <- per_target %>% group_by(window, model) %>%
  summarise(rmse = mean(std_rmse), mae = mean(std_mae), .groups = "drop") %>%
  mutate(w = WIN[window]) %>% select(-window)

tab <- m %>%
  pivot_wider(names_from = w, values_from = c(rmse, mae)) %>%
  right_join(ROWS, by = "model") %>%
  arrange(match(model, ROWS$model)) %>%
  select(group, label, rmse_w20, rmse_w22, mae_w20, mae_w22)
write.csv(tab, file.path(blog_dir, "table1_summary.csv"), row.names = FALSE)
write.csv(per_target, file.path(blog_dir, "table1_summary_by_target.csv"), row.names = FALSE)

num_cols <- c("rmse_w20", "rmse_w22", "mae_w20", "mae_w22")
# Lowest value per column, judged at the 2 decimals shown (ties all bolded)
best <- lapply(num_cols, function(cc) {
  r <- round(tab[[cc]], 2); which(r == min(r))
})

# ---- Format ----------------------------------------------------------------
ink <- "#1f1f1f"; muted <- "#6b6b6b"; rule <- "#bdbdbd"; band <- "#f3f6fa"
ft <- as_grouped_data(tab, groups = "group") %>%
  as_flextable(hide_grouplabel = TRUE) %>%
  set_header_labels(label = "", rmse_w20 = "2020-25\n(incl. COVID)",
                    rmse_w22 = "2022-25\n(excl. COVID)",
                    mae_w20 = "2020-25\n(incl. COVID)",
                    mae_w22 = "2022-25\n(excl. COVID)") %>%
  add_header_row(values = c("", "RMSE", "MAE"), colwidths = c(1, 2, 2)) %>%
  colformat_double(j = num_cols, digits = 2) %>%
  font(fontname = "Arial", part = "all") %>%
  fontsize(size = 10, part = "all") %>%
  color(color = ink, part = "all") %>%
  bold(part = "header") %>%
  align(j = num_cols, align = "center", part = "all") %>%
  align(i = 1, align = "center", part = "header") %>%
  border_remove() %>%
  hline_top(border = fp_border(color = ink, width = 1.5), part = "header") %>%
  hline(i = 1, j = 2:5, border = fp_border(color = rule), part = "header") %>%
  hline_bottom(border = fp_border(color = ink, width = 1), part = "header") %>%
  hline_bottom(border = fp_border(color = ink, width = 1.5), part = "body") %>%
  width(j = 1, width = 3.1) %>% width(j = num_cols, width = 1.05) %>%
  padding(padding.top = 4, padding.bottom = 4, part = "all")

grp_rows  <- which(!is.na(ft$body$dataset$group))
data_rows <- which(is.na(ft$body$dataset$group))
ft <- ft %>%
  bold(i = grp_rows, part = "body") %>%
  bg(i = grp_rows, bg = band, part = "body") %>%
  hline(i = setdiff(grp_rows - 1, 0), border = fp_border(color = rule), part = "body") %>%
  padding(i = data_rows, j = 1, padding.left = 14, part = "body")
for (k in seq_along(num_cols)) {
  ft <- bold(ft, i = data_rows[best[[k]]], j = num_cols[k], part = "body")
}

ft <- ft %>%
  add_header_lines("Nowcast errors across 50 Peru indicators (standardized)") %>%
  fontsize(i = 1, size = 12, part = "header") %>%
  align(i = 1, align = "left", part = "header") %>%
  add_footer_lines(paste0(
    "Note: Errors are expressed in standard deviations of each indicator (growth rate ",
    "or percentage-point change, as modelled), computed over 2000-19, and averaged ",
    "across the 50 indicators. Lower is better. Lowest value in each column in bold. ",
    "Nowcasts are made at each quarter-end of the target year (Q1-Q4); the naive ",
    "forecast is last year's official value.")) %>%
  add_footer_lines("Source: Haver Analytics; World Development Indicators; authors' calculations.") %>%
  fontsize(size = 8, part = "footer") %>%
  color(color = muted, part = "footer")

save_as_image(ft, path = file.path(blog_dir, "table1_summary.png"), res = 200)
save_as_docx(ft, path = file.path(blog_dir, "table1_summary.docx"))
print(as.data.frame(tab), digits = 3)

# Sensitivity: median across indicators (robust to a few volatile series)
med <- per_target %>% group_by(window, model) %>%
  summarise(med_rmse = median(std_rmse), med_mae = median(std_mae), .groups = "drop")
print(as.data.frame(med), digits = 3)
cat("Saved table1_summary.{png,docx,csv} to", blog_dir, "\n")
