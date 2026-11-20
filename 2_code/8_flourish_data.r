# =============================================================================
# 8_flourish_data.r
# Exports the data behind figs 7-11 (official value vs nowcast of each method,
# one chart per sector, 10 indicators each) as five CSV files for Flourish.
#
# Same rows as the figures in 6_compare_blog.r:
#   - official WDI value (modelled form: level / % growth / pp change), 2012+
#   - test years 2020-25: nowcast made at end-December of the target year
#   - years without an official value yet: latest available nowcast
#
# Layout: wide, one row per indicator-year. Flourish line chart: X = Year,
# Y = "Official (WDI)" + the 4 method columns, grid/filter = "Indicator (with unit)".
# Blank cells = no value (no official value yet / no nowcast before 2020).
#
# Reads 3_outputs/sector_lstm/*/val_predictions.csv, 3_outputs/bridge/val_predictions.csv,
#       3_outputs/blog/nowcasts.csv, 1_data/processed/{sector_targets,wdi_all_long}.csv
# Writes 3_outputs/blog/flourish/fig7..fig11_nowcasts_<sector>.csv
# =============================================================================
script_dir <- local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a))) else
    tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) "2_code")
})
source(file.path(script_dir, "0_functions.R"))

project_root   <- get_project_root()
processed_path <- file.path(project_root, "1_data", "processed")
lstm_dir       <- file.path(project_root, "3_outputs", "sector_lstm")
bridge_dir     <- file.path(project_root, "3_outputs", "bridge")
blog_dir       <- file.path(project_root, "3_outputs", "blog")

SHOW_FROM  <- 2012L
TEST_START <- 2020L
MODELS <- c(pooled = "LSTM: one pooled model (50 indicators)",
            sector = "LSTM: one model per sector (5 models)",
            single = "LSTM: one model per indicator (50 models)",
            lasso  = "Bridge equation (LASSO)")
SECTORS <- c(A = "Macro", B = "Financial", C = "Trade & external", D = "Labor", E = "Poverty")
UNIT <- c(level = "as published", pct = "% growth", pp = "change, pp")

targets <- read_csv(file.path(processed_path, "sector_targets.csv"), show_col_types = FALSE)
wdi     <- read_csv(file.path(processed_path, "wdi_all_long.csv"), show_col_types = FALSE)
tw      <- build_target_wide(wdi, targets)

# ---- Predictions: end-December nowcast in test years + latest nowcast ------
val <- bind_rows(
  bind_rows(lapply(names(MODELS)[1:3], function(s)
    read_csv(file.path(lstm_dir, s, "val_predictions.csv"), show_col_types = FALSE))) %>%
    transmute(model = setup, target, cutoff, target_year, pred),
  read_csv(file.path(bridge_dir, "val_predictions.csv"), show_col_types = FALSE) %>%
    transmute(model, target, cutoff, target_year, pred))
pred_q4 <- val %>% filter(model %in% names(MODELS), format(as.Date(cutoff), "%m") == "12") %>%
  transmute(model, target, target_year, pred, cutoff = as.Date(cutoff))
pred_new <- read_csv(file.path(blog_dir, "nowcasts.csv"), show_col_types = FALSE) %>%
  filter(model %in% names(MODELS)) %>%
  transmute(model, target, target_year, pred, cutoff = as.Date(cutoff))
stopifnot(nrow(inner_join(pred_q4, pred_new, by = c("model", "target", "target_year"))) == 0)
preds <- bind_rows(pred_q4, pred_new)
stopifnot(!anyDuplicated(preds[c("model", "target", "target_year")]))

# Cutoff (data vintage) per indicator-year, for the "Nowcast made at" column
vintage <- preds %>% group_by(target, target_year) %>%
  summarise(cutoff = max(cutoff), .groups = "drop")

pred_wide <- preds %>% select(-cutoff) %>%
  pivot_wider(names_from = model, values_from = pred, names_prefix = "m_")

official <- bind_rows(lapply(targets$code, function(t)
  tibble(target = t, target_year = tw$year, official = tw[[t]]))) %>%
  filter(target_year >= SHOW_FROM, !is.na(official))

dat <- full_join(official, pred_wide, by = c("target", "target_year")) %>%
  left_join(vintage, by = c("target", "target_year")) %>%
  filter(target_year >= SHOW_FROM) %>%
  left_join(targets %>% select(target = code, sector, name, transform), by = "target") %>%
  mutate(
    period = case_when(target_year < TEST_START ~ "History",
                       !is.na(official)         ~ "Test period (out-of-sample)",
                       TRUE                     ~ "Nowcast (no official value yet)"),
    indicator = str_remove(name, " \\(modeled ILO estimate\\)"),
    unit = unname(UNIT[transform]),
    sector_lab = paste(sector, SECTORS[sector]),
    nowcast_made = ifelse(is.na(cutoff), NA_character_, format(cutoff, "%b %Y"))) %>%
  arrange(sector, match(target, targets$code), target_year) %>%
  transmute(Sector = sector_lab, Indicator = indicator,
            `Indicator (with unit)` = paste0(indicator, " [", unit, "]"),
            `WDI code` = target, Unit = unit, Year = target_year, Period = period,
            `Official (WDI)` = official,
            !!MODELS[["pooled"]] := m_pooled, !!MODELS[["sector"]] := m_sector,
            !!MODELS[["single"]] := m_single, !!MODELS[["lasso"]] := m_lasso,
            `Nowcast made at (data up to)` = nowcast_made)

# Checks: 50 indicators, 10 per sector; every test/nowcast row has all 4 methods
stopifnot(n_distinct(dat$`WDI code`) == 50,
          all(table(distinct(dat, Sector, `WDI code`)$Sector) == 10),
          all(complete.cases(dat[dat$Period != "History", MODELS])))

# ---- Write one CSV per sector ---------------------------------------------
out_dir <- file.path(blog_dir, "flourish")
dir.create(out_dir, showWarnings = FALSE)
for (s in names(SECTORS)) {
  f <- file.path(out_dir, sprintf("fig%d_nowcasts_%s_%s.csv", 6 + match(s, names(SECTORS)), s,
                                  tolower(gsub("[^A-Za-z]+", "_", SECTORS[s]))))
  write_csv(dat %>% filter(Sector == paste(s, SECTORS[s])), f, na = "")
  cat("Saved", f, "
")
}
print(count(dat, Sector, Period))
