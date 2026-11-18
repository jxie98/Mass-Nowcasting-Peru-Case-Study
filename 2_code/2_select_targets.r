# ============================================================
# 2_select_targets.r — choose and verify the 50 WDI targets (CP2)
# ============================================================
# Input : 1_data/processed/wdi_all_long.csv (from 1_data_processing.r)
# Output: 1_data/processed/sector_targets.csv           final 50 targets
#         1_data/processed/sector_target_candidates.csv full pool + why dropped
#         1_data/processed/sector_target_corr.csv       |corr| of selected targets
#
# Selection rule (agreed at CP2, 2026-11-17)
#   * Sectors are economic, not strictly the WDI_Nowcasting_Groups groups:
#     cross-group indicators are allowed where they make more economic sense.
#   * Each sector has a candidate pool, ordered by economic priority, and each
#     candidate carries a "kind" label. Candidates are taken greedily in order:
#       - eligibility: >= MIN_YEARS non-NA years, last obs >= MIN_LAST_YEAR,
#         >= MIN_TRAIN_OBS obs in 2000-2019;
#       - only ONE target per kind (no near-identical concepts);
#       - |corr| of the transformed series (2000-2025) with every target
#         already selected, in ANY sector, must be <= CORR_MAX.
#     The first 10 survivors per sector are the targets.
#   * Transform:
#       level -> already a growth / inflation rate (".ZG"), or a flow that can
#                be negative: modelled as is
#       pct   -> levels (US$, LCU, indices, counts): 100 * log growth
#       pp    -> ratios / shares / rates (.ZS, Gini): change in percentage points
# ============================================================

script_dir <- local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a))) else
    tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) "2_code")
})
source(file.path(script_dir, "0_functions.R"))

project_root   <- get_project_root()
processed_path <- file.path(project_root, "1_data", "processed")

N_PER_SECTOR   <- 10
MIN_YEARS      <- 15
MIN_LAST_YEAR  <- 2023
MIN_TRAIN_OBS  <- 8
TRAIN_YEARS    <- 2000:2019
TEST_YEARS     <- 2020:2025
CORR_YEARS     <- 2000:2025
CORR_MAX       <- 0.90
CORR_MIN_N     <- 10

# ------------------------------------------------------------
# Candidate pools (row order = priority within sector)
# ------------------------------------------------------------
pool <- tribble(
  ~sector, ~code,                  ~transform, ~kind,
  # ---- A: Macro & National Accounts ---------------------------------------
  "A", "NY.GDP.MKTP.KD.ZG",       "level", "output",
  "A", "NE.CON.PRVT.KD.ZG",       "level", "consumption_private",
  "A", "NE.CON.GOVT.KD.ZG",       "level", "consumption_government",
  "A", "NE.GDI.FTOT.KD.ZG",       "level", "investment",
  "A", "NV.IND.TOTL.KD.ZG",       "level", "va_industry",
  "A", "NV.SRV.TOTL.KD.ZG",       "level", "va_services",
  "A", "NV.AGR.TOTL.KD.ZG",       "level", "va_agriculture",
  "A", "NY.GNS.ICTR.ZS",          "pp",    "savings",
  "A", "NE.RSB.GNFS.ZS",          "pp",    "external_balance",
  "A", "NY.GNP.PCAP.CD",          "pct",   "income_usd",
  "A", "NV.IND.TOTL.ZS",          "pp",    "economic_structure",
  "A", "NE.CON.GOVT.ZS",          "pp",    "government_size",
  "A", "NY.GDP.MKTP.CD",          "pct",   "income_usd",
  "A", "NV.IND.MANF.ZS",          "pp",    "economic_structure",
  "A", "NE.GDI.FTOT.ZS",          "pp",    "investment_ratio",
  "A", "NE.CON.PRVT.ZS",          "pp",    "consumption_private",
  "A", "BN.GSR.FCTY.CD",          "level", "primary_income",
  "A", "NV.SRV.TOTL.ZS",          "pp",    "economic_structure",
  "A", "NY.GDP.PCAP.KD.ZG",       "level", "output",
  "A", "NY.GNP.MKTP.KD.ZG",       "level", "output",
  "A", "NV.IND.MANF.KD.ZG",       "level", "va_industry",
  "A", "NE.GDI.TOTL.KD.ZG",       "level", "investment",
  # ---- B: Financial & Monetary --------------------------------------------
  "B", "FP.CPI.TOTL.ZG",          "level", "inflation",
  "B", "PA.NUS.FCRF",             "pct",   "exchange_rate",
  "B", "FS.AST.PRVT.GD.ZS",       "pp",    "credit_private",
  "B", "FI.RES.TOTL.CD",          "pct",   "reserves",
  "B", "FS.AST.CGOV.GD.ZS",       "pp",    "credit_government",
  "B", "CM.MKT.LCAP.GD.ZS",       "pp",    "equity_market",
  "B", "FB.AST.NPER.ZS",          "pp",    "bank_soundness",
  "B", "FB.CBK.BRWR.P3",          "pct",   "financial_inclusion",
  "B", "DT.DOD.DSTC.IR.ZS",       "pp",    "external_liquidity",
  "B", "FM.AST.DOMS.CN",          "pct",   "credit_domestic",
  "B", "FD.AST.PRVT.GD.ZS",       "pp",    "credit_private",
  "B", "FM.AST.PRVT.ZG.M3",       "level", "credit_private",
  "B", "FM.AST.NFRG.CN",          "pct",   "reserves",
  "B", "FI.RES.TOTL.MO",          "pp",    "reserves",
  "B", "CM.MKT.TRAD.GD.ZS",       "pp",    "equity_market",
  "B", "FB.BNK.CAPA.ZS",          "pp",    "bank_soundness",
  "B", "FB.CBK.DPTR.P3",          "pct",   "financial_inclusion",
  "B", "NY.GDP.DEFL.KD.ZG",       "level", "inflation",
  # ---- C: Trade, External & Private Sector --------------------------------
  "C", "TX.VAL.MRCH.CD.WT",       "pct",   "exports_value",
  "C", "TM.VAL.MRCH.CD.WT",       "pct",   "imports_value",
  "C", "TT.PRI.MRCH.XD.WD",       "pct",   "terms_of_trade",
  "C", "BN.CAB.XOKA.GD.ZS",       "pp",    "current_account",
  "C", "BX.KLT.DINV.WD.GD.ZS",    "pp",    "fdi",
  "C", "BX.TRF.PWKR.CD.DT",       "pct",   "remittances",
  "C", "TX.VAL.SERV.CD.WT",       "pct",   "services_trade",
  "C", "TX.VAL.MMTL.ZS.UN",       "pp",    "export_composition",
  "C", "DT.DOD.DECT.GN.ZS",       "pp",    "external_debt",
  "C", "IC.BUS.NDNS.ZS",          "pct",   "business_formation",
  "C", "TG.VAL.TOTL.GD.ZS",       "pp",    "trade_openness",
  "C", "TX.UVI.MRCH.XD.WD",       "pct",   "terms_of_trade",
  "C", "TX.QTY.MRCH.XD.WD",       "pct",   "exports_real",
  "C", "TM.QTY.MRCH.XD.WD",       "pct",   "imports_real",
  "C", "TM.VAL.SERV.CD.WT",       "pct",   "services_trade",
  "C", "TX.VAL.FOOD.ZS.UN",       "pp",    "export_composition",
  "C", "DT.TDS.DECT.EX.ZS",       "pp",    "external_debt",
  # ---- D: Labor Market & Social Protection --------------------------------
  "D", "SL.UEM.TOTL.NE.ZS",       "pp",    "unemployment",
  "D", "SL.TLF.CACT.NE.ZS",       "pp",    "participation",
  "D", "SL.EMP.VULN.ZS",          "pp",    "informality",
  "D", "SL.AGR.EMPL.ZS",          "pp",    "employment_structure",
  "D", "SL.GDP.PCAP.EM.KD",       "pct",   "productivity",
  "D", "SL.UEM.NEET.ME.ZS",       "pp",    "youth",
  "D", "SL.TLF.TOTL.IN",          "pct",   "labor_force_size",
  "D", "SL.TLF.PART.ZS",          "pp",    "part_time",
  "D", "SL.TLF.CACT.FM.NE.ZS",    "pp",    "gender_gap",
  "D", "SL.EMP.MPYR.ZS",          "pp",    "employers",
  "D", "SM.POP.NETM",             "level", "migration",
  "D", "SL.UEM.TOTL.ZS",          "pp",    "unemployment",
  "D", "SL.UEM.1524.NE.ZS",       "pp",    "unemployment",
  "D", "SL.TLF.CACT.ZS",          "pp",    "participation",
  "D", "SL.EMP.TOTL.SP.NE.ZS",    "pp",    "participation",
  "D", "SL.EMP.WORK.ZS",          "pp",    "informality",
  "D", "SL.SRV.EMPL.ZS",          "pp",    "employment_structure",
  "D", "SL.IND.EMPL.ZS",          "pp",    "employment_structure",
  # ---- E: Poverty & Inequality --------------------------------------------
  "E", "SI.POV.UMIC",             "pp",    "poverty_moderate",
  "E", "SI.POV.DDAY",             "pp",    "poverty_extreme",
  "E", "SI.POV.LMIC",             "pp",    "poverty_lower_middle",
  "E", "SI.POV.GINI",             "pp",    "inequality",
  "E", "SI.POV.UMIC.GP",          "pp",    "poverty_depth",
  "E", "SI.DST.FRST.20",          "pp",    "income_share_bottom",
  "E", "SI.DST.10TH.10",          "pp",    "income_share_top",
  "E", "SI.SPR.PGAP",             "pp",    "prosperity_gap",
  "E", "SI.DST.50MD",             "pp",    "poverty_relative",
  "E", "SN.ITK.DEFC.ZS",          "pp",    "food_security",
  "E", "SI.POV.NAHC",             "pp",    "poverty_national",
  "E", "NE.CON.PRVT.PC.KD.ZG",    "level", "consumption_per_capita",
  "E", "SH.XPD.OOPC.CH.ZS",       "pp",    "health_out_of_pocket",
  "E", "SE.XPD.TOTL.GD.ZS",       "pp",    "public_education_spending",
  "E", "SH.XPD.GHED.GD.ZS",       "pp",    "public_health_spending",
  "E", "SI.DST.04TH.20",          "pp",    "income_share_upper_middle",
  "E", "SI.DST.03RD.20",          "pp",    "income_share_middle",
  "E", "SI.POV.SOPO",             "pp",    "poverty_relative",
  "E", "SI.POV.GAPS",             "pp",    "poverty_depth",
  "E", "SI.POV.LMIC.GP",          "pp",    "poverty_depth",
  "E", "SI.DST.FRST.10",          "pp",    "income_share_bottom",
  "E", "SI.DST.05TH.20",          "pp",    "income_share_top"
) %>% mutate(priority = row_number())
stopifnot(!anyDuplicated(pool$code))

# ------------------------------------------------------------
# User overrides (CP2, 2026-11-17)
# ------------------------------------------------------------
# FORCE_INCLUDE: selected at their pool position regardless of the kind /
#                correlation rules (still must be eligible).
# FORCE_EXCLUDE: never selected.
FORCE_INCLUDE <- c("SI.POV.LMIC")   # $4.20/day headcount, kept despite |r|=0.98 with $3.00
FORCE_EXCLUDE <- c("SI.POV.NAHC")   # national poverty line headcount
stopifnot(all(c(FORCE_INCLUDE, FORCE_EXCLUDE) %in% pool$code),
          !length(intersect(FORCE_INCLUDE, FORCE_EXCLUDE)))

# ------------------------------------------------------------
# Coverage + transformed series
# ------------------------------------------------------------
wdi <- read_csv(file.path(processed_path, "wdi_all_long.csv"), show_col_types = FALSE) %>%
  filter(code %in% pool$code)
missing <- setdiff(pool$code, wdi$code)
if (length(missing)) stop("Not in WDI_PER: ", paste(missing, collapse = ", "))

# Transform on consecutive years only (gaps -> NA, never difference across a gap)
transform_target <- function(df, how) {
  df <- arrange(df, year)
  consec <- c(FALSE, diff(df$year) == 1)
  prev   <- dplyr::lag(df$value)
  df$y <- switch(how,
    level = df$value,
    pct   = ifelse(consec & df$value > 0 & prev > 0, 100 * log(df$value / prev), NA_real_),
    pp    = ifelse(consec, df$value - prev, NA_real_))
  df
}
ty <- wdi %>%
  inner_join(pool %>% select(code, transform), by = "code") %>%
  group_by(code) %>%
  group_modify(~ transform_target(.x, .x$transform[1])) %>%
  ungroup()

stats <- wdi %>%
  group_by(code) %>%
  summarise(name = first(name), wdi_group = first(group),
            n_obs = n(), first_year = min(year), last_year = max(year),
            n_train = sum(year %in% TRAIN_YEARS),
            n_test  = sum(year %in% TEST_YEARS),
            test_years = paste(sort(year[year %in% TEST_YEARS]), collapse = " "),
            .groups = "drop")

cand <- pool %>% left_join(stats, by = "code") %>%
  mutate(eligible = n_obs >= MIN_YEARS & last_year >= MIN_LAST_YEAR &
                    n_train >= MIN_TRAIN_OBS)

# Correlation matrix of transformed series over CORR_YEARS
wide <- ty %>% filter(year %in% CORR_YEARS, !is.na(y)) %>%
  select(year, code, y) %>% pivot_wider(names_from = code, values_from = y) %>%
  arrange(year)
ym <- as.matrix(wide[, -1])
cm <- suppressWarnings(cor(ym, use = "pairwise.complete.obs"))
nm <- crossprod(!is.na(ym))

# ------------------------------------------------------------
# Greedy selection
# ------------------------------------------------------------
cand$status <- NA_character_
selected <- character(0)
for (s in c("A", "B", "C", "D", "E")) {
  kinds_taken <- character(0); n_taken <- 0
  for (i in which(cand$sector == s)) {
    cd <- cand$code[i]
    if (cd %in% FORCE_EXCLUDE) { cand$status[i] <- "excluded by user"; next }
    if (n_taken >= N_PER_SECTOR) { cand$status[i] <- "reserve (sector full)"; next }
    if (!cand$eligible[i]) {
      if (cd %in% FORCE_INCLUDE) stop("Forced target is ineligible: ", cd)
      cand$status[i] <- sprintf("ineligible (n=%d, last=%d, train=%d)",
                                cand$n_obs[i], cand$last_year[i], cand$n_train[i]); next
    }
    if (cd %in% FORCE_INCLUDE) {
      cand$status[i] <- "SELECTED"
      selected <- c(selected, cd); kinds_taken <- c(kinds_taken, cand$kind[i])
      n_taken <- n_taken + 1; next
    }
    if (cand$kind[i] %in% kinds_taken) {
      cand$status[i] <- paste0("same kind as selected: ", cand$kind[i]); next
    }
    if (length(selected) && cd %in% colnames(cm)) {
      others <- intersect(selected, colnames(cm))
      r  <- abs(cm[cd, others]); ok <- nm[cd, others] >= CORR_MIN_N & !is.na(r)
      if (any(ok & r > CORR_MAX)) {
        j <- which(ok & r > CORR_MAX)[which.max(r[ok & r > CORR_MAX])]
        cand$status[i] <- sprintf("|r|=%.2f with %s", r[j], others[j]); next
      }
    }
    cand$status[i] <- "SELECTED"
    selected <- c(selected, cd); kinds_taken <- c(kinds_taken, cand$kind[i])
    n_taken <- n_taken + 1
  }
}

targets <- cand %>% filter(status == "SELECTED")
if (nrow(targets) != 5 * N_PER_SECTOR) {
  options(width = 220)
  print(cand %>% select(sector, code, kind, status), n = Inf)
  print(table(targets$sector))
  stop("A sector has fewer than ", N_PER_SECTOR, " eligible, distinct candidates (see log above).")
}
stopifnot(nrow(targets) == 5 * N_PER_SECTOR,
          all(table(targets$sector) == N_PER_SECTOR),
          !anyDuplicated(targets$code))

# Max |corr| of each selected target with any other selected target
sel_cm <- abs(cm[intersect(targets$code, colnames(cm)), intersect(targets$code, colnames(cm))])
diag(sel_cm) <- NA
targets <- targets %>%
  mutate(max_abs_corr = apply(sel_cm, 1, max, na.rm = TRUE)[code],
         max_corr_with = colnames(sel_cm)[apply(sel_cm, 1, which.max)][match(code, rownames(sel_cm))])

out <- targets %>%
  select(sector, code, name, kind, transform, wdi_group, n_obs, first_year, last_year,
         n_train, n_test, test_years, max_abs_corr, max_corr_with)
write_csv(out, file.path(processed_path, "sector_targets.csv"))
write_csv(cand %>% select(sector, priority, code, name, kind, transform, wdi_group,
                          n_obs, first_year, last_year, n_train, status),
          file.path(processed_path, "sector_target_candidates.csv"))
write_csv(as_tibble(sel_cm, rownames = "code"),
          file.path(processed_path, "sector_target_corr.csv"))

options(width = 220)
message("\n==== Final 50 targets ====")
print(out %>% mutate(name = str_trunc(name, 55)) %>%
        select(sector, code, name, transform, wdi_group, first_year, last_year, n_train, max_abs_corr),
      n = Inf)
message("\n==== Candidates not selected ====")
print(cand %>% filter(status != "SELECTED") %>% select(sector, code, kind, status), n = Inf)
