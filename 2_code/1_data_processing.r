# ============================================================
# 1_data_processing.r — Haver availability filter, dedup, sector map; full WDI
# ============================================================
# Inputs  (1_data/raw):  Haver_Data.xlsx (quarterly), WDI_PER.xlsx,
#                        WDI_Nowcasting_Groups.xlsx
# Outputs (1_data/processed unless noted):
#   haver_catalog.csv        one row per Haver series: metadata, coverage,
#                            drop_stage / drop_reason (NA = kept)
#   haver_filter_funnel.csv  series count after each filter
#   haver_filtered_q.csv     quarterly LEVELS of kept series (date x code)
#   haver_yoy_q.csv          quarterly YoY-transformed kept series
#   1_data/haver_series_map.csv  code, short_name, desc, sector, pub_lag_q,
#                            transform, keep   (user-editable; if it exists,
#                            user edits to sector/pub_lag_q/transform/keep are
#                            preserved on re-run)
#   wdi_all_long.csv         full WDI_PER in long form + group tags
#   wdi_coverage.csv         per WDI series: group, n_years, first/last year
# ============================================================

script_dir <- local({
  a <- grep("^--file=", commandArgs(FALSE), value = TRUE)
  if (length(a)) dirname(normalizePath(sub("^--file=", "", a))) else
    tryCatch(dirname(rstudioapi::getSourceEditorContext()$path), error = function(e) "2_code")
})
source(file.path(script_dir, "0_functions.R"))

project_root   <- get_project_root()
raw_path       <- file.path(project_root, "1_data", "raw")
processed_path <- file.path(project_root, "1_data", "processed")
map_path       <- file.path(project_root, "1_data", "haver_series_map.csv")

# ---- Filter parameters (agreed with user, 2026-11-17) -------
MIN_LAST_YEAR   <- 2025   # series must have data through 2025+
MAX_FIRST_YEAR  <- 2010   # series must start by 2010 (loosened from 2005 at CP1)
MAX_GAP_SHARE   <- 0.05   # < 5% missing between first and last obs
CORR_WINDOW     <- c(2005, 2019)  # dedup correlation sample (pre-test window)
CORR_THRESHOLD  <- 0.95
CORR_MIN_OBS    <- 30     # quarters of overlap needed to judge a pair
RUN_DEDUP       <- FALSE  # TRUE = also apply rule-based + correlation dedup (945 -> 213)

rx <- function(p) regex(p, ignore_case = TRUE)

# ============================================================
# 1. Haver: parse + coverage
# ============================================================
hv  <- read_haver_quarterly(file.path(raw_path, "Haver_Data.xlsx"))
cov <- series_coverage(hv$values, hv$periods)
stopifnot(!anyDuplicated(hv$meta$code))

catalog <- hv$meta %>%
  select(code, desc, frq, data_type, agg, lsource) %>%
  left_join(cov, by = "code") %>%
  mutate(drop_stage = NA_character_, drop_reason = NA_character_)

funnel <- tibble(stage = "0_raw", n_kept = nrow(catalog))
drop_if <- function(df, cond, stage, reason) {
  cond <- cond & is.na(df$drop_stage)
  cond[is.na(cond)] <- FALSE
  df$drop_stage[cond]  <- stage
  df$drop_reason[cond] <- if (length(reason) > 1) reason[cond] else reason
  df
}
log_stage <- function(stage) {
  n <- sum(is.na(catalog$drop_stage))
  funnel <<- bind_rows(funnel, tibble(stage = stage, n_kept = n))
  message(sprintf("%-28s %5d series kept", stage, n))
}

# ============================================================
# 2. Availability filter
# ============================================================
catalog <- drop_if(catalog, catalog$n_obs == 0, "1_all_na", "no observations"); log_stage("1_all_na")
catalog <- drop_if(catalog, str_detect(catalog$desc, regex("weight", ignore_case = TRUE)),
               "2_weights", "CPI/WPI weight");                      log_stage("2_weights")
catalog <- drop_if(catalog, catalog$last_year < MIN_LAST_YEAR, "3_ends_early",
               paste0("last obs before ", MIN_LAST_YEAR));           log_stage("3_ends_early")
catalog <- drop_if(catalog, catalog$first_year > MAX_FIRST_YEAR, "4_starts_late",
               paste0("first obs after ", MAX_FIRST_YEAR));          log_stage("4_starts_late")
catalog <- drop_if(catalog, catalog$gap_share >= MAX_GAP_SHARE, "5_gaps",
               "internal gaps >= 5%");                               log_stage("5_availability_done")

n_avail <- sum(is.na(catalog$drop_stage))
stopifnot(n_avail > 1100, n_avail < 1250)   # expected ~1,163 with start <= 2010

# ============================================================
# 3. Rule-based dedup / pre-screen
# ============================================================
if (RUN_DEDUP) {   # switched off 2026-11-17: user keeps all 945 available series
d <- catalog$desc
n_colon <- str_count(d, ":")

# 3a. Bilateral trade by partner (keep China, United States)
catalog <- drop_if(catalog,
  str_detect(d, "^Peru: (Exports to|Imports from) ") &
    !str_detect(d, "China|United States"),
  "6a_bilateral", "partner-country trade detail")
log_stage("6a_bilateral")

# 3b. CPI / WPI sub-items (keep headline, core, food, energy aggregates)
is_price_sub <- str_detect(d, "(CPI|Consumer Price Index|WPI|Wholesale Price Index)[^:]*:")
keep_price   <- str_detect(d, regex(paste0(
  ":\\s*(Core|Food and Bev|Food & Bev|Food\\b|Energy|Fuel|Underlying|",
  "Tradable|Non-?tradable|Excl|Ex[ .])"), ignore_case = TRUE)) & n_colon <= 2
catalog <- drop_if(catalog, is_price_sub & !keep_price, "6b_price_subitem", "CPI/WPI sub-item")
log_stage("6b_price_subitem")

# 3c. Product-line detail (keep Peru's main commodities)
main_comm <- "Copper|Gold|Zinc|Iron|Fishmeal|Petroleum"
catalog <- drop_if(catalog,
  (str_detect(d, "^Peru: Exports, ")                    & !str_detect(d, main_comm)) |
  (str_detect(d, "^Peru: Export (Volume|Price), ")      & !str_detect(d, main_comm)) |
   str_detect(d, "Imports: Foodstuff, "),
  "6c_product_detail", "product-line detail")
log_stage("6c_product_detail")

# 3d. Deep sub-components of debt / BOP / fiscal / money (>= 3 colon levels),
#     and anything >= 4 levels deep
fam_deep <- str_detect(d, regex(paste0(
  "Pub(lic)? D(e)?bt|Public Debt|Pub Debt|BOP|Balance of Payments|",
  "Cen(tral)? Gov|Cent Govt|Cen Gvt|Gen(eral)? Gov|Mon Agg|Monetary Aggregates|Int Liq"),
  ignore_case = TRUE))
catalog <- drop_if(catalog, (fam_deep & n_colon >= 3) | n_colon >= 4,
               "6d_deep_subcomponent", "sub-component >= 3 levels deep")
log_stage("6d_deep_subcomponent")

# 3e. FX ask/bid quotes (keep mid)
catalog <- drop_if(catalog, str_detect(d, "Exchange Rate \\[(Ask|Bid)\\]"),
               "6e_fx_quote", "FX ask/bid quote")
# 3f. Contributions to q/q growth (%Pt; duplicate the component series)
catalog <- drop_if(catalog, str_detect(d, "Contrib(ution)? to Q/Q"),
               "6f_contribution", "contribution to q/q growth")
log_stage("6ef_fx_contrib")

# 3g. Nominal / real twins: same concept in current soles and constant soles
#     -> keep the real (constant-price) version
unit_pat <- "Mil\\.(2007\\.|Dec2021\\.)?(New)?Soles"
stem <- str_replace(d, unit_pat, "<UNIT>")
is_nominal <- str_detect(d, "Mil\\.(New)?Soles")
is_real    <- str_detect(d, "Mil\\.(2007|Dec2021)\\.(New)?Soles")
alive <- is.na(catalog$drop_stage)
real_stems <- unique(stem[is_real & alive])
catalog <- drop_if(catalog, is_nominal & stem %in% real_stems,
               "6g_nominal_twin", "nominal twin of a constant-price series")
log_stage("6g_nominal_twin")

# 3h. Public debt / securities: keep only the totals (level and % of GDP);
#     detail lines are written with commas, so the colon rule misses them
debt_family <- str_detect(d, rx("Pub(lic)? D(e)?bt|Pub Dbt|Public Sector Securities|Government .*Debt"))
debt_core <- d |>
  str_remove("^Peru: ") |>
  str_remove("\\s*\\(.*$") |>
  str_remove("\\s+as a (%|Percent) of.*$") |>
  str_squish()
debt_totals <- c("Public Debt", "Domestic Public Debt", "Dom Public Debt",
                 "Foreign Public Debt", "Net Public Debt", "Federal Government Gross Debt",
                 "Public Sector Securities")
catalog <- drop_if(catalog, debt_family & !(debt_core %in% debt_totals),
                   "6h_debt_detail", "public debt / securities detail")
log_stage("6h_debt_detail")

# 3i. End-of-period vs period-average twins of the same series -> keep AVG
eop_stem <- d |> str_remove_all("\\b(EOP|AVG)\\b") |> str_replace_all("[ ,]+", " ") |> str_squish()
alive <- is.na(catalog$drop_stage)
avg_stems <- unique(eop_stem[alive & str_detect(d, "\\bAVG\\b")])
catalog <- drop_if(catalog, str_detect(d, "\\bEOP\\b") & eop_stem %in% avg_stems,
                   "6i_eop_twin", "EOP twin of a period-average series")
log_stage("6i_eop_twin")
} # end RUN_DEDUP (3)

# ============================================================
# 4. Transforms (YoY) for survivors, then correlation dedup
# ============================================================
alive_codes <- catalog$code[is.na(catalog$drop_stage)]
meta_alive  <- catalog %>% filter(code %in% alive_codes)

transforms <- vapply(seq_len(nrow(meta_alive)), function(i) {
  default_transform(meta_alive$data_type[i], meta_alive$desc[i],
                    hv$values[meta_alive$code[i], ])
}, character(1))
names(transforms) <- meta_alive$code

yoy <- sapply(alive_codes, function(cd) yoy_transform(hv$values[cd, ], transforms[[cd]]))
rownames(yoy) <- hv$periods

if (RUN_DEDUP) {
# 4a. Correlation dedup on YoY over CORR_WINDOW; greedy, longest history first
yr_idx <- as.integer(substr(hv$periods, 1, 4))
win    <- yr_idx >= CORR_WINDOW[1] & yr_idx <= CORR_WINDOW[2]
yw     <- yoy[win, , drop = FALSE]
cm     <- suppressWarnings(cor(yw, use = "pairwise.complete.obs"))
nm     <- crossprod(!is.na(yw))                  # pairwise overlap counts

# Tie-break priority: (1) protected headline indicators, (2) monthly-origin
# series (published sooner than native quarterly), (3) longer history.
protected <- c("N293RTAR@emergela",   # BCRP reference (policy) rate
               "N293DVM@emergela",    # manufacturing industrial production index
               "N293DVE@emergela",    # electricity production
               "N293PC@emergela",     # Lima CPI (headline)
               "N293IXD@emergela",    # merchandise exports
               "N293IMD@emergela")    # merchandise imports
ord <- meta_alive %>%
  mutate(prio = case_when(code %in% protected ~ 0L,
                          str_detect(frq, "Monthly") ~ 1L,
                          TRUE ~ 2L)) %>%
  arrange(prio, desc(n_obs), first_year, code) %>% pull(code)
kept <- character(0); dup_of <- setNames(rep(NA_character_, length(ord)), ord)
for (cd in ord) {
  if (length(kept)) {
    r  <- abs(cm[cd, kept]); ok <- nm[cd, kept] >= CORR_MIN_OBS
    hit <- which(ok & !is.na(r) & r > CORR_THRESHOLD)
    if (length(hit)) { dup_of[cd] <- kept[hit[which.max(r[hit])]]; next }
  }
  kept <- c(kept, cd)
}
dups <- names(dup_of)[!is.na(dup_of)]
catalog <- drop_if(catalog, catalog$code %in% dups, "7_near_duplicate",
               paste0("|r|>", CORR_THRESHOLD, " with ", dup_of[catalog$code]))
log_stage("7_near_duplicate")
} # end RUN_DEDUP (4a)

# ============================================================
# 5. Sector tagging + publication lags  ->  haver_series_map.csv
# ============================================================
final <- catalog %>% filter(is.na(drop_stage))

pat <- list(
  A = paste0("GDP|Gross Domestic Product|Domestic Demand|Absorption|Gross National|",
             "Mining Production|Electricity Production|Industrial Production|^Peru: IP|",
             "Manufactur|Cement|Productivity|Gross Value added|Cen(tral)? Gov|Cent Govt|",
             "Cen Gvt|Gen(eral)? Gov|\\bGG:|GGovt|Tax|Business Conf|Bus Exp|Business Exp"),
  B = paste0("Monetary|Mon Agg|Money|Credit|Deposit|Interest Rate|Reference Rate|",
             "Rediscount|Yield|Exchange Rate|Int Liq|Reserves|Consumer Price|CPI|WPI|",
             "Wholesale Price|Inflation Expectation|\\bCr of|",
             "\\bStock\\b|MSCI|Share Price|Market Capitalization|Dollarization|Debt|Dbt|",
             "Securities|Housing Prices|Interbank|BCRP FX|Foreign Exchange Trans|FX Trans"),
  C = paste0("Export|Import|Real Imp|Trade Balance|BOP|Balance of Payments|Terms of Trade|",
             "Nominal Prices|Remittance|Direct Investment|Business Conf|Bus Exp|Business Exp|Sales"),
  D = paste0("Employ|Unemploy|Labor|Labour|Wage|Remuneration|Salar|Econ Active|",
             "Economically Active|Working Age|Productivity|Pension|Monthly Income")
)
pat$E <- paste0(pat$D, "|Consumer Price Index|Lima CPI|National CPI|Private Consumption|",
                "Total Consumption|Minimum Wage|Remittance|Transfers|Disposable|",
                "Gross National Income|Real Gross Domestic Product|Agricult|Farming|",
                "Housing|Food")
global_pat <- "Terms of Trade|Nominal Prices: (Exports|Imports)|Export Price, (Copper|Gold)"

tags <- sapply(names(pat), function(s) str_detect(final$desc, rx(pat[[s]])))
tags[str_detect(final$desc, rx(global_pat)), ] <- TRUE
sector_str <- apply(tags, 1, function(r) paste(names(pat)[r], collapse = ";"))

# Lag (quarters after the reference quarter at which the value is usable at a
# quarter-end cutoff): 0 = market prices / CPI known by quarter end; 1 = default.
lag0 <- str_detect(final$desc, rx(paste0(
  "Interest Rate|Reference Rate|Rediscount|Yield|Exchange Rate|\\bStock\\b|MSCI|",
  "Share Price|Market Capitalization|Consumer Price|CPI|WPI|Wholesale Price"))) &
  !str_detect(final$desc, rx("GDP|Gross Domestic Product"))

map_new <- final %>%
  transmute(code,
            short_name = make_short_names(desc),
            desc,
            sector     = sector_str,
            pub_lag_q  = ifelse(lag0, 0L, 1L),
            transform  = unname(transforms[code]),
            keep       = TRUE,
            frq, data_type, first_year, n_obs)

# Preserve user edits if the map already exists
if (file.exists(map_path)) {
  old <- read_csv(map_path, show_col_types = FALSE) %>%
    select(code, sector, pub_lag_q, transform, keep)
  map_new <- map_new %>%
    rows_update(old %>% filter(code %in% map_new$code), by = "code")
  message("Existing haver_series_map.csv found: user edits preserved for ",
          sum(old$code %in% map_new$code), " series")
}
write_csv(map_new, map_path, na = "")

# ============================================================
# 6. Write Haver outputs
# ============================================================
keep_codes <- map_new$code
levels_df <- as_tibble(t(hv$values[keep_codes, , drop = FALSE])) %>%
  mutate(date = hv$dates, period = hv$periods, .before = 1)
yoy_df <- as_tibble(yoy[, keep_codes, drop = FALSE]) %>%
  mutate(date = hv$dates, period = hv$periods, .before = 1)

write_csv(levels_df, file.path(processed_path, "haver_filtered_q.csv"), na = "")
write_csv(yoy_df,    file.path(processed_path, "haver_yoy_q.csv"),      na = "")
write_csv(catalog,       file.path(processed_path, "haver_catalog.csv"),    na = "")
write_csv(funnel,    file.path(processed_path, "haver_filter_funnel.csv"))

# ============================================================
# 7. WDI: full WDI_PER in long form + nowcasting group tags
# ============================================================
wdi_raw <- read_excel(file.path(raw_path, "WDI_PER.xlsx"), sheet = "Data")
year_cols <- names(wdi_raw)[str_detect(names(wdi_raw), "\\[YR[0-9]{4}\\]")]

groups <- read_excel(file.path(raw_path, "WDI_Nowcasting_Groups.xlsx"),
                     sheet = "All Indicators") %>%
  transmute(code = `WDI Series Code`, group = Group,
            nowcastability = str_remove(Nowcastability, "^[^A-Za-z]+"),
            wdi_topic = `WDI Topic`)

wdi_long <- wdi_raw %>%
  filter(!is.na(`Series Code`)) %>%
  select(code = `Series Code`, name = `Series Name`, all_of(year_cols)) %>%
  mutate(across(all_of(year_cols), ~ suppressWarnings(as.numeric(as.character(.x))))) %>%
  pivot_longer(all_of(year_cols), names_to = "year", values_to = "value") %>%
  mutate(year = as.integer(str_extract(year, "[0-9]{4}"))) %>%
  filter(!is.na(value)) %>%
  left_join(groups, by = "code")

wdi_cov <- wdi_long %>%
  group_by(code, name, group, nowcastability) %>%
  summarise(n_years = n(), first_year = min(year), last_year = max(year),
            n_pre2020 = sum(year < 2020), .groups = "drop") %>%
  arrange(group, desc(n_years))

write_csv(wdi_long, file.path(processed_path, "wdi_all_long.csv"), na = "")
write_csv(wdi_cov,  file.path(processed_path, "wdi_coverage.csv"), na = "")

# ============================================================
# 8. Checkpoint summary (CP1)
# ============================================================
message("\n==== Haver filter funnel ====")
print(funnel, n = Inf)
message("\n==== Drops by reason ====")
print(count(catalog, drop_stage), n = Inf)
message("\n==== Sector pool sizes (series may be in several pools) ====")
print(colSums(tags))
message("Untagged series: ", sum(sector_str == ""))
message("\n==== Transforms / lags ====")
print(table(map_new$transform)); print(table(map_new$pub_lag_q))
message("\n==== WDI coverage: groups A-E, >=20 years (>=15 for E) ====")
print(wdi_cov %>% filter(group %in% LETTERS[1:5]) %>%
        group_by(group) %>%
        summarise(n_in_wdi = n(), n_20y = sum(n_years >= 20), n_15y = sum(n_years >= 15)))

# Sanity asserts
stopifnot(!any(str_detect(map_new$desc, rx("weight"))),
          all(catalog$last_year[is.na(catalog$drop_stage)] >= MIN_LAST_YEAR),
          all(catalog$first_year[is.na(catalog$drop_stage)] <= MAX_FIRST_YEAR),
          ncol(yoy_df) - 2 == nrow(map_new))
