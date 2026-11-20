# Mass Nowcasting (Peru Case Study)

Nowcasting 50 of Peru's annual World Development Indicators (WDI) from quarterly
high-frequency data, comparing three LSTM designs with a bridge-equation benchmark.

The targets are 10 indicators in each of five areas: macro, financial, trade and external,
labor, and poverty. The predictors are quarterly Haver Analytics series.
The LSTM designs are:

| Design | Models | Targets per model | Inputs |
|---|---|---|---|
| Pooled | 1 | 50 | union of the five sector predictor sets (25 series) |
| Sector | 5 | 10 | that sector's predictor set |
| Single | 50 | 1 | the target's sector predictor set |

The benchmarks are a LASSO (and OLS) bridge equation per indicator on the same predictors,
a naive forecast (repeat last year's value) and the indicator's historical average.

## Main results

Average error relative to the naive forecast across the 50 indicators
(geometric mean; lower is better, below 1 beats the naive forecast):

| Model | RMSE 2020–25 | RMSE 2022–25 | MAE 2020–25 | MAE 2022–25 |
|---|---|---|---|---|
| LSTM: one pooled model | **0.74** | **0.62** | **0.71** | **0.62** |
| LSTM: one per sector | 0.74 | 0.63 | 0.72 | 0.65 |
| LSTM: one per indicator | 0.74 | 0.63 | 0.72 | 0.65 |
| Bridge equation (LASSO) | 1.00 | 1.13 | 0.96 | 1.05 |
| Historical average | 0.75 | 0.61 | 0.73 | 0.62 |

- The one pooled LSTM is as accurate as 5 sector models or 50 single-indicator models.
- It beats the bridge equation in all five sectors in both test periods.
- Caveat: a historical-average forecast does about as well as the LSTMs, so much of their
  gain over the naive forecast is mean reversion. The test window is short (2020–25) and
  dominated by COVID and the 2022 rebound.

Figures are in [`3_outputs/blog/`](3_outputs/blog/).

## Repository layout

```
1_data/
  raw/                Haver_Data.xlsx, WDI_PER.xlsx, WDI_Nowcasting_Groups.xlsx
  haver_series_map.csv  Haver series -> sector, publication lag, transform
  processed/          filtered and YoY Haver series, WDI long table, target selection files
2_code/               R scripts, run in numerical order
3_outputs/
  sector_lstm/        predictor selection, LSTM predictions per design, checkpoints, run logs
  bridge/             LASSO / OLS bridge predictions and coefficients
  blog/               comparison metrics, nowcasts and figures
  regression_check/   check that the shared functions reproduce the earlier Macro package
```

## Scripts

| Script | Step |
|---|---|
| `0_functions.R` | Shared library: data reading, transforms, publication lags, bridge features, LSTM sequences and model, walk-forward evaluation |
| `0_functions_check.r` | Regression check against the earlier 8-target Macro package (needs that package; optional) |
| `1_data_processing.r` | Read Haver data, availability filter, YoY transforms, sector tagging, publication lags; process WDI |
| `2_select_targets.r` | Select 10 WDI targets per sector (coverage and correlation rules) |
| `3_rhs_selection.r` | Choose each sector's predictors (LASSO stability selection on pre-2020 data, stepwise check) |
| `4_sector_lstm.r` | Train and evaluate the pooled, sector and single LSTMs (walk-forward 2020–25, 5 seeds) |
| `5_sector_bridge.r` | LASSO and OLS bridge equations on the same predictors |
| `6_compare_blog.r` | Metrics (RMSE, MAE relative to the naive forecast), nowcasts and figures |

## Reproducing

1. **Data.** All input data are included in `1_data/`: the Haver export
   (`1_data/raw/Haver_Data.xlsx`, sheet `Data`, quarterly Peru `@emergela` series) and WDI.
2. **R 4.6** with `readxl`, `dplyr`, `tidyr`, `stringr`, `readr`, `zoo`, `glmnet`, `MASS`,
   `ggplot2`, `reticulate` and `keras3`.
3. **Keras.** `keras3` with TensorFlow in a reticulate virtualenv named `r-keras`
   (`keras3::install_keras(envname = "r-keras")`). CPU is enough.
4. Run the scripts in order from the package root, for example
   `Rscript 2_code/1_data_processing.r`.
   - `4_sector_lstm.r` takes several hours on CPU. Finished models are checkpointed, so a re-run
     resumes. For parallel runs use `--part=k/n` and set `SECTOR_LSTM_THREADS`, for example
     `SECTOR_LSTM_THREADS=2 Rscript 2_code/4_sector_lstm.r single --part=1/3`.
   - `Rscript 2_code/4_sector_lstm.r --quick` is a fast smoke test.

## Data sources

- World Bank, World Development Indicators (CC BY 4.0).
- Haver Analytics, Emerging Latin America database. Haver data are subject to Haver Analytics' terms of use.
