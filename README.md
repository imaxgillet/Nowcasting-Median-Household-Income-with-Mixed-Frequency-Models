# Nowcasting median household income with mixed-frequency models

Replication code for [this article](https://www.chicagofed.org/publications/economic-perspectives/2024/7).

# Replication steps

## Full backtest
Run the full backtest to create a forecast of median household income for each model, for each number of factors, on each data release date. These forecasts were created for every release date between 2012 and 2023, inclusive.
1. Download the data using `code/data_download/get_income_dfm_data.R` with the `get_backtest_data` flag as TRUE.
1. Install the [MFSS package](https://github.com/davidakelley/MFSS) as a MATLAB Add-on. You may need to re-create the .mltbx file, which requires updating the toolbox.prj and using the Add-Ons > Package Toolbox dialogue.
1. Run `code/estimate_all.m` to estimate the full backtest of all models. This may take a few days!

## Current forecast
Run the "current forecast" to get the preferred model's estimated model and forecast based on the most up-to-date data. In the article, this uses data as of 2024-07-31. 
1. Download the data using `code/data_download/get_income_dfm_data.R` with the `get_backtest_data` flag as FALSE.
1. Run `code/estimate_preferred_model.m` to estimate just the preferred model, on one date.

## Forecast with updated data
Run the "forecast" to get a forecast using a previously-estimated model, but with up-to-date data. In the article, this uses a model estimated on 2024-07-31 data, but input data from 2024-09-17. 
1. Download the data using `code/data_download/get_income_dfm_data.R` with the `get_backtest_data` flag as FALSE.
1. Point `code/run_updated_data.m` to the estimated model, saved at `output/yyyyMMdd_HHmmss_run/workspace.mat`.
1. Run `code/run_updated_data.m`.

## Lag selection
1. Download the data using `code/data_download/get_income_dfm_data.R` with the `get_backtest_data` flag as FALSE.
1. Run `code/find_bic_at_each_lag.m` to estimate all models with lag orders from 1 to 6.

## Plot creation
Once the previous parts have all been completed, there are some additional data collection steps before creating the figures from the article:

1. There are R scripts to run various data collection tasks in the `code/analysis` folder.
    1. You need to create an IPUMS account and get an API key.
    1. `get_insurance_and_tax_data.R` and `impute_cps_median_income.R` will request this key as input.
    1. `get_zillow_data.R` and `get_freddie_data.R` can be run without any additional input, as long as the Zillow and Freddie Mac websites stay the same.


# Author
Max Gillet, 2024