# create_plots.R ----
# Code to take in the MATLAB results and produce relevant plots and tables
# Max Gillet, 2024 (Chicago Fed)
debugSource("plot_functions.R")

library(data.table)
library(lubridate)
library(ggplot2)
library(knitr)
library(tidyr)
library(xtable)
library(forecast)
library(alfred)
library(keyring)
library(ggpubr)

# Set folders to read from ----
# this path should have:
# the output of cps_hhi.csv of impute_cps_median_income.R
# the output lagBIC.csv of find_bic_at_each_lag.m

output_path <- file.path("..", "..","output")
dir.create(output_path) 


input_path <- file.path("..", "..")

# Folder with backtest results
# from estimate_all.m
backtest_dir <- "20240418_094441_backtest"

# Folder with current forecast model
# from estimate_preferred_model.m
current_test_dir <- "20240731_162027_run"

# result from running run_updated_data.m on old model w new input data
updated_test_dir <- "updateFcast_20240917"

path_to_cps <- file.path(output_path, "cps_hhi.csv")
  
path_to_save_output <- "output_plots_tables"
dir.create(file.path(output_path, path_to_save_output))

oos_year <- 2023 # year that is considered "out of sample"
set2_palette <- RColorBrewer::brewer.pal(name = "Set2", n = 8)

# Set fred key ----
tryCatch(
  fred_key <<- keyring::key_get("fred_api"),
  error = function(e) {
    keyring::key_set("fred_api", prompt = "Type your FRED API key:")
    fred_key <<- keyring::key_get("fred_api")
    
  }
)

# Read in input dataspec ----
dataspec_inputname <- file.path(input_path, "input_data.csv")
dataspec_outputname <- file.path(output_path, path_to_save_output, "data_spec.tex")

# Create Table 2: Data series used
create_dataspec_table(dataspec_inputname, dataspec_outputname)

# ASEC vs. ACS ----
asec_acs_outputname <- file.path(output_path, path_to_save_output, "hhi_comp_plot.png")

# Figure 2: Comparison of ASEC and ACS Measures of Median Household Income
asec_acs_plot(fred_key, asec_acs_outputname)

# All backtesting plots ----
backtest_directory <- file.path(output_path,backtest_dir)
cps_filepath <- file.path(output_path, "cps_hhi.csv")
bic_filepath <- file.path(output_path, "lagBIC.csv")
full_output_path <- file.path(output_path, path_to_save_output)

create_backtesting_results(backtest_directory,
                           cps_filepath,
                           bic_filepath,
                           full_output_path,
                           oos_year)

# creates figures:
# Figure 3: Time series of forecasts and realizations
# Figure 4: Error in forecasting models
# Figure 5: Time series of errors
# Figure 6: Estimation time of given models

# Figure A2: BIC at each lag for each model
# Figure A3: HQIC at each lag for each model

# Examples leveraging the forecast ----
# naive interpolation of ASEC data

# Get current median income forecast ----
full_estimated_income_growth <- fread(file.path(output_path, current_test_dir, "output.csv"))
full_estimated_income_growth[, dataDates := dmy(dataDates)]
full_estimated_income_growth[, incomeEstimateFactor := exp(incomeEstimates)]
full_estimated_income_growth[, date := dataDates + days(1) - years(1)] # date for projection - last year

estimated_income_growth <- full_estimated_income_growth[month(dataDates) == 12]

# updated w/ run_updated_data.m
updated_income_growth <- fread(file.path(output_path, updated_test_dir, "updateOutput.csv"))
updated_income_growth[, dataDates := dmy(dataDates)]
updated_income_growth[, incomeEstimateFactor := exp(incomeEstimates)]
updated_income_growth[, date := dataDates + days(1) - years(1)] # date for projection - last year

updated_income_growth <- updated_income_growth[month(dataDates) == 12]


# Home Affordability Example ----
# requires get_insurance_and_tax_data.R, get_zillow_data.R, get_freddie_data.R

# Get home prices
zhvi_filename <- file.path(".", "data", "midtier_zhvi.csv")
home_prices <- fread(zhvi_filename)
home_prices <- home_prices[RegionName == "United States"]
home_prices <- data.table(date = colnames(home_prices), zhvi = as.numeric(home_prices[1, ]))
home_prices <- home_prices[!(date %in% c(
  "RegionID",
  "SizeRank",
  "RegionName",
  "RegionType",
  "StateName"
))]
home_prices[, date := as.Date(date)]

# Get interest rates
pmi_filename <- file.path(".","data", "freddie_pmrate.csv")
interest_rates <- read.csv(pmi_filename) # fread doesnt get all rows
setDT(interest_rates)
interest_rates[, date := mdy(date)]
interest_rates[, year_month := as.Date(strftime(date, "%Y-%m-01")) %m+% months(1) %m-% days(1)]
avg_rate <- interest_rates[, list(avg_rate = mean(pmms30, na.rm = T)), by = "year_month"]
avg_rate[, A := 1.0 / (1.0 + (avg_rate / 100) / 12.0)]
avg_rate[, A_denom := unlist(lapply(A, function(x) {
  sum(x ^ seq(1, 12 * 30))
}))]

home_payment <- merge(home_prices,
                      avg_rate,
                      by.x = 'date',
                      by.y = 'year_month',
                      all = T)

pct_down <- .2

home_payment[, loan_amount := zhvi * (1 - pct_down)]
home_payment[, principal_interest := (loan_amount) / A_denom]
home_payment[, year := year(date)]

# Assume g-fees
avg_g_fee <- mean(c(55, 56, 54, 56)) / (100 * 100)
home_payment[, pmi := (1.0 / 12.0) * (avg_g_fee * loan_amount)]

# insurance and taxes
insurance_tax_filename <- file.path(".", "data", "autogen_ipums_nationwide_property_ratios.csv")
insurance_taxes <- fread(insurance_tax_filename)
home_payment <- merge(
  home_payment,
  insurance_taxes[, c("year", "median_retax_ratio", "median_propinsr_ratio")],
  by = "year",
  all.x = T,
  all.y = F
)
home_payment[, median_retax_ratio := nafill(median_retax_ratio, type = "locf")]
home_payment[, median_propinsr_ratio := nafill(median_retax_ratio, type = "locf")]

home_payment[, insurance_and_taxes := (1.0 / 12.0) * zhvi * (median_retax_ratio +
                                                               median_propinsr_ratio)]

home_payment[, monthly_payment := principal_interest + insurance_and_taxes + pmi]

# Check the hh income as released
current_hh_income <- alfred::get_alfred_series("MEHOINUSA646N")
current_hh_income <- as.data.table(current_hh_income)
current_hh_income <- current_hh_income[order(date)]
current_hh_income <-
  current_hh_income[realtime_period  == max(realtime_period)]
current_hh_income[, realizedIncomeGrowth := MEHOINUSA646N / data.table::shift(MEHOINUSA646N, 1, type = "lag", fill = NA)]
current_2023_number <- current_hh_income[year(date) == oos_year]

estimated_income_growth <-
  merge(full_estimated_income_growth[month(date) == 1], current_hh_income, all.x = T)
estimated_income_growth <- estimated_income_growth[, c("date", "incomeEstimateFactor", "MEHOINUSA646N")]
estimated_income_growth <- estimated_income_growth[year(date) <= oos_year]
estimated_income_growth[, estimated_income :=  incomeEstimateFactor * data.table::shift(MEHOINUSA646N, n = 1, type = "lag")]
estimated_income_growth[, year := year(date)]


annual_home_payments <- home_payment[, list(
  annual_payment = sum(monthly_payment, na.rm = T),
  n_months = sum(!is.na(monthly_payment))
), by = "year"]
annual_payment_to_income <- merge(
  annual_home_payments,
  estimated_income_growth,
  by = "year",
  all.x = T,
  all.y = F
)
annual_payment_to_income <- annual_payment_to_income[n_months == 12]

annual_payment_to_income[, annual_pti_estimate := annual_payment  / estimated_income]
annual_payment_to_income[, annual_pti_actual := annual_payment  / MEHOINUSA646N]

# annual_payment_to_income
annual_affordability_plot <- ggplot() +
  geom_col(aes(x = year, y = annual_pti_actual, fill = "ASEC"), data = annual_payment_to_income[year < oos_year]) +
  geom_col(aes(x = year, y = annual_pti_actual, fill = "Out-of-Sample ASEC"),
           data = annual_payment_to_income[year == oos_year], position = "dodge",) +
  geom_col(aes(x = year, y = annual_pti_estimate, color = "Model"),
           data = annual_payment_to_income[year == oos_year], position = "dodge", fill = NA) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(title = "Annual Home Payments on Typical Home As % of Median Household Income", x = NULL, y = NULL, fill = NULL, color = NULL) +
  scale_fill_manual(values = c("ASEC" = set2_palette[[1]],"Out-of-Sample ASEC" = set2_palette[[2]], "Model" = "black")) +
  scale_color_manual(values = c("ASEC" = set2_palette[[1]],"Out-of-Sample ASEC" = set2_palette[[2]], "Model" = "black")) +
  theme_minimal()
annual_affordability_plot <- use_frbc_theme(annual_affordability_plot)

print(annual_affordability_plot)

# Figure 7: Annual home affordability measure
ggsave(
  filename = file.path(
    output_path,
    path_to_save_output,
    "fig7-annual-home-affordability.png"
  ),
  annual_affordability_plot,
  width = 6.25,
  height = 4.17,
  dpi = 320,
  units = "in"
)

ggsave(
  filename = file.path(
    output_path,
    path_to_save_output,
    "fig7-annual-home-affordability.pdf"
  ),
  annual_affordability_plot,
  width = 6.25,
  height = 4.17,
  dpi = 320,
  units = "in"
)

# Real income growth ----

cpi_index <- alfred::get_fred_series("CPIAUCSL", api_key = fred_key)
cpi_index <- as.data.table(cpi_index)
cpi_index[, dataDates := date %m+% months(1) %m-% days(1)]
cpi_index[, cpi_yoy := CPIAUCSL / data.table::shift(CPIAUCSL, 12, type = "lag")]
cpi_index[, panel := 1]

# get real personal income growth
real_pi_df <- alfred::get_fred_series("RPI", api_key = fred_key) # or RPI
real_pi_df <- as.data.table(real_pi_df)
real_pi_df[, dataDates := date %m+% months(1) %m-% days(1)]
real_pi_df[, real_pi_yoy := RPI / data.table::shift(RPI, 12, type = "lag")]

# get ASEC numbers
cpi_yoy_df <- copy(cpi_index[month(date) == 12, c("date", "dataDates", "cpi_yoy")])
cpi_yoy_df[, year := year(date)]
current_hh_income[, year:= year(date)]

asec_realinc_df <- merge(current_hh_income[, c("year", "realizedIncomeGrowth")], 
      cpi_yoy_df[, c("year", 'dataDates', "cpi_yoy")], by = "year")
asec_realinc_df[, real_inc_growth := exp(log(realizedIncomeGrowth) - log(cpi_yoy)) - 1]

full_estimated_income_growth[, panel := 1]

# get model bands 25/75
# full_estimated_income_growth[, incomeEstimateFactor := exp(incomeEstimates)]
full_estimated_income_growth[, band25Factor := exp(band25)]
full_estimated_income_growth[, band75Factor := exp(band75)]

# compare model income to CPI
cpi_modelinc_df <- merge(full_estimated_income_growth,
                         cpi_index,
                         by = "dataDates",
                         all = T)
cpi_modelinc_df[, panel := 2]
cpi_modelinc_df[, real_inc_growth := exp(log(incomeEstimateFactor) - log(cpi_yoy)) - 1]
cpi_modelinc_df[, real_inc_growth25 := exp(log(band25Factor) - log(cpi_yoy)) - 1]
cpi_modelinc_df[, real_inc_growth75 := exp(log(band75Factor) - log(cpi_yoy)) - 1]

# compare ASEC income to CPI
cps_hhi_data <- fread(path_to_cps)
cps_hhi_data[, nominal_income_growth := sa_hhi / data.table::shift(sa_hhi, 12, type = "lag")]
cps_hhi_data[, dataDates := as.Date(date)]
cpi_cpsinc_df <- merge(cps_hhi_data[, c("dataDates", "nominal_income_growth")], cpi_index[, c("dataDates", "cpi_yoy")], by = "dataDates", all = T)
cpi_cpsinc_df[, real_inc_growth := exp(log(nominal_income_growth) - log(cpi_yoy)) - 1]

# combine the series in a reasonable way

current_model_real_income_df <- rbindlist(
  list(
    data.table(
      date = full_estimated_income_growth$dataDates,
      value = full_estimated_income_growth$incomeEstimateFactor - 1,
      name = "Nominal Income Growth",
      model = "Model",
      band = NA
    ),
    data.table(
      date = full_estimated_income_growth$dataDates,
      value = full_estimated_income_growth$band25Factor - 1,
      name = "Nominal Income Growth",
      model = "Model",
      band = .25
    ),
    data.table(
      date = full_estimated_income_growth$dataDates,
      value = full_estimated_income_growth$band75Factor - 1,
      name = "Nominal Income Growth",
      model = "Model",
      band = .75
    ),
    data.table(
      date = cpi_index$dataDates,
      value = cpi_index$cpi_yoy - 1,
      name = "Inflation",
      model = "Model",
      band = NA
    ),
    data.table(
      date = cpi_modelinc_df$dataDates,
      value = cpi_modelinc_df$real_inc_growth,
      name = "Real Income Growth",
      model = "Model",
      band = NA
    ),
    data.table(
      date = cpi_modelinc_df$dataDates,
      value = cpi_modelinc_df$real_inc_growth25,
      name = "Real Income Growth",
      model = "Model",
      band = .25
    ),
    data.table(
      date = cpi_modelinc_df$dataDates,
      value = cpi_modelinc_df$real_inc_growth75,
      name = "Real Income Growth",
      model = "Model",
      band = .75
    ),
    data.table(
      date = real_pi_df$dataDates,
      value = real_pi_df$real_pi_yoy - 1,
      name = "Real Personal Income",
      model = "Model",
      band = NA
    ),
    data.table(
      date = asec_realinc_df$dataDates,
      value = asec_realinc_df$real_inc_growth,
      name = "CPI-Adjusted ASEC",
      model = "Model",
      band = NA
    )
    
  )
)

cps_real_income_df <- rbindlist(list(
  data.table(
    date = cps_hhi_data$dataDates,
    value = cps_hhi_data$nominal_income_growth - 1,
    name = "Nominal Income Growth",
    model = "CPS",
    band = NA
  ),
  data.table(
    date = cpi_index$dataDates,
    value = cpi_index$cpi_yoy - 1,
    name = "Inflation",
    model = "CPS",
    band = NA
  ),
  data.table(
    date = cpi_cpsinc_df$dataDates,
    value = cpi_cpsinc_df$real_inc_growth,
    name = "Real Income Growth",
    model = "CPS",
    band = NA
  ),
  data.table(
    date = real_pi_df$dataDates,
    value = real_pi_df$real_pi_yoy - 1,
    name = "Real Personal Income",
    model = "CPS",
    band = NA
  )
  
))

real_income_comp_df <- rbind(cps_real_income_df, current_model_real_income_df)


real_income_to_plot <- real_income_comp_df[name %in% c("Real Income Growth", "Real Personal Income")]
real_income_to_plot[, full_name := paste0(model, ": ", name)]

real_income_to_plot <- real_income_to_plot[full_name != "Model: Real Personal Income"]
real_income_to_plot <- real_income_to_plot[full_name == "CPS: Real Personal Income", full_name := "Real Personal Income"]
real_income_to_plot_bands <- real_income_to_plot[!is.na(band)]
real_income_to_plot <- real_income_to_plot[is.na(band)]

real_income_to_plot_bands <- dcast(real_income_to_plot_bands, date + name + model + full_name ~ band)


# Let's Look at EOY changes
eoy_income_estimates <- full_estimated_income_growth[month(dataDates) == 12]



# 2024 CPI forecast = 3.1 from BCEI July 2024
eoy_income_estimates <- merge(eoy_income_estimates,
                              cpi_index[, c("dataDates", "CPIAUCSL")],
                              all.x = T,
                              all.y = F)
eoy_income_estimates[, log_cpi := log(CPIAUCSL) - data.table::shift(log(CPIAUCSL), 1, type = "lag")]
eoy_income_estimates[is.na(log_cpi) &
                       year(dataDates) == 2024, log_cpi := log(1.031)]
eoy_income_estimates[, real_income_growth := incomeEstimates - log_cpi]
eoy_income_estimates[, real_income_growth25 := band25 - log_cpi]
eoy_income_estimates[, real_income_growth75 := band75 - log_cpi]
# plot
yoy_real_pi_df <- real_pi_df[month(dataDates) == 12]
yoy_cpi_cpsinc_df <- cpi_cpsinc_df[month(dataDates) == 12]
yoy_real_pi_df[, real_inc_growth := real_pi_yoy - 1]
yoy_real_pi_df[, fill_name := "BEA: Dec/Dec Real Personal Income"]
yoy_cpi_cpsinc_df[, fill_name := "CPS: Dec/Dec Real HH Income"]
asec_realinc_df[, fill_name := "ASEC: YoY Real HH Income"]
yoy_real_df <- rbind(yoy_real_pi_df,
                     yoy_cpi_cpsinc_df,
                     asec_realinc_df,
                     fill = T,
                     use.names = T)
yoy_real_df[, year := year(dataDates)]

yoy_real_df[, fill_name := factor(fill_name, 
                                  levels = c("BEA: Dec/Dec Real Personal Income", 
                                             "ASEC: YoY Real HH Income",
                                             "CPS: Dec/Dec Real HH Income"),
                                  ordered = T)]

yoy_real_income_plot <- ggplot() +
  geom_col(
    aes(x = year, y = real_inc_growth, fill = fill_name),
    data = yoy_real_df[year >= 2019],
    position = "dodge"
  ) +
  geom_linerange(aes(
    x =  year(dataDates),
    ymin = exp(real_income_growth25) - 1,
    ymax = exp(real_income_growth75) - 1,
    color = "Model: YoY Real HH Income"
  ),
  data = eoy_income_estimates[year(dataDates) >= 2019]) +
  geom_point(aes(
    x =  year(dataDates),
    y = exp(real_income_growth) - 1,
    color = "Model: YoY Real HH Income"
  ), data = eoy_income_estimates[year(dataDates) >= 2019]) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(
    values = c(
      "CPS: Dec/Dec Real HH Income" = set2_palette[[1]],
      "BEA: Dec/Dec Real Personal Income" = set2_palette[[2]],
      "ASEC: YoY Real HH Income" = set2_palette[[3]],
      "Model: YoY Real HH Income" = "black"
    )
  ) +
  scale_fill_manual(
    values = c(
      "CPS: Dec/Dec Real HH Income" = set2_palette[[1]],
      "BEA: Dec/Dec Real Personal Income" = set2_palette[[2]],
      "ASEC: YoY Real HH Income" = set2_palette[[3]],
      "Model: YoY Real HH Income" = "black"
    )
  ) +
  # facet_wrap(~name) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(limits = c(2018.5, NA), breaks = seq(2019, max(year(
    eoy_income_estimates$dataDates
  )))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    x = NULL,
    y = "%",
    color = NULL,
    linetype = NULL,
    fill = NULL,
    title = "Real Income Growth",
    caption = "Using Blue Chip Economic Indicator Consensus CPI Forecast for 2024 CPI"
  ) +
  guides(fill=guide_legend(nrow=2, ncol = 2, byrow = TRUE))
yoy_real_income_plot <- use_frbc_theme(yoy_real_income_plot)
print(yoy_real_income_plot)

# Figure 8: Year-over-year growth in real income
ggsave(
  filename = file.path(output_path, path_to_save_output, "fig8-yoy-real-income.png"),
  yoy_real_income_plot,
  width = 6.25,
  height = 3.5,
  dpi = 320,
  units = "in"
)

ggsave(
  filename = file.path(output_path, path_to_save_output, "fig8-yoy-real-income.pdf"),
  yoy_real_income_plot,
  width = 6.25,
  height = 3.5,
  dpi = 320,
  units = "in"
)

# Nominal income forecast plot ----
# get personal income growth
nominal_pi_df <- alfred::get_fred_series("PI", api_key = fred_key) # or RPI
nominal_pi_df <- as.data.table(nominal_pi_df)
nominal_pi_df[, dataDates := date %m+% months(1) %m-% days(1)]
nominal_pi_df[, pi_yoy := PI / data.table::shift(PI, 12, type = "lag")]


naive_model_df <- copy(current_hh_income)
naive_model_df <- naive_model_df[, c("date", "realizedIncomeGrowth")]
naive_model_df[, date := date %m+% months(12) %m-% days(1)]
naive_model_df[, value := realizedIncomeGrowth - 1]
naive_model_df[, realizedIncomeGrowth := NULL]

naive_model_dates <- real_income_comp_df[name == "Nominal Income Growth"][model == "Model"][is.na(band)][, c("date")]

naive_model_df <- merge(
  naive_model_dates,
  naive_model_df,
  all.x = T,
  all.y = F,
  by = "date"
)
naive_model_df$value <- approx(naive_model_df$date, naive_model_df$value, xout = naive_model_df$date)$y

naive_model_df[, name := "Nominal Income Growth"]
naive_model_df[, model := "Linear Interpolation"]
naive_model_df[, band := NA]

nominal_income_to_plot <- rbind(
  real_income_comp_df[name == "Nominal Income Growth"],
  data.table(
    date = nominal_pi_df$dataDates,
    value = nominal_pi_df$pi_yoy - 1,
    name = "Nominal Income Growth",
    model = "Nominal Personal Income",
    band = NA
  ),
  naive_model_df
)

nominal_income_to_plot[, full_name := paste0(model, ": ", name)]

nominal_income_to_plot <- nominal_income_to_plot[full_name == "Nominal Personal Income: Nominal Income Growth", full_name := "Nominal Personal Income"]

nominal_income_to_plot <- nominal_income_to_plot[date >= "2011-01-01"]

nominal_income_to_plot_bands <- nominal_income_to_plot[!is.na(band)]
nominal_income_to_plot <- nominal_income_to_plot[is.na(band)]

nominal_income_to_plot_bands <- dcast(nominal_income_to_plot_bands,
                                      date + name + model + full_name ~ band)

nominal_income_to_plot <- nominal_income_to_plot[full_name %in% c("Linear Interpolation: Nominal Income Growth",
                                                                  "Model: Nominal Income Growth")]

first_nominc_date <- min(nominal_income_to_plot[!is.na(value)][full_name == "Linear Interpolation: Nominal Income Growth"]$date)

# Let's show just the annual data
# use only annual values

annual_nominal_forecast <- ggplot(eoy_income_estimates) +
  geom_col(aes(
    x = year(dataDates),
    y = exp(incomeToMatch) - 1,
    fill = "ASEC"
  )) +
  geom_col(
    aes(
      x = year(date) - .05,
      y = realizedIncomeGrowth - 1,
      fill = "Out-of-Sample ASEC"
    ), data= current_2023_number
  ) +
  geom_linerange(aes(
    x = year(dataDates) - .05,
    ymin = exp(band25) - 1,
    ymax = exp(band75) - 1,
    color = "Model"
  )) +
  geom_point(aes(
    x = year(dataDates) - .05,
    y = exp(incomeEstimates) - 1,
    color = "Model"
  )) +
  geom_linerange(aes(
    x = year(dataDates) + .05,
    ymin = exp(band25) - 1,
    ymax = exp(band75) - 1,
    color = "Model with Updated Data"
  ), data = updated_income_growth[year(dataDates) >= oos_year]) +
  geom_point(aes(
    x = year(dataDates) + .05,
    y = exp(incomeEstimates) - 1,
    color = "Model with Updated Data"
  ), data = updated_income_growth[year(dataDates) >= oos_year]) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  scale_color_manual(values = c("ASEC" = set2_palette[[1]],
                                "Out-of-Sample ASEC" = set2_palette[[2]],
                                "Model" = "black",
                                "Model with Updated Data" = set2_palette[[3]])) +
  scale_fill_manual(values = c("ASEC" = set2_palette[[1]],
                               "Out-of-Sample ASEC" = set2_palette[[2]], 
                               "Model" = "black",
                               "Model with Updated Data" = set2_palette[[3]])) +
    # facet_wrap(~name) +
  scale_y_continuous(labels = scales::percent) +
  scale_x_continuous(limits = c(2014.5, NA), breaks = seq(2015, max(year(
    eoy_income_estimates$dataDates
  )))) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    x = NULL,
    y = "%",
    color = NULL,
    fill = NULL,
    linetype = NULL,
    title = "Income Growth (Year-over-Year)",
    subtitle = "Nominal, With 50% Error Bands"
  )
annual_nominal_forecast <- use_frbc_theme(annual_nominal_forecast)
print(annual_nominal_forecast)

# Figure 1: Year-over-year growth in nominal income
ggsave(
  filename = file.path(
    output_path,
    path_to_save_output,
    "fig1-nominalincome.png"
  ),
  annual_nominal_forecast,
  width = 6.25,
  height = 4.17,
  dpi = 320,
  units = "in"
)

ggsave(
  filename = file.path(
    output_path,
    path_to_save_output,
    "fig1-nominalincome.pdf"
  ),
  annual_nominal_forecast,
  width = 6.25,
  height = 4.17,
  dpi = 320,
  units = "in"
)

# Plot nominal income state ----
state_df <- full_estimated_income_growth[, c(
  "dataDates",
  "state",
  "stateBand5",
  "stateBand10",
  "stateBand25",
  "stateBand50",
  "stateBand75",
  "stateBand90",
  "stateBand95"
)]
nominal_income_state_plot <- ggplot() +
  geom_ribbon(
    aes(x = dataDates, ymin = stateBand25, ymax = stateBand75),
    data = state_df,
    alpha = .35
  ) +
  geom_ribbon(
    aes(x = dataDates, ymin = stateBand5, ymax = stateBand95),
    data = state_df,
    alpha = .35
  ) +
  geom_line(aes(x = dataDates, y = state), data = state_df) +
  geom_hline(yintercept = 0, linetype = "dashed") +
  # facet_wrap(~name) +
  coord_cartesian(ylim = c(-1.5, 1.5)) +
  scale_x_date(limits = c(as.Date("2015-01-01"), NA)) +
  theme_minimal() +
  theme(legend.position = "bottom") +
  labs(
    x = NULL,
    y = NULL,
    color = NULL,
    fill = NULL,
    linetype = NULL,
    title = "State Value Related to Monthly Nominal Income Growth",
    subtitle = "With 50% and 90% error bands",
    caption = "Truncated y-axis to emphasize error bands instead of Covid outliers."
  ) +
  guides(fill = "none")
nominal_income_state_plot <- use_frbc_theme(nominal_income_state_plot)
print(nominal_income_state_plot)

# Figure A4: State corresponding to monthly change in nominal income
ggsave(
  filename = file.path(
    output_path,
    path_to_save_output,
    "figa4-state-value.png"
  ),
  nominal_income_state_plot,
  width = 6.25,
  height = 3.5,
  dpi = 320,
  units = "in"
)

ggsave(
  filename = file.path(
    output_path,
    path_to_save_output,
    "figa4-state-value.pdf"
  ),
  nominal_income_state_plot,
  width = 6.25,
  height = 3.5,
  dpi = 320,
  units = "in"
)
