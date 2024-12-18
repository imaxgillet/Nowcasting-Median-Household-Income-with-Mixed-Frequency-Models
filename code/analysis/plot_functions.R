# plot_functions.R ----
library(data.table)
library(xtable)
library(ggplot2)

create_dataspec_table <- function(dataspec_filename,
                                  output_filename){
  # create_dataspec_table creates the .tex file that shows the data inputs
  # inputs:
  #   dataspec_filename (character): full filename (incl path) to the data spec csv
  #   output_filename (character): full filename (incl path) to the data spec .tex output
  # output:
  #   prints/saves .tex
  
  
  data_spec <- fread(dataspec_filename)
  
  # drops the related series
  data_spec <- data_spec[is.na(related)]
  
  data_table_output <-
    data_spec[, c("db_code", "description", "transformation")]
  colnames(data_table_output) <-
    c("FRED Identifier", "Description", "Transformation")
  
  print(
    xtable(data_table_output),
    floating = FALSE,
    latex.environments = NULL,
    booktabs = TRUE,
    file = output_filename,
    include.rownames = F
  )
}

asec_acs_plot <- function(fred_api_key,
                          output_filename){
  # asec_acs_plot creates the plot
  # inputs:
  #   fred_api_key (character): api key to use for FRED
  #   output_filename (character): full filename (incl path) to the acs/asec output
  # output:
  #   prints/saves png
  
  asec_hhi <- alfred::get_fred_series("MEHOINUSA646N", api_key = fred_key)
  asec_hhi$survey <- "ASEC"
  asec_hhi$value <- asec_hhi$MEHOINUSA646N
  asec_hhi$MEHOINUSA646N <- NULL
  
  acs_hhi <- alfred::get_fred_series("MHIUS00000A052NCEN", api_key = fred_key)
  acs_hhi$survey <- "ACS"
  acs_hhi$value <- acs_hhi$MHIUS00000A052NCEN
  acs_hhi$MHIUS00000A052NCEN <- NULL
  
  hhi_comparison_df <- rbindlist(list(acs_hhi, asec_hhi),
                                 use.names = T,
                                 fill = T)
  hhi_comparison_df$year <- year(hhi_comparison_df$date)
  hhi_comparison_df_w <- data.table::dcast(hhi_comparison_df, date ~ survey)

  hhi_xy_comp <- ggplot(hhi_comparison_df_w) +
    geom_point(aes(x = ASEC, y = ACS)) +
    geom_abline(slope = 1, intercept = 0) +
    theme_minimal() +
      labs(color = NULL)
  
  hhi_xy_comp <- use_frbc_theme(hhi_xy_comp)
  
  hhi_line_comp <- ggplot(hhi_comparison_df) +
    geom_point(aes(x = year, y = value, color = survey), alpha = .5) +
    geom_line(aes(x = year, y = value, color = survey), alpha = .5) +
    scale_color_brewer(palette = "Set2") +
    theme_minimal() +
    labs(x = NULL, color = NULL, y = NULL) +
    theme(legend.position = "top")
  
  hhi_line_comp <- use_frbc_theme(hhi_line_comp)
  
  hhi_comp_plot <- ggarrange(hhi_xy_comp, hhi_line_comp, nrow = 1)
  print(hhi_comp_plot)
  
  # Figure 2: Comparison of ASEC and ACS Measures of Median Household Income
  ggsave(
    filename = file.path(output_path, path_to_save_output, "fig2-acs-asec.png"),
    hhi_comp_plot,
    width = 6.25,
    height = 4.17,
    dpi = 320,
    units = "in"
  )
  
  ggsave(
    filename = file.path(output_path, path_to_save_output, "fig2-acs-asec.pdf"),
    hhi_comp_plot,
    width = 6.25,
    height = 4.17,
    dpi = 320,
    units = "in"
  )
  
  ggsave(
    filename = file.path(output_path, path_to_save_output, "fig2a-acs-asec.png"),
    hhi_xy_comp,
    width = 6.25,
    height = 4.17,
    dpi = 320,
    units = "in"
  )
  
  ggsave(
    filename = file.path(output_path, path_to_save_output, "fig2a-acs-asec.pdf"),
    hhi_xy_comp,
    width = 6.25,
    height = 4.17,
    dpi = 320,
    units = "in"
  )
  
  ggsave(
    filename = file.path(output_path, path_to_save_output, "fig2b-acs-asec.png"),
    hhi_line_comp,
    width = 6.25,
    height = 4.17,
    dpi = 320,
    units = "in"
  )
  
  ggsave(
    filename = file.path(output_path, path_to_save_output, "fig2b-acs-asec.pdf"),
    hhi_line_comp,
    width = 6.25,
    height = 4.17,
    dpi = 320,
    units = "in"
  )
}

get_all_backtests <- function(directory_with_backtests){
  
  # folder structure is:
  # output/YYYYMMDD_HHMMSS_backtest/MODELNAME/REALTIMEYYYMMDD/output.csv
  # one of these for each MODEL
  model_folders <- list.dirs(file.path(output_path, backtest_dir), recursive = F)
  backtest_results <- vector("list", length(model_folders))
  for (n_model_folder in seq_along(model_folders)) {
    current_model_folder <- model_folders[n_model_folder]
    current_backtest_dirs <- list.dirs(current_model_folder, full.names = T)
    current_backtest_dirs <- current_backtest_dirs[current_backtest_dirs != current_model_folder]
    backtest_data <- lapply(current_backtest_dirs, function(x) {
      df <- fread(file.path(x, "output.csv"))
      df[, dataDates := dmy(dataDates)]
      return(df)
    })
    names(backtest_data) <- current_backtest_dirs
    
    # do we compare the current Sep output or the last EOY output?
    # do we need to aggregate? Or just take the alpha element
    
    backtest_data_df <-
      rbindlist(backtest_data, idcol = "backtest_folder")
    backtest_data_df <- backtest_data_df[month(dataDates) == 12]
    backtest_data_df[, max_date := max(dataDates), by = "backtest_folder"]
    backtest_data_df <-
      backtest_data_df[dataDates  == max_date, c("dataDates",
                                                 "incomeEstimates",
                                                 "backtest_folder",
                                                 "runTime")]
    # these are in log diffs
    backtest_data_df[, incomeEstimateFactor := exp(incomeEstimates)]
    backtest_data_df[, date := dataDates + days(1) - years(1)] # date for projection - last year
    backtest_data_df[, model := basename(current_model_folder)]
    backtest_results[[n_model_folder]] <- backtest_data_df
  }
  all_backtest_data_df <- rbindlist(backtest_results)
  return(all_backtest_data_df)
  
}

create_backtesting_results <- function(directory_with_backtests,
                                       path_to_cps,
                                       bic_filepath,
                                       output_path,
                                       first_oos_year = NULL){

  # gather up all backtests
  all_backtest_data_df <- get_all_backtests(directory_with_backtests)
  
  # Check the hh income as released
  all_hh_income <- alfred::get_alfred_series("MEHOINUSA646N")
  all_hh_income <- as.data.table(all_hh_income)
  all_hh_income[, max_date := max(date), by = "realtime_period"]
  
  # Deal with backtests that were run in matlab
  to_fit_hh_income <- all_hh_income[date  == max_date]
  to_fit_hh_income[, max_date := NULL]
  to_fit_hh_income[, realizedIncomeGrowth := MEHOINUSA646N /  data.table::shift(MEHOINUSA646N, n = 1, type = "lag")]
  
  
  # keep only models of interest
  models_of_interest_df <- data.table(
    model = c(
      'ar1_first',
      'ar1_first2',
      'base',
      'base2',
      'target_ar1',
      'target2_ar1',
      'targeted',
      'targeted2',
      'mfvar'
    ),
    clean_model = c(
      "1 factor, HHI AR(1) error",
      "2 factor, HHI AR(1) error",
      "1 factor baseline",
      "2 factor baseline",
      "1 factor collapsed, HHI AR(1) error",
      "2 factor collapsed, HHI AR(1) error",
      "1 factor collapsed",
      "2 factor collapsed",
      "MF-VAR"
    )
  )
  
  all_backtest_data_df <- merge(all_backtest_data_df, models_of_interest_df)
  all_backtest_data_df[, model := clean_model]
  all_backtest_data_df[, clean_model := NULL]
  
  
  avg_model <- all_backtest_data_df[, list(incomeEstimateFactor = mean(incomeEstimateFactor, na.rm = T)), by = c("date", "dataDates")]
  avg_model[, model := "Average model"]
  all_backtest_data_df <- rbind(all_backtest_data_df,
                                avg_model,
                                use.names = T,
                                fill = T)
  
  backtest_comparison_df <-
    merge(
      to_fit_hh_income,
      all_backtest_data_df,
      by = c("date"),
      all.x = T,
      all.y = T
    )
  
  backtest_comparison_df <- backtest_comparison_df[order(date, model)]
  backtest_comparison_df[, projected_income := incomeEstimateFactor * data.table::shift(MEHOINUSA646N, n = 1, type = "lag"), by = "model"]
  
  # Annual AR model backtest ----
  # aside from 2020, most years are unrevised after release (ex rounding)
  
  all_realtime_periods <- unique(all_hh_income$realtime_period)
  all_realtime_periods <- all_realtime_periods[order(all_realtime_periods)]
  # date / MEHOINUSA646N / projected_income
  all_projected_rates <- c()
  all_projected_incomes <- c()
  for (nud in seq_along(all_realtime_periods)) {
    this_rtp <- all_realtime_periods[nud]
    
    if (nud == 1) {
      this_data <- all_hh_income[realtime_period == this_rtp]
      this_data <- this_data[order(date)]
      this_data <- head(this_data, nrow(this_data) - 1)
    } else{
      this_data <- all_hh_income[realtime_period == all_realtime_periods[nud -
                                                                           1]]
    }
    
    # calc growth rate
    this_data <- this_data[order(date)]
    this_data[, growth_rate := log(MEHOINUSA646N / data.table::shift(MEHOINUSA646N, n = 1, type = "lag"))]
    
    var_model <- lm(this_data$growth_rate ~ data.table::shift(this_data$growth_rate, 1, type = "lag"))
    var_coef <- var_model$coefficients
    
    last_period_growth <- tail(this_data$growth_rate, 1)
    projected_log_growth <- last_period_growth * var_coef[[2]] + var_coef[[1]]
    
    projected_income <- exp(projected_log_growth) * tail(this_data$MEHOINUSA646N, 1)
    all_projected_incomes <- c(all_projected_incomes, projected_income)
    all_projected_rates <- c(all_projected_rates, exp(projected_log_growth))
  }
  
  var_projection_df <- data.table(
    realtime_period = all_realtime_periods,
    projected_income = all_projected_incomes,
    incomeEstimateFactor = all_projected_rates
  )
  
  var_projection_df <- merge(
    all_hh_income[max_date == date],
    var_projection_df,
    all.x = T,
    all.y = T,
    by = 'realtime_period'
  )
  var_projection_df[, model := "AR"]
  var_projection_df[realtime_period == min(realtime_period), "projected_income"] <- NA
  backtest_comparison_df <- rbind(backtest_comparison_df,
                                  var_projection_df,
                                  fill = T,
                                  use.names = T)
  
  # CPS-imputed income ----
  # Run "impute_cps_median_income.R" first to get the data
  
  # if it exists, read in the CPS-imputed data
  all_projected_incomes_cps <- c()
  all_projected_incomes_cps_sa <- c()
  
  if (file.exists(path_to_cps)) {
    cps_hhi_data <- fread(path_to_cps)
    
    for (nud in seq_along(all_realtime_periods)) {
      this_rtp <- all_realtime_periods[nud]
      
      if (nud == 1) {
        this_data <- all_hh_income[realtime_period == this_rtp]
        this_data <- this_data[order(date)]
        this_data <- head(this_data, nrow(this_data) - 1)
      } else{
        this_data <- all_hh_income[realtime_period == all_realtime_periods[nud -
                                                                             1]]
      }
      
      cps_hhi_now <- cps_hhi_data[date < this_rtp & MONTH == 12]
      
      hhi_growth <- tail(cps_hhi_now$hhi, 2)
      sa_hhi_growth <- tail(cps_hhi_now$sa_hhi, 2)
      
      hhi_growth <- hhi_growth[2] / hhi_growth[1]
      sa_hhi_growth <- sa_hhi_growth[2] / sa_hhi_growth[1]
      
      # calc growth rate
      this_data <- this_data[order(date)]
      
      
      all_projected_incomes_cps <- c(all_projected_incomes_cps,
                                     hhi_growth * tail(this_data$MEHOINUSA646N, 1))
      all_projected_incomes_cps_sa <- c(all_projected_incomes_cps_sa,
                                        sa_hhi_growth * tail(this_data$MEHOINUSA646N, 1))
    }
    
    dt2 <- data.table(
      realtime_period = all_realtime_periods,
      projected_income = all_projected_incomes_cps_sa,
      model = "CPS"
    )
    dt2 <- tail(dt2, nrow(dt2) - 1)
    
    cps_dt <- dt2 # rbind(dt1, dt2)
    cps_dt <- merge(
      all_hh_income[max_date == date],
      cps_dt,
      all.x = T,
      all.y = T,
      by = 'realtime_period'
    )
    
    
    
    backtest_comparison_df <- rbind(backtest_comparison_df,
                                    cps_dt,
                                    fill = T,
                                    use.names = T)
    
    
  } else{
    warning(paste0("Cannot find cps data file at ", path_to_cps))
  }
  
  if (!is.null(first_oos_year)){
    backtest_comparison_df <- backtest_comparison_df[
                             year(date) < first_oos_year]
  }
  
  # Backtest plot: date on X, realization + backtest TS on Y ----
  backtest_ts_plot <-
    ggplot(backtest_comparison_df[!is.na(MEHOINUSA646N) &
                                    !is.na(projected_income)]) +
    geom_line(aes(x = date, y = MEHOINUSA646N)) +
    
    geom_point(aes(x = date, y = MEHOINUSA646N), color = "black") +
    
    geom_point(
      aes(x = date, y = projected_income, fill = model),
      color = 'black',
      shape = 21
    ) +
    theme_minimal() +
    facet_wrap(~ model, ncol = 3) +
    scale_fill_brewer(palette = "Paired") +
    labs(x = NULL,
         y = "Household Income",
         # title = "Time Series Comparison of Income to Projection",
         fill = NULL,
         color = NULL) +
    theme(legend.position = "bottom", 
          legend.text = element_text(margin = margin(t = 1)),
          legend.key.height = ggplot2::unit(1, "mm"))
  backtest_ts_plot <- use_frbc_theme(backtest_ts_plot)
  
  print(backtest_ts_plot)
  
  # Figure 3: Time series of forecasts and realizations
  ggsave(
    filename = file.path(output_path, "fig3-forecast-ts.png"),
    backtest_ts_plot,
    width = 6.25,
    height = 4.17,
    dpi = 320,
    units = "in"
  )
  
  
  ggsave(
    filename = file.path(
      output_path,
      "fig3-forecast-ts.pdf"
    ),
    backtest_ts_plot,
    width = 6.25,
    height = 4.17,
    dpi = 320,
    units = "in"
  )
  
  # RMSE ----
  rmse_table <- backtest_comparison_df[!is.na(MEHOINUSA646N) &
                                         !is.na(projected_income), list(count = .N,
                                                                        rmse = sqrt(mean((MEHOINUSA646N - projected_income) ^ 2, na.rm = T
                                                                        )),
                                                                        mae = mean(abs(MEHOINUSA646N - projected_income), na.rm = T)), by = "model"]
  
  rmse_table <- rmse_table[order(rmse)]
  rmse_table$rmse <- scales::dollar(rmse_table$rmse, accuracy = 1)
  rmse_table$mae <- scales::dollar(rmse_table$mae, accuracy = 1)
  colnames(rmse_table) <- c("Model", "Years Out of Sample", "RMSE", "MAE")
  
  
  backtest_comparison_df[, mae := abs(MEHOINUSA646N - projected_income)]
  backtest_comparison_df[, error := (MEHOINUSA646N - projected_income)]
  backtest_comparison_df[, pct_error := (MEHOINUSA646N - projected_income) /
                           MEHOINUSA646N]
  
  # RMSE 3 panels: one w each time series ----
  error_ts_df <- backtest_comparison_df[!is.na(MEHOINUSA646N) &
                                          !is.na(projected_income), c("date", "model", "mae", "error", "pct_error")]
  error_ts_df <- tidyr::pivot_longer(error_ts_df, -c("date", "model"))
  error_ts_df <- as.data.table(error_ts_df)
  map_clean_names <- c("Error"="error",
                       "Mean absolute error"="mae",
                       "Error as percent of realized value"="pct_error")
  error_ts_df[,clean_name := names(map_clean_names)[match(name, map_clean_names)]]
  
  
  error_ts_df$panel <- ifelse(error_ts_df$date <= "2019-01-01", "before 2020", "2020 and after")
  error_ts_df[, panel := factor(panel, levels = c("before 2020", "2020 and after"), ordered = T)]
  error_ts_combined_plot <- ggplot(error_ts_df) +
    geom_hline(yintercept = 0, linetype = 'dashed') +
    geom_point(aes(x = date, y = value, fill = model),
               color = 'black',
               shape = 21) +
    facet_wrap(
      clean_name ~ panel,
      ncol = 2,
      scales = "free",
      labeller = labeller(.multi_line = F)
    ) +
    theme_minimal() +
    scale_fill_brewer(palette = "Paired") +
    labs(x = NULL, 
         y = NULL,
         fill = NULL) +
    theme(legend.position = "bottom", 
          legend.text = element_text(margin = margin(t = 1)),
          legend.key.height = ggplot2::unit(1, "mm")) +
    guides(fill = guide_legend(byrow = TRUE))
  error_ts_combined_plot <- use_frbc_theme(error_ts_combined_plot)
  print(error_ts_combined_plot)
  
  # Figure 4: Time series of errors
  ggsave(
    filename = file.path(
      output_path,
      "fig5-forecast-ts-errors.png"
    ),
    error_ts_combined_plot,
    width = 6.25,
    height = 4.17,
    dpi = 320,
    units = "in"
  )
  
  ggsave(
    filename = file.path(
      output_path,
      "fig5-forecast-ts-errors.pdf"
    ),
    error_ts_combined_plot,
    width = 6.25,
    height = 4.17,
    dpi = 320,
    units = "in"
  )

  
  # Diebold-Mariano Test ----
  all_models <- unique(backtest_comparison_df[!is.na(MEHOINUSA646N) &
                                                !is.na(projected_income)]$model)
  all_models <- all_models[all_models != "AR"]
  
  dm_results <- vector("list", length(all_models))
  names(dm_results) <- all_models
  for (n_am in seq_along(all_models)) {
    current_model <- all_models[n_am]
    
    cmdf <- backtest_comparison_df[model == current_model, c("date", "error", "pct_error")]
    vardf <- backtest_comparison_df[model == "AR", c("date", "error", "pct_error")]
    
    compdf <- merge(cmdf,
                    vardf,
                    by = "date",
                    suffixes = c("_model", "_var"))
    
    dm_results_error <- dm.test(compdf$error_model,
                                compdf$error_var,
                                alternative = "less",
                                power = 2)
    dm_results_pct <- dm.test(
      abs(compdf$error_model),
      abs(compdf$error_var),
      alternative = "less",
      power = 1
    )
    
    dm_results[[n_am]] <- list(sq_error = dm_results_error$p.value, abs_error = dm_results_pct$p.value)
    
  }
  
  dm_results_df <- rbindlist(dm_results, idcol = "model")
  dm_results_df <- dm_results_df[order(sq_error)]
  
  combined_rmse_df <- merge(
    rmse_table,
    dm_results_df,
    by.x = "Model",
    by.y = "model",
    all.x = T,
    all.y = F
  )
  combined_rmse_df[, sq_error_stars := fcase(sq_error < .01,
                                             "***",
                                             sq_error < .05,
                                             "**",
                                             sq_error < .1,
                                             "*",
                                             default = "")]
  combined_rmse_df[, abs_error_stars := fcase(abs_error  < .01,
                                              "***",
                                              abs_error  < .05,
                                              "**",
                                              abs_error  < .1,
                                              "*",
                                              default = "")]
  
  combined_rmse_df[, RMSE := paste0(RMSE, sq_error_stars)]
  combined_rmse_df[, MAE := paste0(MAE, abs_error_stars)]
  
  
  rmse_table_list <- list(combined_rmse_df[, c("Model", "Years Out of Sample", "RMSE", "MAE")])
  attr(rmse_table_list, "message") <- c("$^{*}$p$<$0.1; $^{**}$p$<$0.05; $^{***}$p$<$0.01")
  rmse_table_list <- xtableList(rmse_table_list)
  
  latex_code <- print.xtableList(
    rmse_table_list,
    file = file.path(output_path, "combined_rmse_table.tex"),
    floating = FALSE,
    latex.environments = NULL,
    booktabs = TRUE,
    include.rownames = F
  )
  
  # Figure 4: Error in forecasting models
  cat(latex_code,
      file = file.path(output_path, "combined_rmse_table.tex"))
  
  
  # Lag Selection ----
  # add BIC of annual VAR
  current_var_df <- all_hh_income[realtime_period == max(realtime_period)]
  # calc growth rate
  current_var_df <- current_var_df[order(date)]
  current_var_df[, growth_rate := log(MEHOINUSA646N / data.table::shift(MEHOINUSA646N, n = 1, type = "lag"))]
  current_var_df[, l1 := data.table::shift(growth_rate, 1, type = "lag")]
  current_var_df[, l2 := data.table::shift(growth_rate, 2, type = "lag")]
  current_var_df[, l3 := data.table::shift(growth_rate, 3, type = "lag")]
  current_var_df[, l4 := data.table::shift(growth_rate, 4, type = "lag")]
  current_var_df[, l5 := data.table::shift(growth_rate, 5, type = "lag")]
  current_var_df[, l6 := data.table::shift(growth_rate, 6, type = "lag")]
  
  current_var_model_l1 <- lm(current_var_df$growth_rate ~
                               current_var_df$l1)
  current_var_model_l2 <- lm(current_var_df$growth_rate ~
                               current_var_df$l1 +
                               current_var_df$l2)
  current_var_model_l3 <- lm(
    current_var_df$growth_rate ~
      current_var_df$l1 +
      current_var_df$l2 +
      current_var_df$l3
  )
  current_var_model_l4 <- lm(
    current_var_df$growth_rate ~
      current_var_df$l1 +
      current_var_df$l2 +
      current_var_df$l3 +
      current_var_df$l4
  )
  current_var_model_l5 <- lm(
    current_var_df$growth_rate ~
      current_var_df$l1 +
      current_var_df$l2 +
      current_var_df$l3 +
      current_var_df$l4 +
      current_var_df$l5
  )
  current_var_model_l6 <- lm(
    current_var_df$growth_rate ~
      current_var_df$l1 +
      current_var_df$l2 +
      current_var_df$l3 +
      current_var_df$l4 +
      current_var_df$l5 +
      current_var_df$l6
  )
  
  
  bic_l1 <- BIC(current_var_model_l1)
  bic_l2 <- BIC(current_var_model_l2)
  bic_l3 <- BIC(current_var_model_l3)
  bic_l4 <- BIC(current_var_model_l4)
  bic_l5 <- BIC(current_var_model_l5)
  bic_l6 <- BIC(current_var_model_l6)
  
  hqic_l1 <- ICglm::HQIC(current_var_model_l1)
  hqic_l2 <- ICglm::HQIC(current_var_model_l2)
  hqic_l3 <- ICglm::HQIC(current_var_model_l3)
  hqic_l4 <- ICglm::HQIC(current_var_model_l4)
  hqic_l5 <- ICglm::HQIC(current_var_model_l5)
  hqic_l6 <- ICglm::HQIC(current_var_model_l6)
  
  annual_var_bic_df <- data.table(
    full_clean_model = "AR",
    lags = c(1, 2, 3, 4, 5, 6),
    BIC = c(bic_l1, bic_l2, bic_l3, bic_l4, bic_l5, bic_l6),
    HQIC = c(hqic_l1, hqic_l2, hqic_l3, hqic_l4, hqic_l5, hqic_l6)
  )
  #
  
  
  bic_df <- fread(bic_filepath)
  
  bic_models_of_interest_df <- data.table(
    functionName = c(
      'estimate_base_model',
      'estimate_ar1_model',
      'estimate_targeted_model',
      'estimate_targeted_ar1_model',
      'estimate_mfvar_model'
    ),
    clean_model = c(
      "baseline",
      "HHI AR(1) error",
      "collapsed",
      "collapsed, HHI AR(1) error",
      "MF-VAR"
    )
  )
  
  bic_df <- merge(
    bic_df,
    bic_models_of_interest_df,
    by = "functionName",
    all.x = T,
    all.y = F
  )
  
  # drop 2 factor collapsed + AR1 error
  bic_df <- bic_df[clean_model != "collapsed, HHI AR(1) error" | nFactors != 2]
  
  bic_df[, full_clean_model := ifelse(nFactors == 0,
                                      clean_model,
                                      ifelse(clean_model == "HHI AR(1) error",
                                             paste0(nFactors, " factor, ", clean_model),
                                             paste0(nFactors, " factor ", clean_model))
                                      )]
  bic_df <- rbind(bic_df, annual_var_bic_df, fill = T)
  bic_plot <- ggplot() +
    geom_point(aes(x = lags, y = BIC, color = "BIC"), data = bic_df) +
    geom_line(aes(x = lags, y = BIC, color = "BIC"), data = bic_df) +
    facet_wrap( ~ full_clean_model, scales = "free_y", ncol = 3) +
    scale_color_brewer(palette = "Set2") +
    labs(color = NULL) +
    theme_minimal() +
    guides(color = FALSE)
  bic_plot <- use_frbc_theme(bic_plot)
  
  print(bic_plot)
  
  # Figure 9: BIC at each lag for each model
  ggsave(
    filename = file.path(output_path, "figa2-bic.png"),
    bic_plot,
    width = 6.25,
    height = 4.17,
    dpi = 320,
    units = "in"
  )
  
  ggsave(
    filename = file.path(output_path, "figa2-bic.pdf"),
    bic_plot,
    width = 6.25,
    height = 4.17,
    dpi = 320,
    units = "in"
  )
  
  hqic_plot <- ggplot() +
    geom_point(aes(x = lags, y = HQIC, color = "HQIC"),
               data = bic_df,
               color = "red") +
    geom_line(aes(x = lags, y = HQIC, color = "HQIC"),
              data = bic_df,
              color = "red") +
    facet_wrap( ~ full_clean_model, scales = "free_y", ncol = 3) +
    theme_minimal()
  hqic_plot <- use_frbc_theme(hqic_plot)
  print(hqic_plot)
  
  # Figure 10: HQIC at each lag for each model
  ggsave(
    filename = file.path(output_path, "figa3-hqic.png"),
    hqic_plot,
    width = 6.25,
    height = 4.17,
    dpi = 320,
    units = "in"
  )
  
  ggsave(
    filename = file.path(output_path, "figa3-hqic.pdf"),
    hqic_plot,
    width = 6.25,
    height = 4.17,
    dpi = 320,
    units = "in"
  )
  
  # Estimation time ----
  
  est_time_df <- all_backtest_data_df[, list(avgRunTime = mean(runTime / 60),
                                             minRunTime = min(runTime / 60)), by = c("model")]
  estime_boxplot <- ggplot(all_backtest_data_df[!is.na(runTime)]) +
    geom_boxplot(aes(x = model, y = runTime / 60, fill = model)) +
    theme_minimal() +
    labs(x = NULL, y = "Minutes") +
    theme(legend.position = "none") +
    guides(fill = FALSE) +
    coord_flip()
  estime_boxplot <- use_frbc_theme(estime_boxplot)
  print(estime_boxplot)
  
  # Figure 5: Estimation time of given models
  ggsave(
    filename = file.path(output_path, "fig6-estimation-time.png"),
    estime_boxplot,
    width = 6.25,
    height = 4.17,
    dpi = 320,
    units = "in"
  )
  
  
  ggsave(
    filename = file.path(output_path, "fig6-estimation-time.pdf"),
    estime_boxplot,
    width = 6.25,
    height = 4.17,
    dpi = 320,
    units = "in"
  )
  
}

use_frbc_theme <- function(ggobj){
  
  ggobj <- ggobj + theme(
    legend.position = "bottom",
    text = element_text(family = "sans", size = 7),
    panel.grid.minor.x = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.y = element_blank(),
    axis.line.x = element_line(),
    axis.line.y = element_line(),
    axis.ticks.x = element_line(),
    axis.ticks.y = element_line(),
    axis.ticks.length.x = ggplot2::unit(-1, "mm"),
    axis.ticks.length.y = ggplot2::unit(-1, "mm"),
    panel.background = element_rect(fill='white', colour='white'),
    plot.background = element_rect(fill='white', colour='white')
  ) +
    guides(linetype = "none")
  return(ggobj)
  
}