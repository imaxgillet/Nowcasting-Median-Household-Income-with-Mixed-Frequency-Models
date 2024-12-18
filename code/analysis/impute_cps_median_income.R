# impute_cps_median_income.R
# Uses IPUMS CPS data to estimate median household income
# Max Gillet, 2024

library(spatstat)
library(ipumsr)
library(data.table)
library(ggplot2)
library(dplyr)
library(magrittr)
library(lubridate)
library(seasonal)
library(zoo)

# Set up IPUMS API Key ----
tryCatch(
  api_key <<- keyring::key_get("ipums_api"),
  error = function(e) {
    keyring::key_set("ipums_api", prompt = "Type your IPUMS key:")
    api_key <<- keyring::key_get("ipums_api")
    
  }
)

all_surveys <- get_sample_info("cps", api_key = api_key)
all_surveys <- as.data.table(all_surveys)
all_surveys <- all_surveys[!grepl("ASEC", description, fixed = T)]
all_surveys <- all_surveys[order(name)]
all_surveys <- all_surveys[as.numeric(substr(name, 4, 7)) >= 2010]


vars_used <- list(
  var_spec("YEAR"),
  var_spec("MONTH"),
  var_spec("HWTFINL"),
  var_spec("MISH"),
  var_spec("FAMINC")
)

cps_hhi_extract <- define_extract_cps(
  description = "median_hhi_cps",
  samples = all_surveys$name,
  variables = vars_used,
  data_format = "fixed_width"
)
submitted_extract <- submit_extract(cps_hhi_extract,
                                    api_key = api_key)
downloadable_extract <- wait_for_extract(submitted_extract,
                                         api_key = api_key)

data_files <- download_extract(downloadable_extract,
                               api_key = api_key,
                               download_dir = "./data/",
                               overwrite = T)

data_file <- read_ipums_micro(data_files)
var_info <- ipums_var_info(data_file)
setDT(data_file)

faminc_map <- var_info %>%
  filter(var_name == "FAMINC") %>%
  pull(val_labels)

faminc_map <- data.table(faminc_map[[1]])
faminc_map[, c("ll", "ul") := tstrsplit(lbl, "[-())]", keep = c(1,2))]
faminc_map[, ll := as.numeric(gsub("[^0-9]","", ll))]
faminc_map[, ul := as.numeric(gsub("[^0-9]","", ul))]

faminc_map$mid_value <- rowMeans(faminc_map[,c("ll", "ul")],na.rm = T)
faminc_map[, ll := ifelse(lbl == "Under $5,000", 0, ll)]
faminc_map[, ul := ifelse(lbl == "Under $5,000", 5000, ul)]

faminc_map[, ll := ifelse(lbl == "$150,000 and over", 150000, ll)]
faminc_map[, ul := ifelse(lbl == "$150,000 and over", Inf, ul)]
faminc_map <- faminc_map[, c("val", "lbl", "ll", "ul", "mid_value")]

# TODO: inflation adjust by MISH, break into a distribution of income by person, combine MISH-level incomes into a monthly income dist

# clean the data ----
hhi_dist_by_ym <- data_file[,list(
  count = .N,
  total_weight = sum(HWTFINL, na.rm = T)
),
by = c("YEAR", "MONTH", "FAMINC")]

hhi_dist_by_ym <- merge(hhi_dist_by_ym, faminc_map, all.x = T, all.y = F, by.x = "FAMINC", by.y = "val")
bucket_no <- unique(hhi_dist_by_ym[, c("mid_value")])
bucket_no <- bucket_no[!is.na(mid_value)]
bucket_no <- bucket_no[order(mid_value)]
bucket_no[, n_bucket := .GRP, by = "mid_value"]

hhi_dist_by_ym <- merge(hhi_dist_by_ym, bucket_no, all.x = T, all.y = F, by = "mid_value")

hhi_dist_by_ym <- hhi_dist_by_ym[!is.na(n_bucket)]
hhi_dist_by_ym <- hhi_dist_by_ym[total_weight > 0]
hhi_dist_by_ym <- hhi_dist_by_ym[order(YEAR, MONTH, mid_value)]
hhi_dist_by_ym[, cdf_incl_bin := cumsum(total_weight) / sum(total_weight), by=  c("YEAR", "MONTH")]
hhi_dist_by_ym[, count_incl_bin := cumsum(total_weight), by=  c("YEAR", "MONTH")]
hhi_dist_by_ym[, group_freq := total_weight / sum(total_weight), by=  c("YEAR", "MONTH")]

# CPI-U adjustment based on MISH ---- 
# For instance, income from households reporting fresh income data for 
# October 2023 (i.e., those in months 1 and 5) is adjusted using the November 
# 2022 through October 2023 CPI-U twelve-month average. The income from the 
# remaining groups in the sample are adjusted accordingly.


# Computation of the median hhi ----
# find which bucket the weighted median is in. Basically 0% chance it is on the edge.



bucket_with_median_df <- hhi_dist_by_ym[, 
                                        list(bucket_with_median = weighted.median(n_bucket, total_weight, type = 1)), 
                                        by = c("YEAR", "MONTH")]

total_ym <- nrow(bucket_with_median_df)

# for each year/month:

appx_medians <- vector("list", total_ym)

for (nr in seq_len(total_ym)){
  
  # check the bucket that the median is in.
  current_ym <- bucket_with_median_df[nr,]
  current_year <- current_ym$YEAR[1]
  current_month <- current_ym$MONTH[1]
  current_bucket <- current_ym$bucket_with_median[1]
  
  current_hhi_df <- hhi_dist_by_ym[YEAR == current_year & MONTH == current_month]
  total_values <- sum(current_hhi_df$total_weight)
  
  bucket_before <- current_hhi_df[n_bucket == (current_bucket-1)]
  bucket_in <- current_hhi_df[n_bucket == (current_bucket)]
  
  lower_lim <- as.numeric(bucket_in$ll)
  upper_lim <- as.numeric(bucket_in$ul)
  
  count_before_bin <- as.numeric(bucket_before$count_incl_bin)
  count_in_bin <- as.numeric(bucket_in$total_weight)
  
  prop_before_bin <- as.numeric(bucket_before$cdf_after_bin)
  prop_incl_bin <- as.numeric(bucket_in$cdf_after_bin)
  
  if (current_bucket == max(current_hhi_df$n_bucket)){
    # Pareto interp: https://en.wikipedia.org/wiki/Pareto_interpolation
    # if it is the max, do pareto interp to calc median income
    
    theta_hat <- (log(1 - prop_before_bin) - log(1 - prop_incl_bin)) /
      (log(upper_lim) - log(lower_lim))
    kappa_hat <- ((prop_incl_bin - prop_before_bin)/
                    ((1/lower_lim^(theta_hat))- (1/upper_lim^(theta_hat))))^(1/theta_hat)
    appx_median <- kappa_hat*2^(1/theta_hat)
    
  }else{
    # if it's not the max, do linear interp to calc median income
    
    appx_median <- lower_lim + (upper_lim - lower_lim)*((total_values/2) - count_before_bin) / count_in_bin 
  }
  appx_medians[[nr]] <- appx_median
}

bucket_with_median_df[, date := ymd(paste0(YEAR, "-", MONTH, "-01"))]
bucket_with_median_df[, date := date %m+% months(1) %m-% days(1)]
bucket_with_median_df$hhi <- as.numeric(appx_medians)
bucket_with_median_df <- bucket_with_median_df[order(date)]

# Seasonal adjustment using X-13 ----
hhi_ts <- ts(bucket_with_median_df$hhi,
             start = c(bucket_with_median_df$YEAR[1], bucket_with_median_df$MONTH[1]),
             end = c(bucket_with_median_df$YEAR[total_ym], bucket_with_median_df$MONTH[total_ym]),
             frequency = 12
)

seas_adj_hhi <- seas(hhi_ts)
bucket_with_median_df$sa_hhi <- coredata(final(seas_adj_hhi))

fwrite(bucket_with_median_df, file.path("..", "..", "output", "cps_hhi.csv"))

