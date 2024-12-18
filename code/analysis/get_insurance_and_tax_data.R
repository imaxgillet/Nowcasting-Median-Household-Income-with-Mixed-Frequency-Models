# get_insurance_and_tax_data.R
## Uses IPUMS to get the homeowner insurance and tax data
# Max Gillet, 2024

library(spatstat)
library(ipumsr)
library(data.table)
library(dplyr)
library(tidycensus)
library(stringr)
library(keyring)

# Set up IPUMS API Key ----
tryCatch(
  api_key <<- keyring::key_get("ipums_api"),
  error = function(e) {
    keyring::key_set("ipums_api", prompt = "Type your IPUMS key:")
    api_key <<- keyring::key_get("ipums_api")
    
  }
)

# Pull the ACS Surveys desired
surveys <- c(
  "us2001a", # 2013 ACS	1.0%
  "us2002a", # 2013 ACS	1.0%
  "us2003a", # 2013 ACS	1.0%
  "us2004a", # 2013 ACS	1.0%
  "us2005a", # 2013 ACS	1.0%
  "us2006a", # 2013 ACS	1.0%
  "us2007a", # 2013 ACS	1.0%
  "us2008a", # 2013 ACS	1.0%
  "us2009a", # 2013 ACS	1.0%
  "us2010a", # 2013 ACS	1.0%
  "us2011a", # 2013 ACS	1.0%
  "us2012a", # 2013 ACS	1.0%
  "us2013a", # 2013 ACS	1.0%
  "us2014a", # 2014 ACS	1.0%
  "us2015a", # 2015 ACS	1.0%
  "us2016a", # 2016 ACS	1.0%
  "us2017a", # 2017 ACS	1.0%
  "us2018a", # 2018 ACS	1.0%
  "us2019a", # 2019 ACS	1.0%
  "us2020a", # 2020 ACS	1.0%	Uses experimental weights to correct for the effects of the COVID-19 pandemic on the 2020 ACS data collection
  "us2021a", # 2021 ACS	1.0%
  "us2022a"#, # 2022 ACS 1.0%
)

vars_to_pull <- list(
  var_spec("YEAR"),
  var_spec("SAMPLE"),
  var_spec("SERIAL"),
  var_spec("CBSERIAL"),
  var_spec("HHWT"),
  var_spec("PERNUM"),
  var_spec("OWNERSHP", case_selections = c("13"), case_selection_type = "detailed"),
  var_spec("MORTGAGE"),
  var_spec("OWNERSHPD"),
  var_spec("PROPINSR"),
  var_spec("PROPTX99"),
  var_spec("VALUEH")
)

insr_tax_extract <-
  define_extract_usa(
    description = "property_tax_and_insurance",
    samples = surveys,
    variables = vars_to_pull,
    data_format = "fixed_width"
  )
submitted_extract <- submit_extract(insr_tax_extract,
                                    api_key = api_key)
downloadable_extract <- wait_for_extract(submitted_extract,
                                         api_key = api_key)
dir.create("data")
data_files <- download_extract(downloadable_extract,
                               api_key = api_key,
                               download_dir = "./data/",
                               overwrite = T)

data_file <- read_ipums_micro(data_files)
var_info <- ipums_var_info(data_file)
setDT(data_file)
data_file <- data_file[PERNUM == 1]

# proptx99 map
proxtx99_map <- var_info %>%
  filter(var_name == "PROPTX99") %>%
  pull(val_labels)

proxtx99_map <- proxtx99_map[[1]]
setDT(proxtx99_map)
proxtx99_map[, c("lbl1", "lbl2") := tstrsplit(lbl, "[-())]", keep = c(1,2))]
proxtx99_map[, lbl1 := as.numeric(gsub("[^0-9]","", lbl1))]
proxtx99_map[, lbl2 := as.numeric(gsub("[^0-9]","", lbl2))]

proxtx99_map$proptx99_value <- rowMeans(proxtx99_map[,c("lbl1", "lbl2")],na.rm = T)
proxtx99_map[, proptx99_value := ifelse(lbl == "None", 0, proptx99_value)]
proxtx99_map[, PROPTX99 := val]

data_file[, PROPINSR := ifelse(PROPINSR == 1, 0, PROPINSR)]

data_file <- merge(data_file, proxtx99_map[,c("PROPTX99", "proptx99_value")], by = "PROPTX99", all.x = T, all.y = F)

new_property_ratios <- data_file[, list(
  
  n = .N,
  median_propinsr = weighted.median(PROPINSR, HHWT),
  median_valueh = weighted.median(as.numeric(VALUEH), HHWT),
  median_proptx99  = weighted.median(proptx99_value, HHWT)
  
) , by = c("SAMPLE")]

new_property_ratios[, year := as.numeric(substr(as.character(SAMPLE), 1, 4))]
new_property_ratios[, sample_code := as.numeric(substr(as.character(SAMPLE), 5, 6))]
new_property_ratios[, acs := ifelse(sample_code == 1, 1, 5)]
new_property_ratios[, median_retax_ratio :=  median_proptx99 / median_valueh]
new_property_ratios[, median_propinsr_ratio := median_propinsr / median_valueh]


new_property_ratios <- new_property_ratios[, c("year", "acs", 
                                               "median_propinsr",
                                               "median_valueh", 
                                               "median_proptx99", 
                                               "median_retax_ratio", 
                                               "median_propinsr_ratio", 
                                               "n")]
new_property_ratios <- new_property_ratios[order(year, acs)]
fwrite(new_property_ratios, "./data/autogen_ipums_nationwide_property_ratios.csv")
