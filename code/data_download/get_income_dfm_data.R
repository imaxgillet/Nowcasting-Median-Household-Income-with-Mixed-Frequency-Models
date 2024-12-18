# get_income_dfm_data.R ----
# Gets all data for the household dfm
# Run this in the code/data_download/ folder
# If it breaks, you can restart it manually from where it left off by changing the
# all_hh_income_dates variable
# Max Gillet, 2024

library(alfred)
library(data.table)
library(lubridate)
library(tidyr)
library(keyring)

# parameters ----
get_backtest_data <- TRUE

# set fred key ----
tryCatch(
  fred_key <<- keyring::key_get("fred_api"),
  error = function(e) {
    keyring::key_set("fred_api", prompt = "Type your FRED API key:")
    fred_key <<- keyring::key_get("fred_api")
    
  }
)

# functions ----
get_alfred_most_recent <- function(alfred_code) {
  df <-
    alfred::get_alfred_series(alfred_code, realtime_start = Sys.Date() - 3 *
                                365, api_key = fred_key)
  df <- as.data.table(df)
  df <- df[realtime_period == max(realtime_period)]
  return(df)
}

get_alfred_asof_date <- function(alfred_code, asof_date) {
  df <- alfred::get_alfred_series(alfred_code,
                                  realtime_start = asof_date - 3 * 365,
                                  realtime_end = asof_date,
                                  api_key = fred_key)
  df <- as.data.table(df)
  df <- df[realtime_period == max(realtime_period)]
  return(df)
}

get_fred_series <- function(fred_code) {
  df <- alfred::get_fred_series(fred_code, api_key = fred_key)
  df <- as.data.table(df)
  df <- df[, realtime_period := max(as.Date(df$date))]
  return(df)
}

get_data <- function(data_spec, asof_date = NULL){
  
  raw_data_tables <- vector("list", nrow(data_spec)) 
  
  for (nr in seq_len(nrow(data_spec))){
    current_row <- data_spec[nr,]
    
    # Pull the data
    if (current_row$db == "ALFRED"){
      
      if (is.null(asof_date)){
        
        current_data <- tryCatch(
          get_alfred_most_recent(current_row$db_code),
          error = function(e){
            print(e)
            Sys.sleep(120)
            get_alfred_most_recent(current_row$db_code)
          }
        )
        
      }else{
        
        
        
        first_release_date <- mdy(current_row$first_alfred_release_date)
        if (asof_date >= first_release_date){
          # if the data is after the first alfred release date, pull it
          
          current_data <- tryCatch(
            get_alfred_asof_date(current_row$db_code, 
                                 asof_date = asof_date),
            error = function(e){
              print(e)
              Sys.sleep(120)
              get_alfred_asof_date(current_row$db_code, 
                                   asof_date = asof_date)
            }
          )
          
        }else{
          # otherwise, pull the data as of the first alfred release and
          # lag it - which will account for data latency but not revisions
          
          current_data <- tryCatch(
            get_alfred_asof_date(current_row$db_code, 
                                 asof_date = first_release_date),
            error = function(e){
              print(e)
              Sys.sleep(120)
              get_alfred_asof_date(current_row$db_code, 
                                   asof_date = first_release_date)
            }
          )
          
          
          if (current_row$freq == "D"){
            last_known_date <- asof_date - lubridate::days(current_row$max_release_lag)
            
          } else if (current_row$freq == "M"){
            last_known_date <- asof_date - months(current_row$max_release_lag)
            
          } else if (current_row$freq == "Q"){
            
            last_known_date <- asof_date - months(3*current_row$max_release_lag)
            
          } else if (current_row$freq == "A"){
            
            last_known_date <- asof_date - years(current_row$max_release_lag)
            
            
          } else {
            stop("freq not recognized for ", current_row$freq)
          }
          
          current_data <- current_data[date <= last_known_date]
          
          warning(
            current_row$db_code,
            " is not available in ALFRED for ",
            strftime(asof_date),
            ". Pulling as of ",
            strftime(first_release_date),
            " and ignoring final ",
            current_row$max_release_lag,
            " periods before requested as of date. ",
            "The last known data is as of ",
            strftime(max(current_data$date)),
            "."
          )
          
        }
        
        
        
        
        
      }
      
    }else if (current_row$db == "FRED"){
      
      
      current_data <- tryCatch(
        get_fred_series(current_row$db_code),
        error = function(e){
          print(e)
          Sys.sleep(120)
          get_fred_series(current_row$db_code)
        }
      )
      
      if (!is.null(asof_date)){
        current_data <- current_data[date <= asof_date]
      }
      
    }else{
      stop("DB not recognized for ", current_row$code)
    }
    
    # incorporate the codes
    colnames(current_data)[3] <- "value"
    current_data$code <- current_row$code
    
    
    # Handle the frequency
    if (current_row$freq == "D"){
      current_data[, yearmo := as.Date(strftime(date, "%Y-%m-01"))]
      current_data <- current_data[!is.na(value)]
      current_data[, max_date := max(date), by = "yearmo"]
      current_data <- current_data[date == max_date]
      current_data[, date := yearmo]
    } else if (current_row$freq == "M"){
      current_data[, date := as.Date(date)]
    } else if (current_row$freq == "Q"){
      current_data[, date := as.Date(date) + months(2)]
    } else if (current_row$freq == "A"){
      current_data[, date := as.Date(date) + months(11)]
    } else {
      stop("freq not recognized for ", current_row$freq)
    }
    
    # Handle the transformations:
    if (current_row$transformation == "DLN"){
      
      current_data[, value := c(NA, diff(log(value)))]
      
    } else if (current_row$transformation == "DLV"){
      
      current_data[, value := c(NA, diff(value))]
      
    } else if (current_row$transformation == "LV"){
      
      current_data[, value := value]
      
    } else {
      stop("transformation not recognized for ", current_row$transformation)
    }
    
    raw_data_tables[[nr]] <- current_data
    
  }
  
  data <- rbindlist(
    raw_data_tables, use.names = T, fill = T
  )
  
  # to EOM
  data[, date := date %m+% months(1) %m-% days(1)]
  data <- data[, c("date", "code", "value")]
  data <- pivot_wider(data, id_cols = "date", names_from = "code")
  data <- as.data.table(data)
  data <- data[order(date)]
  return(data)
}

# set up output paths ----
data_path <- file.path("..", "..", "data")
dir.create(data_path)

# get CURRENT data ----
data_spec <- data.table::fread(file.path("..", "..", "input_data.csv"))
data <- get_data(data_spec)
fwrite(data, file.path(data_path,"data.csv"))


# Now get the backtesting data (one day before release of HH Median Income) ----
if (get_backtest_data){
  
  # Need to get dates of HHI release
  all_hh_income <- alfred::get_alfred_series("MEHOINUSA646N",
                                             api_key = fred_key)
  all_hh_income <- as.data.table(all_hh_income)
  
  hh_income_dates <- unique(all_hh_income$realtime_period)
  
  # for year-months that are not in ALFRED, assume the release is 2nd tuesday in Sept
  
  prev_hh_income_dates <-
    sapply(seq(2000, min(year(hh_income_dates)) - 1), function(y) {
      all_sept_days <- seq(as.Date(paste0(y, "-09-01")),
                           as.Date(paste0(y, "-09-21")),
                           by = "1 day")
      second_tuesday <- which(weekdays(all_sept_days) == "Tuesday")[2]
      return(as.character(all_sept_days[second_tuesday]))
    })
  
  all_hh_income_dates <-
    c(as.Date(prev_hh_income_dates), hh_income_dates)
  
  # start in 2016 as this is the first year where ALFRED has all the data releases
  all_hh_income_dates <- all_hh_income_dates[all_hh_income_dates >= "2012-01-01"]
  all_hh_income_values <- vector("list", length(all_hh_income_dates))
  for (nd in seq_along(all_hh_income_dates)) {
    current_obs_date <- all_hh_income_dates[[nd]]
    current_obs_date <- current_obs_date - 1
    
    udata <- get_data(data_spec, asof_date = current_obs_date)
    
    fwrite(udata, file.path(data_path,
                            paste0(strftime(current_obs_date, "%Y%m%d"),
                                   "_data.csv")))
    all_hh_income_values[[nd]] <- udata
  }
  
}
