# get_freddie_data.R
# Downloads mortgage rate data from Freddie Mac's website
# Max Gillet, 2024

freddie_url <- "https://www.freddiemac.com/pmms/docs/PMMS_history.csv"
dir.create("data")
download.file(freddie_url, destfile = "./data/freddie_pmrate.csv", mode = 'wb')
