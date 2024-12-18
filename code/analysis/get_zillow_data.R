# get_zillow_data.R
# Downloads ZHVI from Zillow's website
# Max Gillet, 2024

midtier_zhvi_url <- "https://files.zillowstatic.com/research/public_csvs/zhvi/Metro_zhvi_uc_sfrcondo_tier_0.33_0.67_sm_sa_month.csv"
dir.create("data")
download.file(midtier_zhvi_url, destfile = "./data/midtier_zhvi.csv", mode = 'wb')
