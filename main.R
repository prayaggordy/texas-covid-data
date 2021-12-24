library(magrittr)

config <- yaml::read_yaml("config.yaml")
sapply(list.files(config$paths$R, full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

download_all_data(config = config)

XWALK_COUNTY_FIPS <- xwalk_county_fips()
CASES <- dm_cases(df_config = config$dshs_data$cases)
