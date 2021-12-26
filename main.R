library(magrittr)

config <- yaml::read_yaml("config.yaml")
sapply(list.files(config$paths$R, full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

download_all_data(config = config)

XWALK_COUNTY_FIPS <- xwalk_county_fips()
LU_TSA <- lu_tsa()

CASES <- dm_outcomes("cases")
DEATHS <- dm_outcomes("deaths")
TESTS <- dm_testing()
HOSPITALIZATIONS <- dm_hospitalizations()

VACCINE_ZIP <- dm_vaccine_zip()
