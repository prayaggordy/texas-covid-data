library(magrittr)

config <- yaml::read_yaml("config.yaml")
sapply(list.files(config$paths$R, full.names = TRUE, recursive = TRUE), source, .GlobalEnv)

download_all_data(config = config)
