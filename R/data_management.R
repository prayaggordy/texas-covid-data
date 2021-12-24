download_data <- function(df_config, 
                          update = FALSE,
                          path_raw = config$paths$data$raw) {
  
  ext <- stringr::str_split(df_config$url, "\\.")[[1]] %>% 
    tail(n = 1) %>% 
    paste0(".", .)
  fn_full <- paste0(path_raw, df_config$fn, ext)
  
  if (update | !file.exists(fn_full)) {
    download.file(df_config$url, fn_full)
  }
}

download_all_data <- function(config) {
  lapply(config$dshs_data, download_data)
}
