download_data <- function(df_config, 
                          update = FALSE,
                          path_raw = config$paths$data$raw$path) {
  
  fn_full <- generate_fn_full(df_config$url, df_config$fn, path_raw)
  
  if (update | !file.exists(fn_full)) {
    download.file(df_config$url, fn_full)
  }
}

download_all_data <- function(config) {
  lapply(config$dshs_data, download_data)
}

dm_cases <- function(df_config,
                     update = FALSE,
                     path_raw = config$paths$data$raw$path,
                     path_proc = config$paths$data$proc$path,
                     ext = config$paths$data$proc$ext) {
  
  fn_raw <- generate_fn_full(df_config$url, df_config$fn, path_raw)
  fn_proc <- paste0(path_proc, df_config$fn, ext)
  
  if (update | !file.exists(fn_proc)) {
    df <- readxl::read_excel(path = fn_raw, sheet = df_config$sheet, skip = df_config$skip) %>% 
      dplyr::rename(county = `County Name`) %>% 
      tidyr::pivot_longer(cols = -county,
                          names_to = "date",
                          values_to = "cumulative_cases") %>% 
      dplyr::group_by(county) %>% 
      dplyr::mutate(date = stringr::str_remove(date, "Cases") %>% 
                      stringr::str_trim() %>% 
                      lubridate::as_date(format = df_config$date_format),
                    new_cases = cumulative_cases - dplyr::lag(cumulative_cases)) %>% 
      dplyr::ungroup() %>% 
      dplyr::inner_join(XWALK_COUNTY_FIPS) %>% 
      dplyr::select(date, fips, cumulative_cases, new_cases)
  } else {
    df <- readr::read_csv(fn_proc)
  }
  
  readr::write_csv(df, fn_proc)
  df
}
