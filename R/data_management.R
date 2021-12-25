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

dm_outcomes <- function(outcome_type,
                        df_config = config$dshs_data,
                        update = TRUE,
                        path_raw = config$paths$data$raw$path,
                        path_proc = config$paths$data$proc$path,
                        ext = config$paths$data$proc$ext) {
  
  df_config <- df_config[[outcome_type]]
  
  fn_raw <- generate_fn_full(df_config$url, df_config$fn, path_raw)
  fn_proc <- paste0(path_proc, df_config$fn, ext)
  
  if (update | !file.exists(fn_proc)) {
    cns <- paste(c("cumulative", "new"), "outcomes", sep = "_")
    names(cns) <- paste(c("cumulative", "new"), outcome_type, sep = "_")
    df <- readxl::read_excel(path = fn_raw, sheet = df_config$sheet, skip = df_config$skip) %>% 
      dplyr::rename(county = `County Name`) %>% 
      tidyr::pivot_longer(cols = -county,
                          names_to = "date",
                          values_to = "cumulative_outcomes") %>% 
      dplyr::mutate(date = stringr::str_remove(date, "[A-z\\s]*") %>% 
                      lubridate::as_date(format = df_config$date_format),
                    county = tolower(county)) %>% 
      dplyr::group_by(county) %>% 
      dplyr::arrange(date) %>% 
      dplyr::mutate(new_outcomes = cumulative_outcomes - dplyr::lag(cumulative_outcomes)) %>% 
      dplyr::ungroup() %>% 
      dplyr::inner_join(XWALK_COUNTY_FIPS) %>% 
      dplyr::select(date, fips, cumulative_outcomes, new_outcomes) %>% 
      dplyr::rename(cns)
    readr::write_csv(df, fn_proc)
  } else {
    df <- readr::read_csv(fn_proc)
  }
  
  df
}



