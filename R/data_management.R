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
                        write_to_proc = TRUE,
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
      dplyr::rename(county = 1) %>% 
      dplyr::mutate(dplyr::across(c(tidyselect:::where(is.character), -county), readr::parse_number)) %>% 
      tidyr::pivot_longer(cols = -county,
                          names_to = "date",
                          values_to = "cumulative_outcomes") %>%
      dplyr::mutate(date = stringr::str_remove(date,
                                               paste0("[(", df_config$date_prefix, ")",
                                                      paste0("(", df_config$extra_removal, ")", collapse = ""),
                                                      "]*")) %>%
                      stringr::str_trim() %>%
                      lubridate::as_date(format = df_config$date_format),
                    county = tolower(county)) %>%
      dplyr::group_by(county) %>%
      dplyr::arrange(date) %>%
      dplyr::mutate(new_outcomes = cumulative_outcomes - dplyr::lag(cumulative_outcomes)) %>%
      dplyr::ungroup() %>%
      dplyr::inner_join(XWALK_COUNTY_FIPS) %>%
      dplyr::select(date, fips, cumulative_outcomes, new_outcomes) %>%
      dplyr::rename(cns)
    
    if (write_to_proc) {
      readr::write_csv(df, fn_proc)
    }
  } else {
    df <- readr::read_csv(fn_proc)
  }
  
  df
}

dm_testing <- function(df_config = config$dshs_data$testing,
                       df_config_legacy = config$dshs_data$testing_legacy,
                       update = TRUE,
                       path_raw = config$paths$data$raw$path,
                       path_proc = config$paths$data$proc$path,
                       ext = config$paths$data$proc$ext) {
  
  fn_raw <- generate_fn_full(df_config$url, df_config$fn, path_raw)
  fn_raw_legacy <- generate_fn_full(df_config_legacy$url, df_config_legacy$fn, path_raw)
  fn_proc <- paste0(path_proc, df_config$fn, ext)
  
  if (update | !file.exists(fn_proc)) {
    supp_args <- list(update = update, write_to_proc = FALSE, path_raw = path_raw, path_proc = path_proc, ext = ext)
    
    df <- do.call(dm_outcomes, append(supp_args, c(outcome_type = "testing_legacy"))) %>% 
      dplyr::rename(cumulative_testing = cumulative_testing_legacy, new_testing = new_testing_legacy) %>% 
      dplyr::mutate(date = date - lubridate::years(1)) %>% 
      dplyr::bind_rows(do.call(dm_outcomes, append(supp_args, c(outcome_type = "testing")))) %>% 
      dplyr::group_by(fips) %>% 
      dplyr::arrange(date) %>% 
      dplyr::ungroup()
    
    readr::write_csv(df, fn_proc)
  } else {
    df <- readr::read_csv(fn_proc)
  }
  
  df
}

