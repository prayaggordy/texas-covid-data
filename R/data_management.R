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

dm_outcomes_helper <- function(df, date_prefix, extra_removal, date_format) {
  df %>% 
    dplyr::rename(geography = 1) %>% 
    dplyr::mutate(dplyr::across(c(tidyselect:::where(is.character), -geography), readr::parse_number)) %>% 
    tidyr::pivot_longer(cols = -geography,
                        names_to = "date",
                        values_to = "cumulative_outcomes") %>%
    dplyr::mutate(date = stringr::str_remove(date,
                                             paste0("[(", date_prefix, ")",
                                                    paste0("(", extra_removal, ")", collapse = ""),
                                                    "]*")) %>%
                    stringr::str_trim() %>%
                    lubridate::as_date(format = date_format),
                  geography = tolower(geography)) %>%
    dplyr::group_by(geography) %>%
    dplyr::arrange(date) %>%
    dplyr::mutate(new_outcomes = cumulative_outcomes - dplyr::lag(cumulative_outcomes)) %>%
    dplyr::ungroup()
}

dm_outcomes_full <- function(url = "", fn = "", sheet = 0, skip = 0, 
                               date_prefix = "", extra_removal = "", date_format = "", 
                               outcome_type, update, write_to_proc, 
                               path_raw, path_proc, ext,
                               ...) {
  
  fn_raw <- generate_fn_full(url, fn, path_raw)
  fn_proc <- paste0(path_proc, fn, ext)
  
  if (update | !file.exists(fn_proc)) {
    cns <- paste(c("cumulative", "new"), "outcomes", sep = "_")
    names(cns) <- paste(c("cumulative", "new"), outcome_type, sep = "_")
    
    df <- readxl::read_excel(path = fn_raw, sheet = sheet, skip = skip) %>% 
      dm_outcomes_helper(date_prefix, extra_removal, date_format) %>% 
      dplyr::rename(county = geography) %>% 
      dplyr::inner_join(XWALK_COUNTY_FIPS, by = "county") %>%
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

dm_outcomes <- function(outcome_type,
                        df_config = config$dshs_data,
                        update = TRUE,
                        write_to_proc = TRUE,
                        path_raw = config$paths$data$raw$path,
                        path_proc = config$paths$data$proc$path,
                        ext = config$paths$data$proc$ext) {
  
  df_config <- df_config[[outcome_type]]
  
  all_args <- list(df_config, 
                   outcome_type = outcome_type,
                   update = update,
                   write_to_proc = write_to_proc,
                   path_raw = path_raw,
                   path_proc = path_proc,
                   ext = ext) %>% 
    purrr::flatten()
  
  do.call(dm_outcomes_full, all_args)
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

dm_hospitalizations <- function(df_config = config$dshs_data$hospitalizations,
                                update = TRUE,
                                path_raw = config$paths$data$raw$path,
                                path_proc = config$paths$data$proc$path,
                                ext = config$paths$data$proc$ext) {
  
  fn_raw <- generate_fn_full(df_config$url, df_config$fn, path_raw)
  fn_proc <- paste0(path_proc, df_config$fn, ext)
  
  if (update | !file.exists(fn_proc)) {
    df <- lapply(names(df_config$sheets),
                 function(x) {
                   cns <- paste(c("cumulative", "new"), "outcomes", sep = "_")
                   names(cns) <- paste(c("cumulative", "new"), x, sep = "_")
                   
                   readxl::read_excel(path = fn_raw, 
                                      sheet = df_config$sheets[[x]]$sheet, 
                                      skip = df_config$sheets[[x]]$skip) %>% 
                     dm_outcomes_helper(date_prefix = df_config$date_prefix, 
                                        extra_removal = df_config$extra_removal, 
                                        date_format = df_config$date_format) %>% 
                     dplyr::rename(TSA = geography, cns) %>% 
                     dplyr::mutate(TSA = stringr::str_remove(TSA, "[^[A-z]]") %>% 
                                     toupper()) %>% 
                     dplyr::inner_join(LU_TSA %>% dplyr::select(TSA), by = "TSA")
                 }) %>% 
      purrr::reduce(dplyr::inner_join, by = c("date", "TSA"))
    
    readr::write_csv(df, fn_proc)
  } else {
    df <- readr::read_csv(fn_proc)
  }
  
  df
}

dm_vaccine_zip <- function(df_config = config$dshs_data$vaccine_zip_today,
                           update = TRUE,
                           path_raw = config$paths$data$raw$path,
                           path_proc = config$paths$data$proc$path,
                           ext = config$paths$data$proc$ext) {
  
  fn_raw <- generate_fn_full(df_config$url, df_config$fn, path_raw)
  fn_proc <- paste0(path_proc, df_config$fn, ext)
  
  if (update | !file.exists(fn_proc)) {
    df <- readxl::read_excel(path = fn_raw, sheet = df_config$sheet, skip = df_config$skip) %>% 
      dplyr::rename(df_config$cns %>% unlist()) %>% 
      dplyr::filter(stringr::str_detect(zip, "^[:digit:]+$")) %>%
      dplyr::mutate(dplyr::across(-zip, as.numeric))
    
    readr::write_csv(df, fn_proc)
  } else {
    df <- readr::read_csv(fn_proc)
  }
  
  df
}

dm_vaccine_county <- function(df_config = config$dshs_data$vaccine_county_today,
                              update = TRUE,
                              path_raw = config$paths$data$raw$path,
                              path_proc = config$paths$data$proc$path,
                              ext = config$paths$data$proc$ext) {
  
  fn_raw <- generate_fn_full(df_config$url, df_config$fn, path_raw)
  fn_proc <- paste0(path_proc, df_config$fn, ext)
  
  if (update | !file.exists(fn_proc)) {
    df <- readxl::read_excel(path = fn_raw, sheet = df_config$sheet, skip = df_config$skip) %>% 
      dplyr::rename(df_config$cns %>% unlist()) %>% 
      dplyr::mutate(county = tolower(county)) %>% 
      dplyr::inner_join(XWALK_COUNTY_FIPS, by = "county") %>% 
      dplyr::select(fips, tidyselect::all_of(names(df_config$cns)), -county)
    
    readr::write_csv(df, fn_proc)
  } else {
    df <- readr::read_csv(fn_proc)
  }
  
  df
}

dm_vaccine_state <- function(df_config = config$dshs_data$vaccine_state_historical,
                              update = TRUE,
                              path_raw = config$paths$data$raw$path,
                              path_proc = config$paths$data$proc$path,
                              ext = config$paths$data$proc$ext) {
  
  fn_raw <- generate_fn_full(df_config$url, df_config$fn, path_raw)
  fn_proc <- paste0(path_proc, df_config$fn, ext)
  
  if (update | !file.exists(fn_proc)) {
    df <- readxl::read_excel(path = fn_raw, sheet = df_config$sheet, skip = df_config$skip) %>% 
      dplyr::rename(df_config$cns %>% unlist()) %>% 
      dplyr::mutate(date = lubridate::as_date(date, format = "%m/%d/%y"))
    
    readr::write_csv(df, fn_proc)
  } else {
    df <- readr::read_csv(fn_proc)
  }
  
  df
}

