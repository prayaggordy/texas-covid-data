xwalk_county_fips <- function() {
  tigris::fips_codes %>% 
    dplyr::filter(state == "TX") %>% 
    dplyr::mutate(county = stringr::str_remove(county, "County") %>% 
                    stringr::str_trim(), 
                  fips = paste0(state_code, county_code)) %>% 
    dplyr::select(county, fips) %>% 
    tibble::as_tibble()
}