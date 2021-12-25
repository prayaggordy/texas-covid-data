xwalk_county_fips <- function() {
  tigris::fips_codes %>% 
    dplyr::filter(state == "TX") %>% 
    dplyr::mutate(county = stringr::str_remove(county, "County") %>% 
                    stringr::str_trim() %>% 
                    tolower(), 
                  fips = paste0(state_code, county_code)) %>% 
    dplyr::select(county, fips) %>% 
    tibble::as_tibble()
}

lu_tsa <- function() {
  tibble::tribble(
    ~"TSA", ~"name",
    "A", "Amarillo",
    "B", "Lubbock",
    "C", "Wichita Falls",
    "D", "Abilene",
    "E", "Dallas/Ft. Worth",
    "F", "Paris",
    "G", "Longview/Tyler",
    "H", "Lufkin",
    "I", "El Paso",
    "J", "Midland/Odessa",
    "K", "San Angelo",
    "L", "Belton/Killeen",
    "M", "Waco",
    "N", "Bryan/College Station",
    "O", "Austin",
    "P", "San Antonio",
    "Q", "Houston",
    "R", "Galveston",
    "S", "Victoria",
    "T", "Laredo",
    "U", "Corpus Christi",
    "V", "Lower Rio Grande Valley"
  )
}