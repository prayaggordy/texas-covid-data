generate_fn_full <- function(url, fn, path_raw) {
  ext <- stringr::str_split(url, "\\.")[[1]] %>% 
    tail(n = 1) %>% 
    paste0(".", .)
  paste0(path_raw, fn, ext)
}