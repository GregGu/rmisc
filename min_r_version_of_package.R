min_r_version <- function(package="ggplot2", exclude_main_pkg=TRUE) {
  
  purrr::walk(c("tools", "purrr", "devtools", "stringi", "tidyr", "dplyr"), 
              require, character.only=TRUE)
  
  deps <- package_dependencies(package, recursive=TRUE)
  
  if (exclude_main_pkg) {
    pkgs <- deps[[1]]
  } else {
    pkgs <- c(package, deps[[1]])
  }
  
  available.packages() %>% 
    as_data_frame() %>% 
    filter(Package %in% pkgs) %>% 
    select(Depends)  %>% 
    unlist() -> pkg_list
  
  # if main pkg only relied on core R packages (i.e. pkgs that aren't in CRAN) and we 
  # excluded the pkg itself from the min version calculation, this is an edge case we need
  # to handle.
  
  if (length(pkg_list) == 0) return("Unspecified")
  
  stri_split_regex(pkg_list, "[,]") %>%
    unlist() %>%
    trimws() %>%
    stri_match_all_regex(c("^R$|^R \\(.*\\)$")) %>%
    unlist() %>%
    discard(is.na(.)) %>%
    unique() %>%
    stri_replace_all_regex("[R >=\\(\\)]", "") %>%
    data_frame(vs=.) %>%
    separate(vs, c("a", "b", "c"), fill="right") %>%
    mutate(c=ifelse(is.na(c), 0, c)) %>%
    arrange(a, b, c) %>%
    tail(1) %>%
    unite(min, a:c, sep=".") -> vs
  
  return(vs$min)
  
}

# min_r_version("ggplot2")
# 
# min_r_version("FPEMcountry")

min_r_version("R2jags")
min_r_version("magrittr")
min_r_version("dplyr")
min_r_version("purrr")
min_r_version("rlang")
min_r_version("data.table")
min_r_version("abind")
min_r_version("MCMCvis")
min_r_version("jsonlite")
min_r_version("ggplot2")

