
#' format extractor
#'
#' @param data \emph{\sQuote{Data.frame}}
#'
#' @return \emph{\sQuote{List}}
#'
#' @examples
#' temp <- format_extract(fpemdata::contraceptive_use)
format_extract <- function(data) {
  data <- data %>% as.data.frame()
  format_list <- list()
  for (name in names(data)) {
    format_list[[name]] <- data[[name]] %>% unique() %>% unlist()
    format_list[[paste0(name, "_missing")]] <- data[[name]] %>% is.na() %>% any()
  }
  return(format_list)
}

