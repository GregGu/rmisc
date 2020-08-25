#' document_data
#'
#' automatically generates roxygen skeleton for data use base r's "str" function
#' 
#' @param df 
#'
#' @export
document_data <- function (df) {
  title <- substitute(df)
  output <- c(paste("#'", title),
              "#' @format data.frame", 
              gsub(pattern ="^", replacement = "#'", capture.output(str(df))) %>% 
                gsub(pattern = "[$]", replacement = "/item{") %>%
                gsub(pattern = "[:]", replacement = "}") %>%
                gsub(pattern = "\\snum\\s", replacement = "{/emph{/sQuote{Numeric}}}", ignore.case = FALSE) %>%
                gsub(pattern = "\\slogi\\s", replacement = "{/emph{/sQuote{Logical}}}", ignore.case = FALSE) %>%
                gsub(pattern = "\\schr\\s", replacement = "{/emph{/sQuote{Character}}}", ignore.case = FALSE) %>%
                gsub(pattern = "[/]", replacement = "\\\\") %>% 
                gsub(pattern = "[ ]", replacement = ""), 
              dQuote(title))
  cat(output, sep="\n")
}


# This is where I started to work ... to be continued
# example <- "$ isonum    : num NA NA 5"
# library(dplyr)
# gsub(pattern = "[$]", replacement = "/item{" , x = example) %>%
#   gsub(pattern = "[:]", replacement = "}") %>%
#   gsub(pattern = "\\snum\\s", replacement = "{/emph{/sQuote{Numeric}}}", ignore.case = FALSE) %>%
#   gsub(pattern = "\\slog\\s", replacement = "{/emph{/sQuote{logical}}}", ignore.case = FALSE) %>%
#   gsub(pattern = "\\schr\\s", replacement = "{/emph{/sQuote{Character}}}", ignore.case = FALSE) %>%
#   gsub(pattern = "[ ]", replacement = "")
# 
