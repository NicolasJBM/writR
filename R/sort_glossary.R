#' Sort the entries of a glossary at the end of a book.
#' @param glossary character string. Address of the glossary to sort.
#' @importFrom dplyr arrange
#' @importFrom dplyr mutate
#' @importFrom dplyr group_by
#' @importFrom dplyr summarise
#' @importFrom tidyr unite
#' @importFrom stringr str_detect
#' @importFrom stringr str_remove_all
#' @importFrom purrr map_dbl
#' @export

sort_glossary <- function(glossary) {
  
  V1 <- NULL
  V2 <- NULL
  first <- NULL
  
  data <- readLines(glossary) %>%
    stringr::str_remove_all(" \\\\newline")
  
  # Extract and remove header
  title <- data[c(1:3)]
  data <- data[-c(1:3)]
  
  # Remove lines with less than 2 character
  data <- data[!stringr::str_detect(data, "\\*\\*")] 
  data <- data[purrr::map_dbl(data, nchar) > 1] 
  
  # Indexing lines
  data <- matrix(data = data, ncol = 2, byrow = TRUE) %>%
    as.data.frame() %>%
    dplyr::arrange(V1) %>%
    tidyr::unite(glossary, V1, V2, sep = "\n") %>%
    dplyr::mutate(first = paste0("\n \n**", substr(glossary,1,1), "**")) %>%
    dplyr::group_by(first) %>%
    dplyr::summarise(glossary = paste(glossary, collapse = " \\newline \n \n")) %>%
    dplyr::mutate(glossary = paste(glossary, " \\newline \n \n", sep = "")) %>%
    tidyr::unite(glossary, first, glossary, sep = " \\newline \n \n")
  
  # Rewrite rmd
  data <- c(title, data$glossary)
  writeLines(data, glossary)
}
