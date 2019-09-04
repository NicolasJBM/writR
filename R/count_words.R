#' Count the number of words in a text.
#' @param text Character. The sentence(s) in which the words have to be counted. Only paragraphs can be counted from the clipboard.
#' @return Numeric. Number of words in the text
#' @importFrom purrr map
#' @importFrom buildR format_ascii
#' @importFrom stringr str_split
#' @importFrom utils read.table
#' @export


count_words <- function(text = ""){
  if (.Platform$OS.type == "unix") cb <- "pbpaste" else cb <- "clipboard"
  if (text == ""){
    text <- suppressWarnings(readLines(pipe(cb)))
    if (length(text) == 0) text <- suppressWarnings(read.table(pipe(cb)))
    closeAllConnections()
  } else text <- text
  text <- unlist(purrr::map(text, stringr::str_split, pattern = " "))
  text <- purrr::map(text, buildR::format_ascii)
  text <- unlist(text[text != ""])
  length(text)
}
