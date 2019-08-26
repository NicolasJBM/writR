#' pct transforms numbers between 0 and 1 in percentages.
#' @param x Double. Number to be properly formated.
#' @return Character. Formated number.
#' @export

pct <- function(x) format(x * 100, big.mark = ",", decimal.mark = ".", scientific = F, digits = 2, nsmall = 2)