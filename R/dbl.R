#' dbl format as a number with two digits.
#' @param x Double or Integer. Number to be properly formated.
#' @return Character. Formated number.
#' @export

dbl <- function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = F, digits = 2, nsmall = 2)
