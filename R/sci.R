#' sci format scientifically.
#' @param x Double or Integer. Number to be properly formated.
#' @return Character. Formated number.
#' @export

sci <- function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = T, digits = 2, nsmall = 2)