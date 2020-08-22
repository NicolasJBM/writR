#' int formats as an integer.
#' @param x Double or Integer. Number to be properly formated.
#' @return Character. Formatted number.
#' @export

int <- function(x) format(x, big.mark = ",", decimal.mark = ".", scientific = F, digits = 0)