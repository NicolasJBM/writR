#' Generate a table displaying univariate statistics for all the variables included in x which must be numeric.
#' @param x         Tibble or dataframe. Table with only numeric variables.
#' @param variables Character vector. Names of the variables.
#' @param include   Character vector. Names of the statistics to report: "Count", "Min", "Median", "Mean", "Max", "Range", "St.Dev","Skew" and "Kurt".
#' @return Dataframe. Table of univariate statistics.
#' @importFrom psych skew
#' @importFrom psych kurtosi
#' @export

stat_univariate <- function(x,
                            variables = NULL,
                            include = c("Variable", "Count", "Min", "Median", "Mean", "Max", "Range", "St.Dev", "Skew", "Kurt")) {

  # Reformat to allow processing
  x <- as.data.frame(x)
  colnbr <- ncol(x)
  if (is.null(variables)) variables <- as.vector(names(x))

  # Create and fill in a matrix containing all descriptive statistics about the columns
  var_desc <- as.data.frame(matrix(nrow = colnbr, ncol = 10))
  names(var_desc) <- c("Variable", "Count", "Min", "Med", "Mean", "Max", "Range", "SD", "Skew", "Kurt")
  var_desc[, "Variable"] <- variables
  for (i in 1:colnbr) {
    var_desc[i, "Count"] <- length(na.omit(x[, i]))
    var_desc[i, "Min"] <- min(x[, i], na.rm = TRUE)
    var_desc[i, "Median"] <- median(x[, i], na.rm = TRUE)
    var_desc[i, "Mean"] <- mean(x[, i], na.rm = TRUE)
    var_desc[i, "Max"] <- max(x[, i], na.rm = TRUE)
    var_desc[i, "Range"] <- max(x[, i], na.rm = TRUE) - min(x[, i], na.rm = TRUE)
    var_desc[i, "St.Dev"] <- sd(x[, i], na.rm = TRUE)
    var_desc[i, "Skew"] <- skew(x[, i], na.rm = TRUE)
    var_desc[i, "Kurt"] <- kurtosi(x[, i], na.rm = TRUE)
  }

  var_desc <- var_desc[, include]
  var_desc[, "Variable"] <- paste0(seq_len(nrow(var_desc)), ". ", var_desc$Variable)

  return(var_desc)
}
