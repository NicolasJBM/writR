#' @name table_univariate
#' @title Report univariate statistics
#' @author Nicolas Mangin
#' @description Generate a table displaying univariate statistics for all the variables included in x which must be numeric.
#' @param x         Tibble or dataframe. Table with only numeric variables.
#' @param variables Character vector. Names of the variables.
#' @param include   Character vector. Names of the statistics to report: "Count", "Min", "Median", "Mean", "Max", "Range", "St.Dev","Skew" and "Kurt".
#' @param digits       Numeric. Number of digits to be displayed in the formated table.
#' @return Dataframe. Table of univariate statistics.
#' @importFrom psych kurtosi
#' @importFrom psych skew
#' @export

table_univariate <- function(x,
                            variables = NULL,
                            include = c("Variable", "Count", "Min", "Median", "Mean", "Max", "Range", "St.Dev", "Skew", "Kurt"),
                            digits = 3) {

  # Reformat to allow processing
  x <- base::as.data.frame(x)
  colnbr <- base::ncol(x)
  if (base::is.null(variables)) variables <- base::as.vector(base::names(x))

  # Create and fill in a matrix containing all descriptive statistics about the columns
  var_desc <- base::as.data.frame(base::matrix(nrow = colnbr, ncol = 10))
  base::names(var_desc) <- c("Variable", "Count", "Min", "Med", "Mean", "Max", "Range", "SD", "Skew", "Kurt")
  var_desc[, "Variable"] <- variables
  for (i in 1:colnbr) {
    var_desc[i, "Count"] <- base::round(base::length(stats::na.omit(x[, i])), digits)
    var_desc[i, "Min"] <- base::round(base::min(x[, i], na.rm = TRUE), digits)
    var_desc[i, "Median"] <- base::round(stats::median(x[, i], na.rm = TRUE), digits)
    var_desc[i, "Mean"] <- base::round(base::mean(x[, i], na.rm = TRUE), digits)
    var_desc[i, "Max"] <- base::round(base::max(x[, i], na.rm = TRUE), digits)
    var_desc[i, "Range"] <- base::round(base::max(x[, i], na.rm = TRUE) - min(x[, i], na.rm = TRUE), digits)
    var_desc[i, "St.Dev"] <- base::round(stats::sd(x[, i], na.rm = TRUE), digits)
    var_desc[i, "Skew"] <- base::round(psych::skew(x[, i], na.rm = TRUE), digits)
    var_desc[i, "Kurt"] <- base::round(psych::kurtosi(x[, i], na.rm = TRUE), digits)
  }

  var_desc <- var_desc[, include]
  var_desc[, "Variable"] <- base::paste0(base::seq_len(base::nrow(var_desc)), ". ", var_desc$Variable)

  return(var_desc)
}
