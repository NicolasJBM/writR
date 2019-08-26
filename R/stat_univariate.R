#' Generate a table displaying variables univariate statistics
#' @param x         Tibble or dataframe. Table with only numeric variables.
#' @param variables Character vector. Names of the variables.
#' @param include   Character vector. Names of the statistics to report (see the call of the function for the availabel statistics).
#' @return Dataframe. Table of univariate statistics.
#' @importFrom psych skew
#' @importFrom psych kurtosi
#' @export

stat_univariate <- function(x,
                       variables=NULL,
                       include = c("Variable", "Count", "Min", "Median", "Mean", "Max", "Range", "St.Dev","Skew","Kurt")) {
  
  # Reformat to allow processing
  x <- as.data.frame(x)
  colnbr <- ncol(x)
  if (is.null(variables)) variables <- as.vector(names(x))

  # Create and fill in a matrix containing all descriptive statistics about the columns
  varDesc <- as.data.frame(matrix(nrow = colnbr, ncol = 10))
  names(varDesc) <- c("Variable", "Count", "Min", "Med", "Mean", "Max", "Range", "SD", "Skew", "Kurt")
  varDesc$Variable <- variables
  for (i in 1:colnbr) {
    varDesc[i, "Count"] <- length(na.omit(x[, i]))
    varDesc[i, "Min"] <- min(x[, i], na.rm = TRUE)
    varDesc[i, "Median"] <- median(x[, i], na.rm = TRUE)
    varDesc[i, "Mean"] <- mean(x[, i], na.rm = TRUE)
    varDesc[i, "Max"] <- max(x[, i], na.rm = TRUE)
    varDesc[i, "Range"] <- max(x[, i], na.rm = TRUE) - min(x[, i], na.rm = TRUE)
    varDesc[i, "St.Dev"] <- sd(x[, i], na.rm = TRUE)
    varDesc[i, "Skew"] <- skew(x[, i], na.rm = TRUE)
    varDesc[i, "Kurt"] <- kurtosi(x[, i], na.rm = TRUE)
  }
  
  varDesc <- varDesc[,include]
  varDesc$Variable <- paste0(1:nrow(varDesc), ". ", varDesc$Variable)
  
  return(varDesc)
}