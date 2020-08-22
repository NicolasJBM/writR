#' Add totals (sums or means) to a dataframe.
#' @param x        Dataframe. Data to which you wish to add the totals
#' @param rows     Logical. Whether totals should be added at the bottom of the table.
#' @param columns  Logical. Whether totals should be added at the right-side of the table.
#' @param omit_col Character vector. Names of the variables not to be included in the total (for instance containing the id of the observation).
#' @param summary  Character. Whether the reported statistic should be a "sum" or a "mean".
#' @return Tibble with the summary statistics added as a column or a row.
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @export


stat_totals <- function(x,
                        rows = TRUE,
                        columns = TRUE,
                        omit_col = NA,
                        summary = "sum") {
  if (rows) {
    y <- dplyr::select(x, -omit_col)
    if (summary == "sum") {
      srows <- as.data.frame(t(colSums(y, na.rm = TRUE)))
    } else {
      srows <- as.data.frame(t(colMeans(y, na.rm = TRUE)))
    }
    x <- dplyr::bind_rows(x, srows)
  }

  if (columns) {
    y <- dplyr::select(x, -omit_col)
    if (summary == "sum") {
      x[, "Total"] <- rowSums(y, na.rm = TRUE)
    } else {
      x[, "Total"] <- rowMeans(y, na.rm = TRUE)
    }
  }

  dplyr::select(x, omit_col, everything())
}
