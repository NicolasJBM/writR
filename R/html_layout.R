#' @name html_layout
#' @title Insert a Grid Layout
#' @author Nicolas Mangin
#' @description Create a HTML grid layout
#' @param layout list of vectors. one vector per row, the integers in the vector specifying the relative width of each cell of this row.
#' @return Character. Code for HTML grid.
#' @export


html_layout <- function(layout = list(c(1, 2, 1), c(3, 1), c(1, 1, 1, 1))) {
  k <- 1
  for (i in seq_len(length(layout))) {
    cells <- layout[[i]]
    for (j in seq_len(length(cells))) {
      cells[[j]] <- html_cell(cells[[j]], k)
      k <- k + 1
    }
    layout[[i]] <- html_row(cells)
    k <- k + 1
  }

  grid <- paste(layout, collapse = "\n")
  grid <- paste("<div>\n", grid, "\n</div>", collapse = "\n")

  rstudioapi::insertText(grid)
}
