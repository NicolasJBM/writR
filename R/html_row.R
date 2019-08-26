#' Gather html cells on a row (is ment to be used through html_layout)
#' @param cells character vector. Vector of outputs of the function html_cell
#' @return Character. html row composed of several cells.
#' @seealso html_layout
#' @export

html_row <- function(cells){
  row <- paste0(cells, collapse = "\n")
  row <- paste0(
    '<div style="display: flex;">\n',
    row,
    '\n</div>'
  )
  return(row)
}