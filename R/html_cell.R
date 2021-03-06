#' @name html_cell
#' @title Create a cell for a row in a layout
#' @author Nicolas Mangin
#' @description Write HTML code for a cell; it is meant to be used through html_layout.
#' @param width     Integer. relative width of the cell.
#' @param cell_name Integer. ID of the cell to identify it.
#' @return Character. HTML code for a cell.
#' @seealso html_layout
#' @export


html_cell <- function(width = 1, cell_name) {
  cell <- paste0(
    '<div style="flex: ', width,
    '; margin:1%; padding: 1%; background-color: white; border: solid 1px white; color:black; text-align: left; vertical-align: top;">\n\nCell ',
    cell_name,
    "\n\n</div>"
  )
  return(cell)
}
