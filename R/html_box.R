#' @name html_box
#' @title Place a Box on a Slide
#' @author Nicolas Mangin
#' @description Create a HTML box which can be placed anywhere on the page or slide, just by specifying the distance from the top left.
#' @param text Character. Content of the box.
#' @param width Character. Width of the box.
#' @param top Character. Distance from the top.
#' @param left Character. Distance from the right.
#' @param background Character. Background color.
#' @param color Character. Font color.
#' @param size Integer. Font size.
#' @param align Character. Text alignment
#' @return Character. Formatted and placed box.
#' @importFrom grDevices rgb
#' @export

html_box <- function(text = "",
                     width = "20%",
                     top = "0px",
                     left = "0px",
                     background = rgb(0, 0, 0, 0.75),
                     color = "#FFF",
                     size = 14,
                     align = "justify") {
  rstudioapi::insertText(paste0(
    '<div style="position: absolute; width: ', width,
    "; top: ",
    top,
    "; left: ",
    left,
    "; box-shadow: 0 1px 4px rgba(0,0,0,0.5), 0 5px 25px rgba(0,0,0,0.2); background-color: ",
    background,
    "; color: ",
    color,
    "; padding: 20px; font-size: ",
    size,
    "; text-align: ",
    align,
    ';"><p>\n',
    text,
    "\n\n</p> </div>"
  ))
}
