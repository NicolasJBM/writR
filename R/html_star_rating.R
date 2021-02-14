#' @name html_star_rating
#' @title Insert a Star Rating
#' @author Nicolas Mangin
#' @description Function inserting a star rating in an HTML document. This requires the css and the starrating.js to be specified separately in the header and footer of the HTML document.
#' @param source Character. Name of the book.
#' @param page   Integer. Number of the section in the book.
#' @return HTML code to collect star ratings in online textbooks.
#' @export


html_star_rating <- function(source = "", page = 1) {
  writeLines(c(
    "<section id='rating' class='rating-widget'>",
    "  <p class='text-center'>Please indicate how clear and understandable this page was for you:</p>",
    paste0(
      "  <div id='",
      source, "_", page,
      "' class='rating-stars text-center'><ul id='stars'><li class='star' title='Poor' data-value='1'><i class='fa fa-star fa-fw'></i></li><li class='star' title='Fair' data-value='2'><i class='fa fa-star fa-fw'></i></li><li class='star' title='Good' data-value='3'><i class='fa fa-star fa-fw'></i></li><li class='star' title='Excellent' data-value='4'><i class='fa fa-star fa-fw'></i></li><li class='star' title='WOW!!!' data-value='5'><i class='fa fa-star fa-fw'></i></li></ul></div>"
    ),
    "  <div class='success-box'><div class='clearfix'></div><div class='text-message'></div><div class='clearfix'></div></div>",
    "</section>"
  ))
}
