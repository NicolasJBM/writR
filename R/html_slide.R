#' Insert a reveal js slide
#' @param title Character. Title of the slide.
#' @param colorback Character. Background color.
#' @param transback Character. Type of transition for the background: fade, slide, convex, concave, or zoom.
#' @param transdata Character. Type of transition for data: fade, slide, convex, concave, or zoom.
#' @param image Character. Address of the background image.
#' @param video Character. Address of the background video.
#' @return Character. Reveal js slide formatted for rmarkdown.
#' @export


html_slide <- function(title = "",
                       colorback = "#FFFFFF",
                       transback = "slide",
                       transdata = "fade",
                       image = "",
                       video = "") {
  properties <- dplyr::case_when(
    image != "" ~ paste0(
      ' {data-transition="', transdata,
      '" data-background-transition="', transback,
      '" data-background-color="', colorback,
      '" data-background="',
      image, '"}'
    ),
    video != "" ~ paste0(
      ' {data-transition="', transdata,
      '" data-background-transition="', transback,
      '" data-background-color="', colorback,
      '" data-background-video="', video,
      '"}'
    ),
    TRUE ~ paste0(
      ' {data-transition="', transdata,
      '" data-background-transition="', transback,
      '" data-background-color="', colorback,
      '"}'
    )
  )
  rstudioapi::insertText(paste0("## ", title, properties))
}
