#' @name latex_long_stargazer
#' @title Make Table Long
#' @author Nicolas Mangin
#' @description Create a longtable stargazer output
#' @param ...             Arguments passed to the stargazer function.
#' @param table_caption   Character. Title of the table.
#' @param table_label     Character. Label of the table (for referencing in Latex).
#' @param threeparttable  Logical. Align captions with table.
#' @param landscape       Logical. Put on a landscape page.
#' @param font_size       Integer. Size of the font inside the table.
#' @return                A printable Latex long table
#' @importFrom stargazer stargazer
#' @importFrom utils capture.output
#' @export

latex_long_stargazer <- function(...,
                                 table_caption,
                                 table_label,
                                 threeparttable = TRUE,
                                 landscape = FALSE,
                                 font_size = "small") {

  # Capturing stargazer to hack it
  x <- capture.output(
    stargazer(...)
  )

  # Changing tabulare environment for longtable
  x <- gsub("tabular", "longtable", x)

  x <- c(
    x[1:which(x == "\\hline \\\\[-1.8ex] ")[1] - 1],
    "\\endhead",
    x[which(x == "\\hline \\\\[-1.8ex] ")[1]:length(x)]
  )

  x <- c(
    paste0("\\", font_size),
    x[1:2],
    paste0("\\caption{", table_caption, "}"),
    paste0("\\label{", table_label, "}"),
    x[3:length(x)]
  )

  if (threeparttable) {
    x <- c("\\begin{ThreePartTable}", x, "\\end{ThreePartTable}")
  }

  if (landscape) {
    x <- c("\\begin{landscape}", x, "\\end{landscape}")
  }

  cat(x, sep = "\n")
}
