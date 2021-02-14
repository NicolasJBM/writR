#' @name latex_fit_stargazer
#' @title Fit Table Size
#' @author Nicolas Mangin
#' @description Create a Latex tabular environment for stargazer and insert it in an adjustable table
#' @param ...           Arguments passed to the stargazer function.
#' @param table_caption Character. Title of the table.
#' @param table_label   Character. Label of the table (for referencing in Latex).
#' @param env           Character. One of "table" or "sidewaystable".
#' @param adjwidth      Double. Percentage of the width of the text area.
#' @param adjheight     Double. Percentage of the height of the text area.
#' @param space_before  Character. Spacing before the table.
#' @param space_after   Character. Spacing after the table.
#' @return              A resized printable Latex table.
#' @importFrom stargazer stargazer
#' @importFrom utils capture.output
#' @export

latex_fit_stargazer <- function(...,
                                table_caption = "",
                                table_label = "",
                                env = "table",
                                adjwidth = 0.75,
                                adjheight = 0.75,
                                space_before = "15pt",
                                space_after = "15pt") {
  x <- capture.output(
    stargazer(...)
  )

  if (env == "sidewaystable") {
    width <- "\\textheight"
    height <- "\\textwidth"
  } else {
    height <- "\\textheight"
    width <- "\\textwidth"
  }

  writeLines(paste0(
    "\\vspace{",
    space_before,
    "} \n\\begin{",
    env,
    "}[ht] \n\\begin{center} \n",

    "\\begin{adjustbox}{width=",
    adjwidth,
    width,
    ",totalheight=",
    adjheight,
    height,
    "} \n",

    "\\begin{threeparttable} \n",
    "\\caption{",
    table_caption,
    "} \n",
    "\\label{tab:",
    table_label,
    "}"
  ))

  cat(x, sep = "\n")

  writeLines(paste0(
    "\n\\end{threeparttable} \n",
    "\\end{adjustbox} \n\\end{center} \n",
    "\\end{",
    env,
    "} \n\\vspace{",
    space_after,
    "}"
  ))
}
