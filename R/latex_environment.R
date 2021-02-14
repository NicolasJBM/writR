#' @name latex_env
#' @title Insert Latex Environments
#' @author Nicolas Mangin
#' @description Insert text like a question, proposition, hypothesis or quote in Latex environments
#' @param text          character vector. Text to be inserted. If more than one entry are prvided, create a sub-environment.
#' @param env           Character. Latex environment in which the text has to be inserted.
#' @param space_before  Character. Spacing before the group.
#' @param space_between Character. Spacing between entries.
#' @param space_after   Character. Spacing after the group.
#' @param author        Character. Person quoted.
#' @param bibkey        Logical. Whether the author is formatted as a reference.
#' @param pages         Character. Pages of the quote.
#' @return              Character. Formatted Latex environment.
#' @importFrom dplyr mutate
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @export

latex_env <- function(text = "",
                      env = "hypothesis",
                      space_before = "15pt",
                      space_between = "5pt",
                      space_after = "15pt",
                      author = "",
                      bibkey = FALSE,
                      pages = "") {
  createnv <- function(text, env, space_before, space_after) {
    paste0(
      "\\vspace{", space_before, "} \n\\begin{", env, "} ",
      text,
      " \\end{", env, "} \n\\vspace{", space_after, "} \n"
    )
  }

  createquote <- function(text, env, space_before, space_after, author) {
    paste0(
      "\\vspace{", space_before, "} \n\\begin{quotep}[30pt]{", author, "} ",
      '"', text, '"',
      " \\end{quotep} \n\\vspace{", space_after, "} \n"
    )
  }

  if (length(text) == 1) {
    if (env == "quote") {
      if (bibkey & pages == "") {
        author <- paste0("\\citet{", author, "}")
      }
      if (bibkey & pages != "") {
        author <- paste0("\\citet[", pages, "]{", author, "}")
      }
      y <- createquote(
        text,
        env = env,
        space_before = space_before,
        space_after = space_after,
        author = author
      )
    } else {
      y <- createnv(
        text,
        env = env,
        space_before = space_before,
        space_after = space_after
      )
    }
  } else {
    subgroup <- paste0("gp", env, collapse = "")

    y <- tibble::tibble(text = text) %>%
      dplyr::mutate(latex = purrr::map(
        text,
        createnv,
        env = env,
        space_before = "0pt",
        space_after = space_between
      ))

    y <- paste0(
      c(
        "\\vspace{", space_before, "} \n\\begin{", subgroup, "} \n",
        y$latex,
        "\\end{", subgroup, "} \n\\vspace{", space_after, "}"
      ),
      collapse = ""
    )
  }

  writeLines(y)
}
