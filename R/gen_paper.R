#' @name gen_paper
#' @title Format submissions
#' @author Nicolas Mangin
#' @description Format submissions to various academic conferences and journals.
#' @inheritParams rmarkdown::pdf_document
#' @param engine character. Latex engine to be used to produce the pdf.
#' @param journal character. Journal to which the paper is submitted. See details for the available options.
#' @details The templates currently available are:
#' \itemize{
#'    \item aos for Accounting, Organizations, and Society
#'    \item bria for Behavioral Research in Accounting
#'    \item car for Contemporary Accounting Research
#'    \item cpa for Critical Perspectives on Accounting
#'    \item ear for European Accounting Review
#'    \item jae for Journal of Accounting and Economics
#'    \item jar for Journal of Accounting Research
#'    \item jaaf for Journal of Accounting, Auditing, and Finance
#'    \item jmar for Journal of Management Accounting Research
#'    \item mar for Management Accounting Research
#'    \item tar for The Accounting Review
#'    \item jfe for Journal of Financial Economics
#'    \item amj for Academy of Management Journal
#'    \item amr for Academy of Management Review
#'    \item jibs for Journal of International Business Studies
#'    \item jom for Journal of Management
#'    \item mnsc for Management Science
#'    \item orsc for Organization Science
#'    \item smj for Strategic Management Journal
#'    \item orm for Organizational Research Methods
#'    \item jap for Journal of Applied Psychology
#' }
#' @return R Markdown output format to pass to
#'   \code{\link[rmarkdown:render]{render}}
#' @import rstudioapi
#' @import rmarkdown
#' @import rticles
#' @importFrom dplyr filter
#' @importFrom dplyr select
#' @export

gen_paper <- function(...,
                      engine = "xelatex",
                      journal = "aom",
                      keep_tex = TRUE,
                      citation_package = "natbib",
                      md_extensions = c("-autolink_bare_uris")) {


  # Select the appropriate template for the journal specified and find its path.
  template <- writR::dat_journals[writR::dat_journals$acronym == journal, ]

  template_tex <- system.file(
    "rmarkdown",
    "tex",
    file.path(template),
    package = "writR"
  )

  # Retrieve the template and produce the document.
  inherit_pdf_document <- function(...) {
    fmt <- rmarkdown::pdf_document(...)
    fmt$inherits <- "pdf_document"
    fmt
  }

  inherit_pdf_document(...,
    template = template_tex,
    keep_tex = keep_tex,
    citation_package = citation_package,
    md_extensions = md_extensions,
    latex_engine = engine
  )
}
