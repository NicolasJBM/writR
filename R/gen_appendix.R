#' Format appendices.
#'
#' Actually make sure no special format is applied to the appendices so that the main template applies.
#'
#' @inheritParams rmarkdown::pdf_document
#' @return R Markdown output format to pass to
#'   \code{\link[rmarkdown:render]{render}}
#' @import rstudioapi
#' @import rmarkdown
#' 
#' @export

gen_appendix <- function(...,
                        keep_tex = TRUE,
                        md_extensions = c("-autolink_bare_uris")) {
  
  template <- system.file("rmarkdown", "tex", file.path("appendix.tex"), package = "writR")
  
  # Retrieve the template and produce the document.
  inherit_pdf_document <- function(...) {
    fmt <- rmarkdown::pdf_document(...)
    fmt$inherits <- "pdf_document"
    fmt
  }
  
  inherit_pdf_document(...,
                       template = template,
                       keep_tex = keep_tex,
                       md_extensions = md_extensions,
                       latex_engine = "xelatex")
  
}