#' Insert tables or figures in panels in a Latex environments
#' @param panels      Character vector. Output of the capture.output function applied to latex tables.
#' @param subcaptions character. Titles of the subtables.
#' @param tab_width   Character. Width of the minipage if length == 1, width of each subtable otheriwse.
#' @param end_line    Numeric. Vector of same length as tab_width; 1 if the subtable should end the line, 0 otherwise.
#' @param maincaption Character. Title of the group of figures.
#' @param label       Character. Label of the group of figures.
#' @param orientation Character. "portrait" of "landscape".
#' @param font_size   Character. Latex specification of the font size (e.g. "normalsize", "small", "tiny").
#' @return            Character. A set of objects organized as panels.
#' @importFrom dplyr %>%
#' @importFrom dplyr mutate
#' @importFrom dplyr select
#' @importFrom tibble tibble
#' @importFrom purrr map
#' @export


latex_panels <- function(panels,
                         subcaptions,
                         tab_width = c(1),
                         end_line = c(F),
                         maincaption,
                         label,
                         orientation = "portrait",
                         font_size = "normalsize"){
  
  stopifnot(
    length(panels) == length(subcaptions),
    length(tab_width) == length(end_line),
    orientation %in% c("portrait", "landscape")
  )
  
  # Bind variables
  before <- NULL
  object <- NULL
  after <- NULL
  width <- NULL
  
  if (orientation == "landscape"){
    first <- "\\begin{landscape} \n"
    last <- "\n\\end{landscape}"
  } else {
    first <- ""
    last <- ""
  }
  
  if (length(tab_width) == 1){
    
    core <- tibble::tibble(
      object = panels,
      subcaptions = subcaptions
    )
    
    begbox <- paste0("\\begin{table}[!hbt] \n\\centering \n\\begin{minipage}{", tab_width, "\\textwidth} \n\\caption{", maincaption,"} \n\\label{", label,"}")
    
    endbox <- paste0("\\end{minipage} \n
                     \\end{table}")
    
    core <- core %>%
      dplyr::mutate(
        before = paste0("\\begin{subtable}[t]{\\linewidth} \n\\caption{", subcaptions,"} \n\\", font_size),
        after = "\\end{subtable} \n\\vspace{15pt}\n"
      ) %>%
      mutate(object = purrr::pmap(list(before, object, after), c)) %>%
      dplyr::select(object)
    
  } else {
    
    core <- tibble::tibble(
      object = panels,
      width = tab_width,
      subcaptions = subcaptions,
      end_line = end_line
    ) %>%
      dplyr::mutate(
        end_line = dplyr::case_when(end_line == 1 ~ " \n\n\\vspace{15pt}\n\n", TRUE ~ " \n\\hspace{\\fill}")
      )
    
    begbox <- paste0("\\begin{table}[!hbt] \n\\caption{", maincaption,"} \n\\label{", label,"}")
    
    endbox <- paste0("\\end{table}")
    
    core <- core %>%
      dplyr::mutate(
        before = paste0("\\begin{subtable}[t]{", width, "\\linewidth} \n\\caption{", subcaptions,"} \n\\raggedright \n\\", font_size),
        after = paste0("\\end{subtable}", end_line)
      ) %>%
      dplyr::mutate(object = purrr::pmap(list(before, object, after), c)) %>%
      dplyr::select(object)
  }
  
  start <- paste0(first,begbox)
  end <- paste0(endbox, last)
  
  output <- unlist(list(
    start,
    core,
    end
  ))
  
  writeLines(output)
}