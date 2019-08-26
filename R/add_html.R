#' Gadget to insert various objects in an html document.
#' @return A citation.
#' @importFrom miniUI miniPage
#' @importFrom miniUI gadgetTitleBar
#' @importFrom miniUI miniTabstripPanel
#' @importFrom miniUI miniTabPanel
#' @importFrom miniUI miniContentPanel
#' @importFrom shiny fillCol
#' @importFrom shiny fillRow
#' @importFrom shiny icon
#' @importFrom shiny fileInput
#' @importFrom shiny textInput
#' @importFrom shiny dateInput
#' @importFrom shiny numericInput
#' @importFrom shiny textAreaInput
#' @importFrom shiny selectInput
#' @importFrom shiny checkboxInput
#' @importFrom shiny downloadButton
#' @importFrom shiny downloadHandler
#' @importFrom shiny stopApp
#' @importFrom shiny runGadget
#' @importFrom shiny conditionalPanel
#' @importFrom shiny tags
#' @importFrom shiny dataTableOutput
#' @importFrom shiny htmlOutput
#' @importFrom shiny uiOutput
#' @importFrom shiny plotOutput
#' @importFrom shiny textOutput
#' @importFrom shiny actionButton
#' @importFrom shiny renderDataTable
#' @importFrom shiny renderUI
#' @importFrom shiny renderPlot
#' @importFrom shiny renderText
#' @importFrom shiny reactive
#' @importFrom shiny reactiveValues
#' @importFrom shiny observe
#' @importFrom shiny observeEvent
#' @importFrom shiny withProgress
#' @importFrom shiny incProgress
#' @importFrom shiny h3
#' @importFrom shiny isolate
#' @importFrom shiny reactiveValuesToList
#' @importFrom shiny tableOutput
#' @importFrom shiny renderTable
#' @importFrom shiny HTML
#' @importFrom shiny validate
#' @importFrom shiny need
#' @importFrom shiny fluidRow
#' @importFrom shiny column
#' @importFrom shiny showModal
#' @importFrom shiny modalDialog
#' @importFrom shiny eventReactive
#' @importFrom shiny dialogViewer
#' @importFrom shinythemes shinytheme
#' @importFrom colourpicker colourInput
#' @importFrom tibble column_to_rownames
#' @importFrom tibble rownames_to_column
#' @importFrom tibble tibble
#' @importFrom tibble as_tibble
#' @importFrom dplyr select
#' @importFrom dplyr filter
#' @importFrom dplyr group_by
#' @importFrom dplyr summarize_all
#' @importFrom dplyr mutate
#' @importFrom dplyr %>%
#' @importFrom dplyr case_when
#' @importFrom dplyr arrange
#' @importFrom dplyr bind_rows
#' @importFrom dplyr everything
#' @importFrom tibble tibble
#' @importFrom stringr str_extract
#' @importFrom stringr str_extract_all
#' @importFrom stringr str_remove_all
#' @importFrom stringr str_split
#' @importFrom stringr str_detect
#' @importFrom stringr str_replace 
#' @importFrom rhandsontable renderRHandsontable
#' @importFrom rhandsontable rHandsontableOutput
#' @importFrom rhandsontable hot_to_r
#' @importFrom rhandsontable rhandsontable
#' @importFrom rhandsontable hot_cols
#' @importFrom stats na.omit
#' @importFrom utils read.csv
#' @importFrom RefManageR ReadBib
#' @export


add_html <- function() {
  
  options(shiny.maxRequestSize=500*1024^2)
  
  ui <- miniPage(
    theme = shinytheme("spacelab"),
    
    gadgetTitleBar("Insert html"),
    miniTabstripPanel(
      
      # Panel where the author selects references in the filtered list
      miniTabPanel(
        "Search",
        icon = icon("search"),
        miniContentPanel(
          selectInput("typeobj", label = "Select the type of object to insert",
                      choices = c("slide","meme","layout","iframe","box","fragment","link","note"),
                      selected = "slide"),
          tags$hr(),
          uiOutput("specifications"),
          tags$hr(),
          actionButton("insert", "Insert")
        )
      )
    )
  )
  
  
  
  server <- function(input, output, session) {
    
    
    output$specifications <- renderUI({
      
      if (input$typeobj == "slide"){
        ui <- list(
          textInput("title", "Title of the slide"),
          colourInput("colorback", "Color of the background", value = "#FFFFFF"),
          selectInput("transback", "Background transition",
                      choices = c("fade", "slide", "convex", "concave","zoom","cube","none"),
                      selected = "slide"),
          selectInput("transdata", "Data transition",
                      choices = c("fade", "slide", "convex", "concave","zoom"),
                      selected = "fade"),
          textInput('image', "Relative path to the image", value = ""),
          textInput("video", "relative path to the video", value = "")
        )
      } else if (input$typeobj == "meme"){
        ui <- list(
          textInput("imgmeme", "Address of the image", value = "img/img.png"),
          textInput("uppermeme", "Top sentence", value = "sentence at the top"),
          textInput("lowermeme", "Bottom sentence", value = "sentence at the bottom"),
          numericInput("sizememe", "Size of the text", value = 3),
          numericInput("contourmeme", "Size of the contour", value = 0.3),
          colourInput("colmeme", "Color of the text", value = "#FFFFFF"),
          colourInput("bkgmeme", "Color of the text", value = "#000")
        )
      } else if (input$typeobj == "layout"){
        ui <- list(
          numericInput("rows", "Number of rows", value = 2),
          numericInput("columns", "Number of columns", value = 2)
        )
      } else if (input$typeobj == "iframe"){
        ui <- list(
          textInput("linkiframe", "Address of the web page")
        )
      } else if (input$typeobj == "box"){
        ui <- list(
          textInput("text1", "Text in the box", value = ""),
          textInput("width", "Width of the box", value = "20%"),
          textInput("top", "Width of the box", value = "0px"),
          textInput("left", "Width of the box", value = "0px"),
          colourInput("background", "Color of the background", value = "#000",allowTransparent = TRUE),
          colourInput("color", "Color of the characters", value = "#FFFFFF"),
          numericInput("size", "Font size", value = 14),
          selectInput("align", "Text alignment", choices = c("left","center","right","justify"), selected = "center")
        )
      } else if (input$typeobj == "fragment"){
        ui <- list(
          selectInput("animations", "Animations of the fragment",
                      choices = c("fade-out","semi-fade-out", "reduce-size"),
                      selected = c("semi-fade-out","reduce-size"),
                      multiple = TRUE)
        )
      } else if (input$typeobj == "link"){
        ui <- list(
          ui <- list(
            textInput("link", "Address of the web page"),
            textInput("label", "Text of the hyperlink")
          )
        )
      } else {
        ui <- list(
          textInput("note", "Content of the notes")
        )
      }
      ui
    })
    
    
    observeEvent(input$insert, {
      
      if (input$typeobj == "slide"){
        
        html_slide(title = input$title,
                   colorback = input$colorback,
                   transback = input$transback,
                   transdata = input$transdata,
                   image = input$image,
                   video = input$video)
        
      } else if (input$typeobj == "meme"){
        
        c(
          "```{r echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE, background='black', fig.width=8, fig.height=6}\n\n",
          "meme::meme(\n",
          paste0("  img = '", input$imgmeme,"',\n"),
          paste0("  upper = '", input$uppermeme,"',\n"),
          paste0("  lower = '", input$lowermeme,"',\n"),
          paste0("  size = ", input$sizememe,",\n"),
          paste0("  r = ", input$contourmeme,",\n"),
          paste0("  color = '", input$colmeme,"',\n"),
          paste0("  bgcolor = '", input$bkgmeme,"'\n)\n\n"),
          "```"
        ) %>%
          paste(collapse = "") %>%
          rstudioapi::insertText()
        
      } else if (input$typeobj == "layout"){
        
        nrow <- input$rows
        ncol <- input$columns
        layout <- list()
        for (i in 1:nrow){
          layout[[i]] <- rep(1, ncol)
        }
        html_layout(layout)
        
      } else if (input$typeobj == "iframe"){
        
        rstudioapi::insertText(paste0('<iframe src="', input$linkiframe,'" width="100%" height="900px"></iframe>'))
        
      } else if (input$typeobj == "box"){
        
        html_box(
          text = input$text1,
          width = input$width,
          top = input$top,
          left = input$left,
          background = input$background,
          color = input$color,
          size = input$size,
          align = input$align
        )
        
      } else if (input$typeobj == "fragment"){
        
        animations <- input$animations %>%
          stringr::str_replace_all("fade-out","fo") %>%
          stringr::str_replace_all("semi-fade-out","sfo") %>%
          stringr::str_replace_all("reduce-size","rs")
        rstudioapi::insertText(paste0('<p class = "fragment ', paste(animations, collapse = " "),'" >   </p>'))
        
      } else if (input$typeobj == "link"){
        
        rstudioapi::insertText(paste0('<a href="',input$link,'">', input$label,'</a>'))
        
      } else {
        
        rstudioapi::insertText(paste0('<aside class = "notes">\n\n', input$note,'\n\n</aside>'))
        
      }
    })
    
    observeEvent(input$done, {
      stopApp()
    })
  }
  runGadget(ui, server, viewer = paneViewer(minHeight = "maximize"))
}
