#' Gadget to insert various charts in documents.
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
#' @importFrom shiny paneViewer
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
#' @importFrom tidyr unite
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
#' @importFrom plotly renderPlotly
#' @importFrom plotly plotlyOutput
#' @importFrom collapsibleTree renderCollapsibleTree
#' @importFrom collapsibleTree collapsibleTreeOutput
#' @importFrom DiagrammeR renderDiagrammeR
#' @importFrom DiagrammeR DiagrammeROutput
#' @importFrom DiagrammeR render_graph
#' @importFrom DiagrammeR renderGrViz
#' @importFrom DiagrammeR grVizOutput
#' @importFrom ndtv ndtvAnimationWidgetOutput
#' @importFrom ndtv renderNdtvAnimationWidget
#' @export


add_chart <- function() {
  
  
  ui <- miniPage(
    theme = shinytheme("spacelab"),
    
    gadgetTitleBar("Insert chart"),
    miniTabstripPanel(
      miniTabPanel(
        "Search",
        icon = icon("search"),
        miniContentPanel(
          fluidRow(
            column(4, uiOutput("information")),
            column(4, uiOutput("object")),
            column(4, uiOutput("type"))
          ),
          fluidRow(
            column(4, uiOutput("format")),
            column(4, uiOutput("chartid")),
            column(4, checkboxInput("inchunk", "In chunk", value = TRUE))
          ),
          tags$hr(),
          uiOutput("display"),
          tags$hr(),
          fluidRow(
            column(3, actionButton("insert", "Insert")),
            column(9, textOutput("use"))
          ) 
        )
      )
    )
  )
  
  
  server <- function(input, output, session) {
    
    # Bind variables
    idchart <- NULL
    information <- NULL
    object <- NULL
    type <- NULL
    
    
    ##############################################################################
    ####                          FILTERS                                     ####
    ##############################################################################
    
    ##########################
    output$information <- renderUI({
      choices <- c("", sort(unique(dplyr::filter(writer::charts, !is.na(render))$information)))
      selectInput("slctinfo", "Information:", choices = choices, selected = "")
    })
    
    afterinfo <- reactive({
      if (is.null(input$slctinfo)){
        dplyr::filter(writer::charts, !is.na(render))
      } else if (input$slctinfo == ""){
        dplyr::filter(writer::charts, !is.na(render))
      } else {
        dplyr::filter(writer::charts, !is.na(render)) %>%
          dplyr::filter(information == input$slctinfo)
      }
    })
    
    ##########################
    output$object <- renderUI({
      choices <- c("", sort(unique(afterinfo()$object)))
      selectInput("slctobj", "Object:", choices = choices, selected = "")
    })
    
    afterobject <- reactive({
      if (is.null(input$slctobj)){
        afterinfo() 
      } else if (input$slctobj == ""){
        afterinfo()
      } else {
        afterinfo() %>%
          dplyr::filter(object == input$slctobj)
      }
    })
    
    ##########################
    output$type <- renderUI({
      choices <- sort(na.omit(unique(c("", aftertype()$type))))
      selectInput("slcttype", "Type:", choices = choices, selected = "")
    })
    
    aftertype <- reactive({
      if (is.null(input$slcttype)){
        afterobject()
      } else if (input$slcttype == ""){
        afterobject()
      } else {
        afterobject() %>%
          dplyr::filter(type %in% c("any", input$slcttype))
      }
    })
    
    ##########################
    output$format <- renderUI({
      choices <- sort(na.omit(unique(c("", aftertype()$format))))
      selectInput("slctfmt", "Format:", choices = choices, selected = "")
    })
    
    afterformat <- reactive({
      if (is.null(input$slctfmt)){
        aftertype()
      } else if (input$slctfmt == ""){
        aftertype()
      } else {
        aftertype() %>%
          dplyr::filter(format %in% c("any", input$slctfmt))
      }
    })
    
    ##########################
    output$chartid <- renderUI({
      choices <- sort(na.omit(unique(c(0, afterformat()$idchart))))
      selectInput("slctid", "Chart:", choices = choices, selected = "")
    })
    
    afterid <- reactive({
      if (is.null(input$slctid)){
        afterformat()
      } else if (input$slctid == 0){
        afterformat()
      } else {
        afterformat() %>%
          dplyr::filter(idchart == input$slctid)
      }
    })
    
    
    
    ##############################################################################
    ####                          DISPLAY                                     ####
    ##############################################################################
    
    rendering <- reactive({
      if (!is.null(afterid())){
        if (nrow(afterid()) == 1){
          afterid()$render[[1]]
        } else "text"
      } else "text"
    })
    
    ##########################
    output$plot <- renderPlot({
      validate(need(rendering() == "plot", message=FALSE))
      source(paste0(find.package("writer"), "/charts/",afterid()$idchart[[1]],".R"))
      if (exists("chart")) get("chart")
    })
    
    output$plotly <- renderPlotly({
      validate(need(rendering() == "plotly", message=FALSE))
      source(paste0(find.package("writer"), "/charts/",afterid()$idchart[[1]],".R"))
      if (exists("chart")) get("chart")
    })
    
    output$collapsibleTree <- renderCollapsibleTree({
      validate(need(rendering() == "collapsibleTree", message=FALSE))
      source(paste0(find.package("writer"), "/charts/",afterid()$idchart[[1]],".R"))
      if (exists("chart")) get("chart")
    })
    
    
    output$grviz <- renderGrViz({
      validate(need(rendering() == "grviz", message=FALSE))
      source(paste0(find.package("writer"), "/charts/",afterid()$idchart[[1]],".R"))
      if (exists("chart")) get("chart")
    })
    
    output$ndtv <- ndtv::renderNdtvAnimationWidget({
      validate(need(rendering() == "ndtv", message=FALSE))
      source(paste0(find.package("writer"), "/charts/",afterid()$idchart[[1]],".R"))
      if (exists("chart")) get("chart")
    })
    
    output$text <- renderText({
      "Please select one type of chart to display."
    })
    
    
    ##########################
    output$display <- renderUI({
      if (rendering() == "plot"){
        plotOutput("plot", height = "400px")
      } else if (rendering() == "plotly"){
        plotlyOutput("plotly", height = "400px")
      } else if (rendering() == "collapsibleTree"){
        collapsibleTreeOutput("collapsibleTree", height = "400px")
      } else if (rendering() == "grviz"){
        grVizOutput("grviz", width = "100%", height = "400px")
      } else if (rendering() == "ndtv"){
        ndtv::ndtvAnimationWidgetOutput("ndtv", width = "100%", height = "400px")
      } else textOutput("text")
    })
    
    
    output$use <- renderText({
      if (!is.null(afterid())){
        if (nrow(afterid()) == 1){
          afterid()$use
        } else ""
      } else ""
    })
    
    ##############################################################################
    ####                           INSERT                                     ####
    ##############################################################################
    
    observeEvent(input$insert, {
      
      if (rendering() == "text"){
        code <- "\n\n\n"
      } else {
        code <- paste(c(readLines(paste0(find.package("writer"), "/charts/",afterid()$idchart[[1]],".R")), "\nchart"), "\n")
      }
      
      if (input$inchunk){
        
        if (afterid()$type[[1]] == "static" & afterid()$format[[1]] == "latex"){
          code <- c(
            "```{r idchunk, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}\n",
            code,
            "\n```"
          )
        } else if (afterid()$type[[1]] == "static" & afterid()$format[[1]] == "html"){
          code <- c(
            "```{r idchunk, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}\n",
            code,
            "\n```"
          )
        } else if (afterid()$type[[1]] == "interactive" & afterid()$format[[1]] == "html"){
          code <- c(
            "```{r idchunk, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}\n",
            code,
            "\n```"
          )
        } else if (afterid()$type[[1]] == "dynamic" & afterid()$format[[1]] == "html"){
          code <- c(
            "```{r idchunk, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}\n",
            code,
            "\n```"
          )
        } else {
          code <- c(
            "```{r idchunk, echo=FALSE, message=FALSE, warning=FALSE, paged.print=FALSE}\n",
            code,
            "\n```"
          )
        }
      }
      
      code %>%
        paste0(collapse = "") %>%
        rstudioapi::insertText()
    })
    
    observeEvent(input$done, {
      stopApp()
    })
  }
  runGadget(ui, server, viewer = paneViewer(minHeight = "maximize"))
}
