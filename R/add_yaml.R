#' Gadget to insert the yaml of a research paper.
#' @return A citation.
#' @importFrom miniUI miniPage
#' @importFrom miniUI gadgetTitleBar
#' @importFrom miniUI miniTabstripPanel
#' @importFrom miniUI miniTabPanel
#' @importFrom miniUI miniContentPanel
#' @importFrom shiny icon
#' @importFrom shiny fileInput
#' @importFrom shiny textInput
#' @importFrom shiny textAreaInput
#' @importFrom shiny selectInput
#' @importFrom shiny checkboxInput
#' @importFrom shiny downloadButton
#' @importFrom shiny downloadHandler
#' @importFrom shiny stopApp
#' @importFrom shiny runGadget
#' @importFrom shiny conditionalPanel
#' @importFrom shiny tags
#' @importFrom DT DTOutput
#' @importFrom shiny htmlOutput
#' @importFrom shiny uiOutput
#' @importFrom shiny plotOutput
#' @importFrom shiny textOutput
#' @importFrom shiny actionButton
#' @importFrom DT renderDT
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
#' @importFrom tokenizers count_words
#' @export


add_yaml <- function() {
  
  
  ui <- miniPage(
    theme = shinytheme("spacelab"),
    
    gadgetTitleBar("Insert yaml"),
    miniTabstripPanel(
      
      miniTabPanel(
        "Paper",
        icon = icon("copy"),
        miniContentPanel(
          uiOutput("paperinfo")
        )
      ),
      
      miniTabPanel(
        "Authors",
        icon = icon("user-friends"),
        miniContentPanel(
          uiOutput("addauthor"),
          tags$hr(),
          uiOutput("orgauthor"),
          tags$hr(),
          DT::DTOutput("displayauthor")
        )
      ),
      
      miniTabPanel(
        "Codes",
        icon = icon("filter"),
        miniContentPanel(
          fluidRow(
            column(4, uiOutput("jelfiltl1")),
            column(4, uiOutput("jelfiltl2")),
            column(4, uiOutput("jelfiltl3"))
          ),
          DT::DTOutput("preslctjel"),
          tags$hr(),
          fluidRow(
            column(9, uiOutput("slctjelcodes")),
            column(3, actionButton("addjel", "Update",
                                   style="margin-top: 25px; color: #fff; background-color: #337ab7; border-color: #2e6da4")
                   )
          ),
          DT::DTOutput("displayjelselection")
        )
      ),
      
      miniTabPanel(
        "Format",
        icon = icon("align-justify"),
        miniContentPanel(
          uiOutput('slctjnl'),
          tags$hr(),
          uiOutput("specifications")
        )
      )
    )
  )
  
  
  
  server <- function(input, output, session) {
    
    # bind variables
    code <- NULL
    name <- NULL
    level1 <- NULL
    level2 <- NULL
    level3 <- NULL
    template <- NULL
    field <- NULL
    data <- NULL
    acronym <- NULL
    affiliation <- NULL
    address <- NULL
    
    
    if (file.exists("data/yamlinfo.RData")){
      load("data/yamlinfo.RData")
    } else {
      yamlinfo <- list(
        title = "",
        abstract = "",
        keywords = "",
        data = "",
        disclaimer = "",
        acknowledgements = "",
        authors = tibble(name = as.character(NA), email = as.character(NA),
                         affiliation = as.character(NA), address = as.character(NA),
                         rank = as.integer(NA), corresponding = as.logical(NA)),
        jel = c(""),
        journal = "",
        submissionid = "",
        firstpage = TRUE,
        doublespace = TRUE,
        raggedright = FALSE,
        tabfigtoend = TRUE,
        linenumbers = FALSE,
        layout = NA,
        date = "`r format(Sys.time(), '%B %d, %Y')`",
        bibliography = "ref.bib"
      )
    }
    
    
    values <- reactiveValues()
    values$authors <- yamlinfo$authors
    values$jelcodes <- writR::jelcodes
    values$slctcodes <- writR::jelcodes %>%
      dplyr::filter(code %in% yamlinfo$jel)
    values$journals <- writR::journals
    
    
    #######################################################
    # General informations about the paper
    
    output$cwabstr <- renderText({
      if (!is.null(input$abstract)) paste0(tokenizers::count_words(input$abstract), " words.") else ""
    })
    
    output$paperinfo <- renderUI({
      ui <- list(
        textAreaInput("title", "Title:", value = yamlinfo$title, height = "50px") %>%
          shiny::tagAppendAttributes(style = 'width: 100%;'),
        textAreaInput("abstract", "Abstract:", value = yamlinfo$abstract, height = "300px") %>%
          shiny::tagAppendAttributes(style = 'width: 100%;'),
        uiOutput("cwabstr"),
        tags$hr(),
        textAreaInput("keywords", "Keywords (separated with comas):", value = yamlinfo$keywords, height = "50px") %>%
          shiny::tagAppendAttributes(style = 'width: 100%;'),
        textAreaInput("data", "Data availability:", value = yamlinfo$data, height = "50px") %>%
          shiny::tagAppendAttributes(style = 'width: 100%;'),
        textAreaInput("disclaimer", "Disclaimer:", value = yamlinfo$disclaimer, height = "50px") %>%
          shiny::tagAppendAttributes(style = 'width: 100%;'),
        textAreaInput("acknowledgements", "Acknowledgements:", value = yamlinfo$acknowledgements, height = "75px") %>%
          shiny::tagAppendAttributes(style = 'width: 100%;')
      )
      ui
    })
    
    
    #######################################################
    # Authors
    output$addauthor <- renderUI({
      ui <- list(
        fluidRow(
          column(6, textInput("name","Name:", value = "")),
          column(6, textInput("email","email", value = ""))
        ),
        fluidRow(
          column(6, textInput("affiliation", "Affiliation:", value = "")),
          column(6, textInput("address", "Address:", value = ""))
        ),
        actionButton("addentry", "Add")
      )
    })
    
    observeEvent(input$addentry, {
      if (!is.null(input$name)){
        values$authors <- values$authors %>%
          na.omit() %>%
          bind_rows(tibble(
            name = input$name, email = input$email,
            affiliation = input$affiliation, address = input$address,
            rank = (nrow(na.omit(values$authors))+1), corresponding = FALSE
          ))
      }
    })
    
    output$orgauthor <- renderUI({
      ui <- list(
        fluidRow(
          column(6, selectInput("authorrank", "Authors in order of apparition:",
                                choices = values$authors$name, selected = values$authors$name, multiple = TRUE)),
          column(4, selectInput("corresponding", "Corresponding author:",
                                choices = values$authors$name, selected = values$authors$name[[1]], multiple = FALSE)),
          column(2, actionButton("reorder", "Apply"))
        )
      )
      ui
    })
    
    observeEvent(input$reorder, {
      if (!is.null(input$corresponding)){
        if (!is.null(input$authorrank)){
          values$authors <- values$authors %>%
            na.omit() %>%
            dplyr::mutate(corresponding = name == input$corresponding) %>%
            dplyr::filter(name %in% input$authorrank) %>%
            dplyr::mutate(rank = as.integer(factor(name, levels = input$authorrank))) %>%
            dplyr::arrange(rank)
        }
      }
    })
    
    output$displayauthor <- DT::renderDT({values$authors})
    
    
    #######################################################
    # JEL codes
    
    output$jelfiltl1 <- renderUI({
      choices <- c("",unique(values$jelcodes$level1))
      selectInput("jel1", "JEL - First level:", choices = choices, selected = "")
    })
    
    jelafter1 <- reactive({
      filter <- input$jel1
      if (is.null(filter)){
        values$jelcodes
      } else if (filter == "") {
        values$jelcodes
      } else {
        dplyr::filter(values$jelcodes, level1 == filter)
      }
    })
    
    output$jelfiltl2 <- renderUI({
      choices <- c("", unique(jelafter1()$level2))
      selectInput("jel2", "JEL - Second level:", choices = choices, selected = "")
    })
    
    jelafter2 <- reactive({
      filter <- input$jel2
      if (is.null(filter)){
        jelafter1()
      } else if (filter == "") {
        jelafter1()
      } else {
        dplyr::filter(jelafter1(), level2 == filter)
      }
    })
    
    output$jelfiltl3 <- renderUI({
      choices <- c("",unique(jelafter2()$level3))
      selectInput("jel3", "JEL - Third level:", choices = choices, selected = "")
    })
    
    jelafter3 <- reactive({
      filter <- input$jel3
      if (is.null(filter)){
        jelafter2()
      } else if (filter == "") {
        jelafter2()
      } else {
        dplyr::filter(jelafter2(), level3 == filter)
      }
    })
    
    output$preslctjel <- DT::renderDT({
      jelafter3()
    }, options = list(
      pageLength = 4
    ))
    
    output$slctjelcodes <- renderUI({
      choices <- values$jelcodes$code
      selected <- values$slctcodes$code
      selectInput("slctjel","Select the following JEL codes", choices = choices, selected = selected, multiple = TRUE)
    })
    
    observeEvent(input$addjel, {
      selection <- as.character(input$slctjel)
      
      values$slctcodes <- values$jelcodes %>%
        dplyr::filter(code %in% selection)
    })
    
    output$displayjelselection <- DT::renderDT({
      values$slctcodes
    }, options = list(
      pageLength = 4
    ))
    
    
    #######################################################
    # Formating
    
    output$slctjnl <- renderUI({
      
      jnl <- values$journals %>%
        dplyr::select(-template) %>%
        dplyr::group_by(field) %>%
        tidyr::nest() %>%
        dplyr::mutate(data = purrr::map(data, function(x){
          y <- x$acronym
          names(y) <- x$journal
          return(y)
        }))
      
      jnllist <- jnl$data
      names(jnllist) <- jnl$field
      
      ui <- fluidRow(
        column(6, selectInput("tgtjnl", label = "Select the journal",
                              choices = jnllist,
                              selected = jnllist[[1]],
                              width = "100%")),
        column(3, textInput("submissionid", "Submission ID (if any):", value = ""))
      )
    })
    
    
    output$specifications <- renderUI({
      
      if (!is.null(input$tgtjnl)) {
        if (input$tgtjnl != "") {
          
          tgtjnl <- values$journals %>%
            dplyr::filter(acronym == input$tgtjnl)
          
          if (tgtjnl$type[[1]] == "writR"){
            ui <- list(
              checkboxInput("firstpage", "Include the first page?", value = TRUE),
              checkboxInput("tabfigtoend", "Place tables and figures at the end?", value = TRUE),
              checkboxInput("doublespace", "Use double space?", value = TRUE),
              checkboxInput("linenumbers", "Add line numbers?", value = FALSE),
              checkboxInput("raggedright", "Ragged right? (by default the text is justified)", value = FALSE)
            )
          } else {
            
            if (tgtjnl$type[[1]] == "informs") {
              layout <- c("blindrev", "copyedit", "nonblindrev")
            } else {
              layout <- c("review","commented")
            }
            
            ui <- list(
              selectInput("layout", "Layout:", choices = layout, selected = layout[[1]])
            )
          }
          
          ui
        }
      }
      
    })
    
    observeEvent(input$done, {
      
      type <- values$journals %>%
        dplyr::filter(acronym == input$tgtjnl) %>%
        dplyr::select(type) %>%
        as.character()
      
      if (is.null(input$layout)) layout <- NA else layout <- input$layout
      
      yamlinfo <- list(
        title = input$title,
        abstract = input$abstract,
        keywords = input$keywords,
        data = input$data,
        disclaimer = input$disclaimer,
        acknowledgements = input$acknowledgements,
        authors = values$authors,
        jel = values$slctcodes$code,
        journal = input$tgtjnl,
        submissionid = input$submissionid,
        firstpage = input$firstpage,
        doublespace = input$doublespace,
        raggedright = input$raggedright,
        tabfigtoend = input$tabfigtoend,
        linenumbers = input$linenumbers,
        layout = layout,
        date = "`r format(Sys.time(), '%B %d, %Y')`",
        bibliography = "ref.bib"
      )
      
      save(yamlinfo, file = "data/yamlinfo.RData")
      
      if (type == "elsevier") {
        
        mdauthors <- c("author:\n")
        for (i in 1:nrow(values$authors)){
          tmp1 <- values$authors[i,]
          tmp2 <- c(
            paste0("  - name: ", tmp1$name[[1]], "\n"),
            paste0("    email: ", tmp1$email[[1]], "\n"),
            paste0("    affiliation: ", tmp1$affiliation[[1]], "\n")
          )
          
          if (tmp1$corresponding[[1]]) tmp2 <- c(tmp2, "    footnote: Corresponding author.\n")
          
          mdauthors <- c(mdauthors, tmp2)
          rm(tmp1, tmp2)
        }
        
        mdaddress <- c("address:\n")
        institutions <- values$authors %>%
          dplyr::select(affiliation, address) %>%
          unique()
        
        for (i in 1:nrow(institutions)){
          tmp1 <- institutions[i,]
          tmp2 <- c(
            paste0("  - code: ", tmp1$affiliation[[1]], "\n"),
            paste0("    address: ", tmp1$address[[1]], "\n")
          )
          
          mdaddress <- c(mdaddress, tmp2)
          rm(tmp1, tmp2)
        }
        
        mdoutput <- c(
          "output:\n",
          "  writR::gen_paper:\n",
          "     engine: xelatex\n",
          paste0("     journal: ", yamlinfo$journal, "\n")
        )
        
        
        yaml <- c(
          "---\n",
          paste0('title: "', yamlinfo$title, '"\n'),
          
          mdauthors,
          mdaddress,
          
          paste0("abstract: |\n  ", yamlinfo$abstract, "\n"),
          paste0("keywords: ", yamlinfo$keywords, "\n"),
          paste0("jel: ", paste0(yamlinfo$jel, collapse = ", "), "\n"),
          paste0("data: ", yamlinfo$data, "\n"),
          
          paste0("bibliography: ", yamlinfo$bibliography, "\n"),
          
          paste0("tabfigtoend: ", yamlinfo$tabfigtoend, "\n"),
          paste0("linenumbers: ", yamlinfo$linenumbers, "\n"),
          
          mdoutput,
          "---"
        )
        
        rstudioapi::insertText(paste0(yaml, collapse = ""))
        
        
        
      } else if (type == "informs") {
        
        mdauthors <- c("author:\n")
        for (i in 1:nrow(values$authors)){
          tmp1 <- values$authors[i,]
          tmp2 <- c(
            paste0("  - name: ", tmp1$name[[1]], "\n"),
            paste0("    email: ", tmp1$email[[1]], "\n"),
            paste0("    affiliation: ", tmp1$affiliation[[1]], "\n"),
            paste0("    address: ", tmp1$address[[1]], "\n")
          )
          
          if (tmp1$corresponding[[1]]) tmp2 <- c(tmp2, "    footnote: Corresponding author.\n")
          
          mdauthors <- c(mdauthors, tmp2)
          rm(tmp1, tmp2)
        }
        
        mdoutput <- c(
          "output:\n",
          "  writR::gen_paper:\n",
          "     engine: xelatex\n",
          paste0("     journal: ", yamlinfo$journal, "\n")
        )
        
        
        yaml <- c(
          "---\n",
          paste0('title: "', yamlinfo$title, '"\n'),
          
          mdauthors,
          
          paste0('acknowledgements: "', yamlinfo$acknowledgements, '"\n'),
          
          paste0("abstract: |\n  ", yamlinfo$abstract, "\n"),
          paste0("keywords: ", yamlinfo$keywords, "\n"),
          
          paste0("submissionid: ", yamlinfo$submissionid, "\n"),
          paste0("bibliography: ", yamlinfo$bibliography, "\n"),
          
          paste0("layout: ", yamlinfo$layout, "\n"),
          paste0("informsjnl: ", yamlinfo$journal, "\n"),
          
          mdoutput,
          "---"
        )
        
        rstudioapi::insertText(paste0(yaml, collapse = ""))
     
        
      } else {
        
        mdauthors <- c("author:\n")
        for (i in 1:nrow(values$authors)){
          tmp1 <- values$authors[i,]
          tmp2 <- c(
            paste0("  - name: ", tmp1$name[[1]], "\n"),
            paste0("    email: ", tmp1$email[[1]], "\n"),
            paste0("    affiliation: ", tmp1$affiliation[[1]], "\n"),
            paste0("    address: ", tmp1$address[[1]], "\n")
          )
          
          if (tmp1$corresponding[[1]]) tmp2 <- c(tmp2, "    footnote: Corresponding author.\n")
          
          mdauthors <- c(mdauthors, tmp2)
          rm(tmp1, tmp2)
        }
        
        mdoutput <- c(
          "output:\n",
          "  writR::gen_paper:\n",
          "     engine: xelatex\n",
          paste0("     journal: ", yamlinfo$journal, "\n")
        )
        
        
        yaml <- c(
          "---\n",
          paste0('title: "', yamlinfo$title, '"\n'),
          
          mdauthors,
          
          paste0('date: "', yamlinfo$date, '"\n'),
          paste0('disclaimer: "', yamlinfo$disclaimer, '"\n'),
          paste0('acknowledgements: "', yamlinfo$acknowledgements, '"\n'),
          
          paste0("abstract: |\n  ", yamlinfo$abstract, "\n"),
          paste0("keywords: ", yamlinfo$keywords, "\n"),
          paste0("jel: ", paste0(yamlinfo$jel, collapse = ", "), "\n"),
          paste0("data: ", yamlinfo$data, "\n"),
          
          paste0("submissionid: ", yamlinfo$submissionid, "\n"),
          paste0("firstpage: ", yamlinfo$firstpage, "\n"),
          paste0("bibliography: ", yamlinfo$bibliography, "\n"),
          
          paste0("doublespace: ", yamlinfo$doublespace, "\n"),
          paste0("tabfigtoend: ", yamlinfo$tabfigtoend, "\n"),
          paste0("raggedright: ", yamlinfo$raggedright, "\n"),
          paste0("linenumbers: ", yamlinfo$linenumbers, "\n"),
          
          mdoutput,
          "---"
        )
        
        rstudioapi::insertText(paste0(yaml, collapse = ""))
        
      }
      
      
      stopApp()
    })
  }
  runGadget(ui, server, viewer = paneViewer(minHeight = "maximize"))
}
