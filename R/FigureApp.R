#' @title Shiny App to generate Figures
#' @description
#' Use `FigureApp()` to run the self-service Figure App
#' 
#' @import dplyr
#' @import tibble
#' @import ggplot2
#' @import openxlsx
#' @import readxl
#' @import DT
#' @import haven
#' @import shiny
#' @import ggtext
#' 
#' @export
#' 

FigureApp <- function(...) {
  ui <- navbarPage(
    # add favicon to browser tab
    tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
    # tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
    # replace title with logo on navbar
    title = div(img(src="CTSC_Data_Loofah_Icon.png",
                    width = "90px", height = "60px")),
    # title in browser tab
    windowTitle = "CTSC Data Loofah",
    # adjust dimensions of navbar to accommodate logo
    header = fresh::use_theme(
      fresh::create_theme(
        theme = "default",
        fresh::bs_vars_navbar(
          height = "90px",
          margin_bottom = "15px",
          padding_vertical = "15px"
          
        )
      )
    ),
    
    tabPanel(
      "Intro",
      
      fluidPage(
        fluidRow(
          h3("Purpose"),
          p("The purpose of this tool is to ",
            "investigate the data and its quality prior to analysis. The ",
            "goal is to catch data issues such as:")
        )
      )
    ),
    
    
    tabPanel(
      "Data Import",
      importUI("import"),
      dataUI("data")
    ),
    
    tabPanel(
      "Figure",
      tabsetPanel(
        id = "Figure Type",
        tabPanel("Histogram",
                 sidebarLayout(
                   sidebarPanel(
                     histogramUI("hist")
                   ),
                   mainPanel(
                     plotOutput("hist"),
                     downloadUI()
                   )
                 )
        ),
        tabPanel("Scatterplot",
                 sidebarLayout(
                   sidebarPanel(
                     scatterUI("scatter")
                   ),
                   mainPanel(
                     plotOutput("scatter"),
                     downloadUI()
                   )
                 )
        ),
        tabPanel("Boxplot",
                 sidebarLayout(
                   sidebarPanel(
                     boxUI("box")
                   ),
                   mainPanel(
                     plotOutput("box"),
                     downloadUI()
                   )
                 )
        ),
        tabPanel("Barplot",
                 sidebarLayout(
                   sidebarPanel(
                     barUI("bar")
                   ),
                   mainPanel(
                     plotOutput("bar"),
                     downloadUI()
                   )
                 )
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    data <- importServer("import")
    data_class <- dataServer("data", data = data)
    
    output$info <- DT::renderDataTable({
      if((!is.null(data_class()))) {
        DT::datatable(data_class(), options = list(ordering = FALSE))
      }
    })
    
    output$hist <- histogramServer("hist", data = data, data_class = data_class)
    output$box <- boxServer("box", data = data, data_class = data_class)
    output$bar <- barServer("bar", data = data, data_class = data_class)
    output$scatter <- scatterServer("scatter", data = data, data_class = data_class)
  }
  
  shinyApp(ui, server, ...)
}
