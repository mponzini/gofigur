pacman::p_load(shiny, shinydashboard, ggplot2, cowplot, dplyr, tibble, tidyr,
               readxl, haven, fresh, htmltools)

ui <- shiny::navbarPage(
  # add favicon to browser tab
  # tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
  # tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  # replace title with logo on navbar
  title = div(img(src="CTSC_Data_Loofah_Icon.png",
                  width = "90px", height = "60px")),
  # title in browser tab
  windowTitle = "CTSC Self-Service Figures",
  # adjust dimensions of navbar to accommodate logo
  header = use_theme(
    create_theme(
      theme = "default",
      bs_vars_navbar(
        height = "90px",
        margin_bottom = "15px",
        padding_vertical = "15px"
        
      )
    )
  ),
  # Landing panel with details on apps purpose and brief instructions #
  shiny::tabPanel(
    "Introduction",
    
    shiny::fluidPage(
      shiny::fluidRow(
        htmltools::h3("Purpose"),
        htmltools::p(
          "The purpose of this tool is to ",
          "allow investigators to generate basic/common figures."
        ),
        htmltools::h3("Instructions"),
        htmltools::tags$ul(
          htmltools::tags$li(
            "To import your data go to the ", htmltools::tags$b("Data Import"),
            " tab and click ", htmltools::tags$b("Browse"),
            " to select your data file. If your data is stored",
            " in an Excel file you can select the sheet to import from",
            " the ", htmltools::tags$b("Sheet")," dropdown."
          ),
          htmltools::tags$li(
            "Once your data is imported, a summary sentence and table ",
            "will appear. The sentence summarizes the number of records",
            " and variables in the data file. The table shows each ",
            "variable in the data file and their class (",
            "how the data are stored). Check that the variables are stored",
            " as expected."
          ),
          htmltools::tags$li(
            "Finally, go to the ", htmltools::tags$b("Figures"), 
            " tab to construct your figure."
          )
        )
      )
    )
  ),
  # Panel for data import by browsing folders #
  shiny::tabPanel(
    "Data Import",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        fileInput(
          "upload", NULL, accept = c(".csv", ".xlsx", ".xls",
                                     ".sas7bdat", ".sav",
                                     ".dta", ".rds")
        ),
        shiny::selectInput('sheet', "Choose Sheet",  NULL)
      ),
      shiny::mainPanel(
        shiny::fluidRow(
          shiny::column(
            11,
            htmltools::p(shiny::textOutput("dataInfo"))
          )
        ),
        shiny::fluidRow(
          shiny::column(
            11,
            DT::dataTableOutput("info")
          )
        ),
      )
    )
  ),
  # Panel to generate Figures
  shiny::tabPanel(
    "Figure",
    shiny::sidebarLayout(
      shiny::sidebarPanel(
        shiny::selectInput(
          "type",
          "Plot Type",
          choices = c(" ", "Histogram", "Boxplot")
        ),
        # common variable panels
        selectInput(
          inputId = "x_var",
          label = "X Variable:",
          choices = c(""), selected = NULL, multiple = FALSE
        ),
        selectInput(
          inputId = "y_var",
          label = "Y Variable:",
          choices = c(""), selected = NULL, multiple = FALSE
        ),
        # conditional panels
        parameter_tabs,
        # common axis/text panels
        textInput(
          inputId = "x_lab",
          label = "X Axis Label:",
          value = NULL
          
        ),
        textInput(
          inputId = "y_lab",
          label = "Y Axis Label:",
          value = NULL
          
        ),
        numericInput(
          inputId = "x_size",
          label = "X Axis Text Size:",
          value = 7,
          min = 1
        ),
        numericInput(
          inputId = "y_size",
          label = "Y Axis Text Size:",
          value = 7,
          min = 1
        ),
        numericInput(
          inputId = "x_title_size",
          label = "X Axis Title Text Size:",
          value = 10,
          min = 1
        ),
        numericInput(
          inputId = "y_title_size",
          label = "Y Axis Title Text Size:",
          value = 10,
          min = 1
        )
      ),
      shiny::mainPanel(
        shiny::plotOutput("plot")
      )
    )
  )
)
