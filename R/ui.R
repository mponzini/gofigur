## ui ##
library(shiny)

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
    sidebarLayout(
      sidebarPanel(
        selectInput("type", "Plot Type", choices = c("Histogram", "Scatterplot", "Boxplot", "Barplot")),
        conditionalPanel(
          condition = 'input.type == "Histogram"',
          histogramUI("hist")
        ),
        conditionalPanel(
          condition = 'input.type == "Scatterplot"',
          scatterUI("scatter")
        ),
        conditionalPanel(
          condition = 'input.type == "Boxplot"',
          boxUI("box")
        ),
        conditionalPanel(
          condition = 'input.type == "Barplot"',
          barUI("bar")
        )
        # select.y.var.input("y.var"),
        # select.by.var.input("by.var")
      ),
      mainPanel(
        plotOutput("plot"),
        downloadUI()
      )
    )
  )
)