# ui #
library(shiny)
library(dplyr)
library(tibble)
library(openxlsx)
library(readxl)
library(shinythemes)
library(ggplot2)
library(ggtext)

ui <- navbarPage(
  # add favicon to browser tab
  tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
  # tags$head(tags$link(rel="shortcut icon", href="favicon.ico")),
  # replace title with logo on navbar
  title = div(img(src="CTSC_Data_Loofah_Icon.png",
                  width = "90px", height = "60px")),
  # title in browser tab
  windowTitle = "CTSC Figures",
  # adjust dimensions of navbar to accommodate logo
  header = fresh::use_theme(
    fresh::create_theme(
      theme = "cerulean",
      fresh::bs_vars_navbar(
        height = "90px",
        margin_bottom = "15px",
        padding_vertical = "15px"
        
      )
    )
  ),
  # theme = shinythemes::shinytheme(theme = "cyborg"),
  
  tabPanel(
    "Intro",
    
    fluidPage(
      fluidRow(
        h3("Purpose"),
        p("The purpose of this tool is to enable investigators to create ",
          "high quality {ggplot2} figures without coding knowledge.")
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
      ),
      tabPanel("Provided",
               sidebarLayout(
                 sidebarPanel(
                   providedUI("provided")
                 ),
                 mainPanel(
                   plotOutput("provided"),
                   downloadUI()
                 )
               )
      )
    )
  )
)