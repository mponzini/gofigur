# ui #
library(shiny)
library(dplyr)
library(tibble)
library(readxl)
library(shinythemes)
library(ggplot2)
library(ggtext)

colorblind_palette <- c(
  "#88CCEE", "#CC6677", "#DDCC77", "#117733", "#332288", "#AA4499", 
  "#44AA99", "#999933", "#882255", "#661100", "#6699CC", "#888888"
)

options(
  ggplot2.discrete.fill = colorblind_palette,
  ggplot2.discrete.colour = colorblind_palette
)

ui <- navbarPage(
  # add favicon to browser tab
  # tags$head(tags$link(rel="shortcut icon", href="go figur 64mp.png")),
  tags$head(tags$link(rel="shortcut icon", href="go figur 64mp.ico")),
  # replace title with logo on navbar
  title = div(img(src="Go FiguR Graphic v6.1.png",
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
  
  # landing page
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
  
  # data import
  tabPanel(
    "Data Import",
    gofigur:::importUI("import"),
    gofigur:::dataUI("data")
  ),
  # Figure tabset panel for each supported figure type
  tabPanel(
    "Figure",
    tabsetPanel(
      id = "Figure Type",
      gofigur:::histogramUI("hist"),
      gofigur:::scatterUI("scatter"),
      gofigur:::boxUI("box"),
      gofigur:::tntUI("tnt"),
      gofigur:::barUI("bar"),
      gofigur:::kmUI("km"),
      gofigur:::providedUI("provided")
    )
  )
)