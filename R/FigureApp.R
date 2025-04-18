#' @export

FigureApp <- function(){
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
        histogramUI("hist"),
        scatterUI("scatter"),
        boxUI("box"),
        barUI("bar"),
        providedUI("provided")
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
    
    histogramServer("hist", data = data, data_class = data_class)
    boxServer("box", data = data, data_class = data_class)
    barServer("bar", data = data, data_class = data_class)
    scatterServer("scatter", data = data, data_class = data_class)
    providedServer("provided", data = data)
    
  }
  
  shiny::shinyApp(ui = ui, server = server)
}