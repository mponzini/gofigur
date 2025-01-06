## module for data import ##
importUI <- function(id) {
  # htmltools::tagList(
    sidebarLayout(
      sidebarPanel(
        fileInput(
          NS(id, "upload"), 
          NULL, 
          accept = c(
            ".csv", ".xlsx", ".xls", ".sas7bdat", ".sav", ".dta", ".rds"
          )
        ),
        selectInput(NS(id, 'sheet'), "Choose Sheet",  NULL)
      ),
      mainPanel(
        fluidRow(
          column(
            11,
            p(textOutput(NS(id, "dataInfo")))
          )
        ),
        fluidRow(
          column(
            11,
            DT::dataTableOutput(NS(id, "info"))
          )
        )
      )
    )
  # )
}

importServer <- function(id) {
  shiny::moduleServer(id, function(input, output, session) {
    # get sheet names
    sheetNames <- reactive({
      req(input$upload)
      ext <- tools::file_ext(input$upload$name)
      if(ext == 'xls' | ext == "xlsx"){
        readxl::excel_sheets(input$upload$datapath)
      } else {
        "No Sheets"
      }
    })
    
    # update dropdown menu after data is imported
    observe({
      updateSelectInput(
        session, "sheet", choices = sheetNames()
      )
    })
    
    
    # import data using appropriate function
    data <- reactive({
      if((!is.null(input$upload)) && (input$sheet != "")){
        ext <- tools::file_ext(input$upload$name)
        # import data using appropriate function based on file type
        switch(
          ext,
          csv = read.csv(input$upload$datapath),
          xls = readxl::read_xls(input$upload$datapath, sheet = input$sheet),
          xlsx = openxlsx::read.xlsx(input$upload$datapath, sheet = input$sheet),
          sas7bdat = haven::read_sas(input$upload$datapath),
          sav = haven::read_spss(input$upload$datapath),
          dta = haven::read_dta(input$upload$datapath),
          rds = readRDS(input$upload$datapath),
          validate(paste0("Invalid file; Please upload a file of the following",
                          " types: .csv, .xls, .xlsx, .sas7bdat, .sav, .dta, .rds"))
        )
      } else {
        NULL
      }
    })
    
    # Display message to import data file or summary of data file (nrow, ncol)
    data_info <- reactive({
      if((!is.null(data()))){
        text <- paste0("The data has ", nrow(data()), " rows and ", ncol(data()),
                       " columns. The variable types are displayed below.",
                       " Please review each variable and check that its class",
                       " (numeric or character) is as expected.")
      } else {
        text <- paste0("Import your data file.
                     Accepted file types are xls, xlsx",
                     ", csv, R (rds), SAS (sas7bdat), Stata (dta), or SPSS (sav).")
      }
      text
    })
    
    output$dataInfo <- renderText(data_info())
    
    # get class of each variable
    data_class <- reactive({
      if((!is.null(data()))){
        data.frame(
          "Variable" = colnames(data()),
          "Class" = sapply(1:ncol(data()), function(x){class(data()[[x]])})
        )
      }
    })
    
    # display variable names and classes in table
    output$info <- DT::renderDataTable({
      DT::datatable(data_class(),
                    options = list(ordering = FALSE))
    })
  })
}


dataImportApp <- function() {
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
      importUI("import")
    ),
    
    tabPanel(
      "Figure",
      fluidPage(
        fluidRow(
          p("Placeholder for Figure page")
        )
      )
    )
  )
  
  server <- function(input, output, session) {
    importServer("import")
  }
  
  shinyApp(ui, server)
}

dataImportApp()
