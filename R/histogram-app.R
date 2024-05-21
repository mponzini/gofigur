pacman::p_load(shiny, shinydashboard, dplyr, ggplot2, haven, openxlsx, DT, 
               readxl, fresh, ggtext)

# User Interface
ui <- navbarPage(
  # add favicon to browser tab
  tags$head(tags$link(rel="shortcut icon", href="favicon.png")),
  # replace title with logo on navbar
  title = div(img(src="CTSC_Data_Loofah_Icon.png",
                  width = "90px", height = "60px")),
  # title in browser tab
  windowTitle = "Self Service Figures",
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
  
  # Panel 1: Introduction to the App
  tabPanel(
    "Introduction",
    fluidPage(
      fluidRow(
        # brief purpose of the application
        h3("Purpose"),
        p("The purpose of this tool is to enable investigators to ",
          "create or modify common figures for their ",
          "presentations/manuscripts.")
      ),
      p("This application should be used following data analysis and it not ",
        "meant to be used as an exploratory tool."),
      # brief list of instructions
      h3("Instructions"),
      
      tags$ul(
        tags$li("To import your data go to the ", tags$b("Data Import"),
                " tab and click ", tags$b("Browse"),
                " to select your data file. If your data is stored",
                " in an Excel file you can select the sheet to import from",
                " the ", tags$b("Sheet")," dropdown."),
        tags$li("Once your data is imported, a summary sentence and table ",
                "will appear. The sentence summarizes the number of records",
                " and variables in the data file. The table shows each ",
                "variable in the data file and their class (",
                "how the data are stored). Check that the variables are stored",
                " as expected."),
        tags$li("If your data is imported as expected, go the the ", 
                tags$b("Figure"), " tab to create the figure of your choice.",
                " Supported figures include: scatter plots, bar charts, 
                and box plots. Future support will enable the addition and ",
                "annotation of a regression line to scatter plots.")
      )
    )
  ),
  
  # Panel 2: Data Import (taken directly from the DataLoofah app)
  tabPanel(
    "Data Import",
    sidebarLayout(
      sidebarPanel(
        fileInput(
          "upload", NULL, accept = c(".csv", ".xlsx", ".xls",
                                     ".sas7bdat", ".sav",
                                     ".dta", ".rds")
        ),
        selectInput('sheet', "Choose Sheet",  NULL)
      ),
      mainPanel(
        fluidRow(
          column(
            11,
            p(textOutput("dataInfo"))
          )
        ),
        fluidRow(
          column(
            11,
            DT::dataTableOutput("info")
          )
        ),
      )
    )
  ),
  
  # Panel 3: Figure Creation
  tabPanel(
    "Histogram",
    sidebarPanel(
      selectInput(
        inputId = "plot_type",
        label = "Select Type of Plot:",
        choices = c("", "Histogram", "Boxplot"),
        selected = NULL, multiple = FALSE
      ),
      histogramUI()
      # selectInput(
      #   inputId = "x_var",
      #   label = "X Variable:",
      #   choices = c(""), selected = NULL, multiple = FALSE
      # ),
      # numericInput(
      #   inputId = "numb_bins",
      #   label = "Number of Bins:",
      #   value = 20,
      #   min = 1
      # ),
      # textInput(
      #   inputId = "x_lab",
      #   label = "X Axis Label:",
      #   value = NULL
      #   
      # ),
      # textInput(
      #   inputId = "y_lab",
      #   label = "Y Axis Label:",
      #   value = NULL
      #   
      # ),
      # selectInput(
      #   inputId = "hist_fill",
      #   label = "Histogram Color:",
      #   choices = c("grey35", "black", "grey20", "grey50"),
      #   selected = "grey35", 
      #   multiple = FALSE
      # ),
      # numericInput(
      #   inputId = "x_size",
      #   label = "X Axis Text Size:",
      #   value = 7,
      #   min = 1
      # ),
      # numericInput(
      #   inputId = "y_size",
      #   label = "Y Axis Text Size:",
      #   value = 7,
      #   min = 1
      # ),
      # numericInput(
      #   inputId = "x_title_size",
      #   label = "X Axis Title Text Size:",
      #   value = 10,
      #   min = 1
      # ),
      # numericInput(
      #   inputId = "y_title_size",
      #   label = "Y Axis Title Text Size:",
      #   value = 10,
      #   min = 1
      # )
    ),
    
    mainPanel(
      plotOutput("histogram"),
      # fluidRow(
      #  column(
      #    width = 8,
      #    textInput(
      #      inputId = "plot_name",
      #      label = "Figure File Name:",
      #      value = "figure-name"
      #    ),
      #    selectInput(
      #      inputId = "device",
      #      label = "Figure File Type:",
      #      choices = c("jpeg", "png", "tiff"),
      #      selected = "jpeg",
      #      multiple = FALSE
      #    ),
      #    numericInput(
      #      inputId = "plot_height",
      #      label = "Figure Height:",
      #      value = 6,
      #      min = 1
      #    ),
      #    numericInput(
      #      inputId = "plot_width",
      #      label = "Figure Width:",
      #      value = 6,
      #      min = 1
      #    ),
      #    numericInput(
      #      inputId = "plot_dpi",
      #      label = "Figure Dots per Inch (DPI):",
      #      value = 300,
      #      min = 1,
      #      max = 600
      #    )
      #  ),
      #  column(
      #    width = 4,
      #    downloadButton(
      #      outputId = "downloadPlot",
      #      label = "Download Figure"
      #    )
      #  )
      downloadUI()
      # )
    )
  )
)


# Server
server <- function(input, output, session){
  ## data import (code taken from DataLoofah app) ##
  # get Sheet names if xlsx file in imported
  sheetNames <- reactive({
    req(input$upload)
    ext <- tools::file_ext(input$upload$name)
    if(ext == "xlsx" || ext == "xls"){
      readxl::excel_sheets(input$upload$datapath)
    } else{
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
        xls = read_xls(input$upload$datapath, sheet = input$sheet),
        xlsx = read.xlsx(input$upload$datapath, sheet = input$sheet),
        sas7bdat = read_sas(input$upload$datapath),
        sav = read_spss(input$upload$datapath),
        dta = read_dta(input$upload$datapath),
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
  
  ## Create Histogram ##
  # update options for X Variable
  observe({
    if (!is.null(data())) 
      updateSelectInput(session, "x_var", choices=names(data()))
  })
  x_label <- reactive({
    ifelse(input$x_lab == "", paste(input$x_var), input$x_lab)
  })
  y_label <- reactive({
    ifelse(input$y_lab == "", "count", input$y_lab)
  })
  
  p_histogram <- reactive({
    ggplot(
      data(),
      aes(
        x = !!sym(input$x_var)
      )
    ) +
      geom_histogram(
        bins = input$numb_bins,
        fill = input$hist_fill
      ) +
      labs(x = x_label(), y = y_label()) +
      theme_bw() +
      theme(
        axis.text.x = element_markdown(size = input$x_size),
        axis.text.y = element_markdown(size = input$y_size),
        axis.title.x = element_markdown(size = input$x_title_size),
        axis.title.y = element_markdown(size = input$y_title_size)
      )
  })
  
  output$histogram <- renderPlot({
    print(p_histogram())
  })
  
  output$downloadPlot <- downloadHandler(
    filename = function(){paste0(input$plot_name, ".", input$device)},
    content = function(file){
      ggsave(
        file,
        p_histogram(),
        height = input$plot_height,
        width = input$plot_width,
        dpi = input$plot_dpi
      )
    }
  )
  
}

shinyApp(ui, server)
