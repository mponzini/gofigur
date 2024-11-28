server <- function(input, output, session) {
  ###################
  # data import tab #
  ###################
  # extract sheet names from Excel files
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
        xlsx = readxl::read_xlsx(input$upload$datapath, sheet = input$sheet),
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
  
  
  ## Figure ##
  # tabset panel
  shiny::observeEvent(
    input$type, {
      shiny::updateTabsetPanel(
        inputId = 'params',
        selected = input$type
      )
    }
  )
  
  plot_type <- reactive({
    input$type
  })
  
  ## update input choices based on plot type? ##
  # update x_var
  shiny::observeEvent(
    plot_type(), {
      switch(
        plot_type(),
        Histogram = shiny::updateSelectInput(
          session,
          "x_var_hist",
          choices = data_class() |>
            dplyr::filter(Class %in% c('numeric', 'integer')) |>
            dplyr::pull(Variable)
        ),
        Boxplot = shiny::updateSelectInput(
          session,
          'x_var_box',
          choices = data_class() |>
            dplyr::filter(Class %in% c('character', 'factor', 'logical')) |>
            dplyr::pull(Variable)
        )
      )
    }
  )
  # update y_var
  
  
  x_label <- reactive({
    ifelse(input$x_lab == "", paste(input$x_var), input$x_lab)
  })
  y_label <- reactive({
    ifelse(input$y_lab == "", "count", input$y_lab)
  })
  
  ## ggplot ##
  # ggplot2 geom based on selected plot type
  plot_geom <- shiny::reactive({
    switch(
      plot_type(),
      Histogram = ggplot2::ggplot(
        data = data(),
        ggplot2::aes(
          x = .data[[input$x_var]]
        )
      ) +
        ggplot2::geom_histogram(
          bins = input$numb_bins,
          fill = input$hist_fill
        ),
      Boxplot = ggplot2::ggplot(
        data = data(),
        ggplot2::aes(
          x = .data[[input$x_var]],
          y = .data[[input$y_var]]
        )
      ) +
        ggplot2::geom_boxplot()
    )
  })
  
  
  output$plot <- shiny::renderPlot({
    req(plot_geom())
    plot_geom() +
      ggplot2::labs(x = x_label(), y = y_label()) +
      ggplot2::theme_bw() +
      ggplot2::theme(
        # update axis title
        axis.title.x = ggtext::element_markdown(
          size = input$x_title_size
        ),
        axis.title.y = ggtext::element_markdown(
          size = input$y_title_size
        ),
        # update axis text
        axis.text.x = ggtext::element_markdown(
          size = input$x_size
        ),
        axis.text.y = ggtext::element_markdown(
          size = input$y_size
        )
      )
  })
  
  
}