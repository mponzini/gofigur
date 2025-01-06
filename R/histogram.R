## Module for histogram ##
histogramUI <- function(id) {
  htmltools::tagList(
    selectInput(
      inputId = NS(id, "x_var"),
      label = "X Variable:",
      choices = c(""), selected = NULL, multiple = FALSE
    ),
    numericInput(
      inputId = NS(id, "numb_bins"),
      label = "Number of Bins:",
      value = 20,
      min = 1
    ),
    selectInput(
      inputId = NS(id, "hist_fill"),
      label = "Histogram Color:",
      choices = c("grey35", "black", "grey20", "grey50"),
      selected = "grey35", 
      multiple = FALSE
    )
  )
}

histogramServer <- function(id, data, data_class) {
  shiny::moduleServer(id, function(input, output, session) {
    # Update x var selection
    shiny::updateSelectInput(
      session,
      "x_var",
      choices = data_class() |>
        dplyr::filter(Class %in% c('numeric', 'integer')) |>
        dplyr::pull(Variable)
    )
    # Update axis labels
    x_label <- reactive({
      ifelse(input$x_lab == "", paste(input$x_var), input$x_lab)
    })
    y_label <- reactive({
      ifelse(input$y_lab == "", "count", input$y_lab)
    })
  })
}