#' @title Histogram Module UI
#' @export

histogramUI <- function(id) {
  htmltools::tagList(
    select.x.var.input(NS(id, "x.var")),
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
    ),
    textInput(
      inputId = NS(id, "x_lab"),
      label = "X-axis Label"
    ),
    textInput(
      inputId = NS(id, "y_lab"),
      label = "Y-axis Label"
    )
  )
}

#' @title Histogram Module Server
#' @export

histogramServer <- function(id, data, data_class) {
  shiny::moduleServer(id, function(input, output, session) {
    # select x var
    x_var <- select.x.var.server("x.var", data)
    # Update axis labels
    x_label <- reactive({
      ifelse(input$x_lab == "", paste(input$x_var), input$x_lab)
    })
    y_label <- reactive({
      ifelse(input$y_lab == "", "count", input$y_lab)
    })
    # axis title and text size
    # x_text <- reactive({ifelse(input$x_text )})
    # y_text
    # x_title
    # y_title
    
    # plot
    output$plot <- renderPlot({
      data() |> 
        ggplot2::ggplot() +
        ggplot2::aes(
          x = !!rlang::sym(x_var())
        ) +
        ggplot2::geom_histogram(
          fill = input$hist_fill,
          bins = input$numb_bins
        ) +
        ggplot2::labs(
          x = x_label(),
          y = y_label()
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          axis.text.x = ggtext::element_markdown(),
          axis.text.y = ggtext::element_markdown(),
          axis.title.x = ggtext::element_markdown(),
          axis.title.y = ggtext::element_markdown()
        )
    })
  })
}

