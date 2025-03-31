#' @title Scatterplot Module UI
#' @export

scatterUI <- function(id) {
  htmltools::tagList(
    select.x.var.input(NS(id, "x.var")),
    select.y.var.input(NS(id, "y.var")),
    select.by.var.input(NS(id, "by.var")),
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

#' @title Scatterplot Module Server
#' @export

scatterServer <- function(id, data, data_class) {
  shiny::moduleServer(id, function(input, output, session) {
    # select variables
    x_var <- select.x.var.server("x.var", data)
    y_var <- select.y.var.server("y.var", data)
    by_var <- select.by.var.server("by.var", data)
    # Update axis labels
    x_label <- reactive({
      ifelse(input$x_lab == "", paste(x_var()), input$x_lab)
    })
    y_label <- reactive({
      ifelse(input$y_lab == "", paste(y_var()), input$y_lab)
    })
    # axis title and text size
    # x_text <- reactive({ifelse(input$x_text )})
    # y_text
    # x_title
    # y_title
    
    # plot
    renderPlot({
      data() |> 
        ggplot2::ggplot() +
        ggplot2::aes(
          x = !!rlang::sym(x_var()),
          y = !!rlang::sym(y_var()),
          color = !!rlang::sym(by_var())
        ) +
        ggplot2::geom_point() +
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

