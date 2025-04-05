#' @title Barplot Module UI
#' @export

barUI <- function(id) {
  htmltools::tagList(
    select.x.var.input(NS(id, "x.var")),
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

#' @title Barplot Module Server
#' @export

barServer <- function(id, data, data_class) {
  shiny::moduleServer(id, function(input, output, session) {
    # select x and by variables
    x_var <- select.x.var.server("x.var", data)
    by_var <- select.by.var.server("by.var", data)
    # Update axis labels
    x_label <- reactive({
      ifelse(input$x_lab == "", paste(x_var()), input$x_lab)
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
    renderPlot({
      data() |> 
        ggplot2::ggplot() +
        ggplot2::aes(
          x = !!rlang::sym(x_var()),
          fill = !!rlang::sym(by_var())
        ) +
        ggplot2::geom_bar() +
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
