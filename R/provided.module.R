#' @title Provided Figure Module UI
#' @export

providedUI <- function(id) {
  htmltools::tagList(
    textInput(
      inputId = NS(id, "x_lab"),
      label = "X-axis Label"
    ),
    textInput(
      inputId = NS(id, "y_lab"),
      label = "Y-axis Label"
    ),
    textInput(
      inputId = NS(id, "by_lab"),
      label = "Legend Label"
    ),
    numericInput(
      inputId = NS(id, "x_lab_size"),
      label = "X-axis Label Font Size",
      value = 14,
      min = 1,
      step = 0.5
    ),
    numericInput(
      inputId = NS(id, "x_text_size"),
      label = "X-axis Text Font Size",
      value = 14,
      min = 1,
      step = 0.5
    ),
    numericInput(
      inputId = NS(id, "y_lab_size"),
      label = "Y-axis Label Font Size",
      value = 14,
      min = 1,
      step = 0.5
    ),
    numericInput(
      inputId = NS(id, "y_text_size"),
      label = "Y-axis Text Font Size",
      value = 14,
      min = 1,
      step = 0.5
    ),
    numericInput(
      inputId = NS(id, "by_lab_size"),
      label = "Legend Label Font Size",
      value = 14,
      min = 1,
      step = 0.5
    ),
    numericInput(
      inputId = NS(id, "by_text_size"),
      label = "Legend Text Font Size",
      value = 14,
      min = 1,
      step = 0.5
    )
  )
}

#' @title Boxplot Module Server
#' @export

providedServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    # Update axis labels
    x_label <- reactive({
      shiny::req(data())
      shiny::req(any(class(data()) != "gg"))
      
      ifelse(input$x_lab == "", data()$label$x, input$x_lab)
    })
    y_label <- reactive({
      shiny::req(data())
      shiny::req(any(class(data()) != "gg"))
      
      ifelse(input$y_lab == "", data()$label$y, input$y_lab)
    })
    by_label <- reactive({
      shiny::req(data())
      shiny::req(any(class(data()) != "gg"))
      
      ifelse(input$by_lab == "", paste("Provide By Label"), input$by_lab)
    })
    
    
    # plot
    renderPlot({
      data() +
        ggplot2::labs(
          x = x_label(),
          y = y_label()
        ) +
        ggplot2::theme(
          # x-axis
          axis.title.x = ggtext::element_markdown(
            size = input$x_lab_size
          ),
          axis.text.x = ggtext::element_markdown(
            size = input$x_text_size
          ),
          # y-axis
          axis.title.y = ggtext::element_markdown(
            size = input$y_lab_size
          ),
          axis.text.y = ggtext::element_markdown(
            size = input$y_text_size
          ),
          # legend
          legend.title = ggtext::element_markdown(
            size = input$by_lab_size
          ),
          legend.text = ggtext::element_markdown(
            size = input$by_text_size
          )
        )
    })
  })
}