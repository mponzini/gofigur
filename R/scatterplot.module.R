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
    by_label <- reactive({
      ifelse(input$by_lab == "", paste(by_var()), input$by_lab)
    })
    
    # create tmp data for plot
    plot_data <- reactive({
      shiny::req(data())
      shiny::req(all(class(data()) != "gg"))
      
      data() |> 
        dplyr::mutate(
          dplyr::across(
            .cols = dplyr::any_of(c(by_var())),
            ~ .x |> as.factor()
          )
        )
    })
    
    # conditional aes
    gg_aes <- reactive({
      if (by_var() != "No group") {
        ggplot2::aes(
          x = .data[[x_var()]],
          y = .data[[y_var()]],
          color = .data[[by_var()]]
        )
      } else {
        ggplot2::aes(
          x = .data[[x_var()]],
          y = .data[[y_var()]]
        )
      }
    })

    
    # plot
    renderPlot({
      plot_data() |> 
        ggplot2::ggplot() +
        gg_aes() +
        ggplot2::geom_point() +
        ggplot2::labs(
          x = x_label(),
          y = y_label(),
          color = by_label()
        ) +
        ggplot2::theme_bw() +
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

