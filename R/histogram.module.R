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
    ),
    # textInput(
    #   inputId = NS(id, "by_lab"),
    #   label = "Legend Label"
    # ),
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
    )#,
    # numericInput(
    #   inputId = NS(id, "by_lab_size"),
    #   label = "Legend Label Font Size",
    #   value = 14,
    #   min = 1,
    #   step = 0.5
    # ),
    # numericInput(
    #   inputId = NS(id, "by_text_size"),
    #   label = "Legend Text Font Size",
    #   value = 14,
    #   min = 1,
    #   step = 0.5
    # )
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
    # by_label <- reactive({
    #   ifelse(input$by_lab == "", paste(by_var()), input$by_lab)
    # })
    
    # create tmp data for plot
    plot_data <- reactive({
      data() #|> 
        # dplyr::mutate(
        #   dplyr::across(
        #     .cols = dplyr::any_of(c(by_var())),
        #     ~ .x |> as.factor()
        #   )
        # )
    })
    
    # conditional aes
    # gg_aes <- reactive({
    #   if (by_var() != "No group") {
    #     ggplot2::aes(
    #       x = .data[[x_var()]],
    #       y = .data[[y_var()]],
    #       color = .data[[by_var()]]
    #     )
    #   } else {
    #     ggplot2::aes(
    #       x = .data[[x_var()]],
    #       y = .data[[y_var()]]
    #     )
    #   }
    # })
    
    # plot
    output$plot <- renderPlot({
      data() |> 
        ggplot2::ggplot() +
        ggplot2::aes(
          x = .data[[x_var()]]
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
          )#,
          # # legend
          # legend.title = ggtext::element_markdown(
          #   size = input$by_lab_size
          # ),
          # legend.text = ggtext::element_markdown(
          #   size = input$by_text_size
          # )
        )
    })
  })
}

