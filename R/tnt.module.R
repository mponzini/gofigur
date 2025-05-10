tntUI <- function(id) {
  tabPanel(
    "Barplot",
    sidebarLayout(
      sidebarPanel(
        select.x.var.input(NS(id, "x.var")),
        select.y.var.input(NS(id, "y.var")),
        select.by.var.input(NS(id, "by.var")),
        labels_and_fonts("tnt")
      ),
      mainPanel(
        plotOutput(NS(id, "tnt")),
        downloadUI("tnt")
      )
    )
  )
}

tntServer <- function(id, data, data_class) {
  shiny::moduleServer(id, function(input, output, session) {
    # select x and by variables
    x_var <- select.x.var.server("x.var", data)
    y_var <- select.y.var.server("y.var", data)
    by_var <- select.by.var.server("by.var", data)
    # Update axis labels
    x_label <- reactive({
      ifelse(input$x_lab == "", paste(x_var()), input$x_lab)
    })
    y_label <- reactive({
      ifelse(input$y_lab == "", paste(x_var()), input$y_lab)
    })
    by_label <- reactive({
      ifelse(input$by_lab == "", paste(by_var()), input$by_lab)
    })
    
    # create tmp data for plot
    plot_data <- reactive({
      shiny::req(data())
      shiny::req(all(class(data()) != "gg"))
      
      data() |> 
        dplyr::summarise(
          Mean = mean(.data[[y_var()]], na.rm = TRUE)
        )
    })
    
    # conditional aes
    gg_aes <- reactive({
      if (by_var() != "No group") {
        ggplot2::aes(
          x = .data[[x_var()]],
          y = .data[[y_var()]],
          fill = .data[[by_var()]]
        )
      } else {
        ggplot2::aes(
          x = .data[[x_var()]],
          y = .data[[y_var()]]
        )
      }
    })
    
    # plot
    plot <- reactive({
      plot_data() |> 
        ggplot2::ggplot() +
        gg_aes() +
        ggplot2::geom_col() +
        ggplot2::geom_errorbar() +
        ggplot2::labs(
          x = x_label(),
          y = y_label(),
          fill = by_label()
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
    
    output$bar <- renderPlot({plot()})
    
    # download handler
    output$downloadPlot <- downloadHandler(
      filename = function(file) {
        paste(input$plot_name, input$plot_device, sep = ".")
      },
      content = function(file) {
        ggplot2::ggsave(
          file,
          ,
          plot = plot(),
          width = input$plot_width,
          height = input$plot_height,
          dpi = input$plot_dpi,
          device = input$plot_device
        )
      }
    )
  })
}
