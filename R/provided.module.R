providedUI <- function(id) {
  tabPanel(
    "Provided",
    sidebarLayout(
      sidebarPanel(
        labels_and_fonts("provided", by = TRUE)
      ),
      mainPanel(
        plotOutput(NS(id, "provided")),
        downloadUI("provided")
      )
    )
  )
}

providedServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    # extract labels to generate appropriate 'Label' UI fields
    map_labels <- reactive({
      shiny::req(data())
      
      data()$labels |> names()
    })
    
    # Update axis labels
    x_label <- reactive({
      ifelse(input$x_lab == "", data()$label$x, input$x_lab)
    })
    y_label <- reactive({
      ifelse(input$y_lab == "", data()$label$y, input$y_lab)
    })
    
    # update the 'labels' list in the plot data
    plot_data <- reactive({
      shiny::req(data())
      
      data()
    })
    
    plot_data()$labels <- new_labels()
    
    
    # plot
    plot <- reactive({
      if (any(class(data()) == "gg")) {
        plot_data() +
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
      } else {
        ggplot2::ggplot() +
          ggplot2::aes(
            x = 1,
            y = 1,
            label = "Upload a saved {ggplot2} figure to utilize this panel"
          ) +
          ggplot2::geom_text(size = 6) +
          ggplot2::theme_void()
      }
    })

    output$provided <- renderPlot({plot()})
    
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