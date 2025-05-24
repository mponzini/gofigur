providedUI <- function(id) {
  tabPanel(
    "Provided",
    sidebarLayout(
      sidebarPanel(
        uiOutput(NS(id, "label_ui")),
        fonts("provided", by = TRUE)
      ),
      mainPanel(
        plotOutput(NS(id, "provided")),
        downloadUI("provided"),
        textOutput(NS(id, "test"))
      )
    )
  )
}

providedServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    # extract labels to generate appropriate 'Label' UI fields
    map_labels <- reactive({
      shiny::req(data())
      
      data()$labels
    })
    
    map_labels_names <- reactive({
      shiny::req(data())
      
      map_labels() |> names()
    })
    
    # generate UI
    output$label_ui <- renderUI({
      purrr::map(
        map_labels_names(), 
        function(x) {
          textInput(
            inputId = NS(id, x),
            label = paste(x, "Label", sep = " "),
            value = map_labels()[[x]]
          )
        }
      )
    })
    
    # extract new Labels
    new_labels <- reactive({
      tmp <- purrr::map(
        map_labels_names(),
        function(x) input[[x]]
      )
      
      names(tmp) <- map_labels_names()
      
      tmp
    })
    
    # update the 'labels' list in the plot data
    plot_data <- reactive({
      shiny::req(data())
      
      tmp_data <- data()
      
      tmp_data$labels <- new_labels()
      
      tmp_data
    })
    
    
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