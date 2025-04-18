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
    plot <- reactive({
      shiny::req(data())
      shiny::req(any(class(data()) != "gg"))
      
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