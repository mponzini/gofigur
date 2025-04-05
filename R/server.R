server <- function(input, output, session) {
  data <- importServer("import")
  data_class <- dataServer("data", data = data)
  
  output$info <- DT::renderDataTable({
    if((!is.null(data_class()))) {
      DT::datatable(data_class(), options = list(ordering = FALSE))
    }
  })
  
  # observeEvent(input$type, {
  #   updateTabsetPanel(inputId = "figure", selected = input$type)
  # })
  
  # x_var <- select.x.var.server("x.var", data = data)
  # y_var <- select.y.var.server("y.var", data = data)
  # by_var <- select.by.var.server("by.var", data = data)
  
  # output$x.var <- renderText(x_var())
  # output$y.var <- renderText(y_var())
  # output$by.var <- renderText(by_var())
  
  # output$plot <- histogramServer("hist", data = data, data_class = data_class)
  # output$plot <- boxServer("box", data = data, data_class = data_class)
  # output$plot <- barServer("bar", data = data, data_class = data_class)
  reactive({
    if(input$type == "Histogram") {
      histogramServer("hist", data = data, data_class = data_class)
    } else if (input$type == "Scatterplot") {
      scatterServer("scatter", data = data, data_class = data_class)
    } else if (input$type == "Boxplot") {
      boxServer("box", data = data, data_class = data_class)
    } else if (input$type == "Barplot") {
      barServer("bar", data = data, data_class = data_class)
    }
  })
  
}