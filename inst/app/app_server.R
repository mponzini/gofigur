server <- function(input, output, session) {
  data <- importServer("import")
  data_class <- dataServer("data", data = data)
  
  output$info <- DT::renderDataTable({
    if((!is.null(data_class()))) {
      DT::datatable(data_class(), options = list(ordering = FALSE))
    }
  })
  
  output$hist <- histogramServer("hist", data = data, data_class = data_class)
  output$box <- boxServer("box", data = data, data_class = data_class)
  output$bar <- barServer("bar", data = data, data_class = data_class)
  output$scatter <- scatterServer("scatter", data = data, data_class = data_class)
  output$provided <- providedServer("provided", data = data)
  
}