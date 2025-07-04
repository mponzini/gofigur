server <- function(input, output, session) {
  data <- gofigur:::importServer("import")
  data_class <- gofigur:::dataServer("data", data = data)
  
  output$info <- DT::renderDataTable({
    if((!is.null(data_class()))) {
      DT::datatable(data_class(), options = list(ordering = FALSE))
    }
  })
  
  # modules for each figure type
  gofigur:::histogramServer("hist", data = data, data_class = data_class)
  gofigur:::boxServer("box", data = data, data_class = data_class)
  gofigur:::tntServer("tnt", data = data, data_class = data_class)
  gofigur:::barServer("bar", data = data, data_class = data_class)
  gofigur:::scatterServer("scatter", data = data, data_class = data_class)
  gofigur:::kmServer("km", data = data, data_class = data_class)
  gofigur:::providedServer("provided", data = data)
  
}