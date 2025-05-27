#' @export

downloadUI <- function(id){
  tagList(
    column(
      width = 8,
      selectInput(
        inputId = NS(id, "plot_device"),
        label = "Figure File Type:",
        choices = c("jpeg", "png", "tiff", "pdf", "rds"),
        selected = NULL,
        multiple = FALSE
      ),
      uiOutput(NS(id, "downloadOpts"))
    ),
    column(
      width = 4,
      downloadButton(
        outputId = NS(id, "downloadPlot"),
        label = "Download Figure"
      )
    )
  )
}


# downloadServer <- function(id) {
#   shiny::moduleServer(id, function(input, output, session) {
#     
#     opts <- reactive({
#       if(input$plot_device == "rds") {
#         list(
#           textInput(
#             inputId = NS(id, "plot_name"),
#             label = "Figure File Name:",
#             value = "figure-name"
#           )
#         )
#       } else {
#         list(
#           textInput(
#             inputId = NS(id, "plot_name"),
#             label = "Figure File Name:",
#             value = "figure-name"
#           ),
#           numericInput(
#             inputId = NS(id, "plot_height"),
#             label = "Figure Height:",
#             value = 6,
#             min = 1
#           ),
#           numericInput(
#             inputId = NS(id, "plot_width"),
#             label = "Figure Width:",
#             value = 6,
#             min = 1
#           ),
#           numericInput(
#             inputId = NS(id, "plot_dpi"),
#             label = "Figure Dots per Inch (DPI):",
#             value = 300,
#             min = 1,
#             max = 600
#           )
#         )
#       }
#     })
#     
#     output$downloadOpts <- renderUI(opts())
#   })
# }