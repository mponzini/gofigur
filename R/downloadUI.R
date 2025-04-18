#' @export

downloadUI <- function(id){
  tagList(
    column(
      width = 8,
      textInput(
        inputId = NS(id, "plot_name"),
        label = "Figure File Name:",
        value = "figure-name"
      ),
      selectInput(
        inputId = NS(id, "plot_device"),
        label = "Figure File Type:",
        choices = c("jpeg", "png", "tiff", "pdf"),
        selected = "jpeg",
        multiple = FALSE
      ),
      numericInput(
        inputId = NS(id, "plot_height"),
        label = "Figure Height:",
        value = 6,
        min = 1
      ),
      numericInput(
        inputId = NS(id, "plot_width"),
        label = "Figure Width:",
        value = 6,
        min = 1
      ),
      numericInput(
        inputId = NS(id, "plot_dpi"),
        label = "Figure Dots per Inch (DPI):",
        value = 300,
        min = 1,
        max = 600
      )
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
