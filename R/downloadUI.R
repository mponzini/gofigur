downloadUI <- function(id){
  tagList(
    column(
      width = 8,
      textInput(
        inputId = "plot_name",
        label = "Figure File Name:",
        value = "figure-name"
      ),
      selectInput(
        inputId = "device",
        label = "Figure File Type:",
        choices = c("jpeg", "png", "tiff", "pdf"),
        selected = "jpeg",
        multiple = FALSE
      ),
      numericInput(
        inputId = "plot_height",
        label = "Figure Height:",
        value = 6,
        min = 1
      ),
      numericInput(
        inputId = "plot_width",
        label = "Figure Width:",
        value = 6,
        min = 1
      ),
      numericInput(
        inputId = "plot_dpi",
        label = "Figure Dots per Inch (DPI):",
        value = 300,
        min = 1,
        max = 600
      )
    ),
    column(
      width = 4,
      downloadButton(
        outputId = "downloadPlot",
        label = "Download Figure"
      )
    )
  )
}