parameter_tabs <- shiny::tabsetPanel(
  id = "params",
  type = "hidden",
  shiny::tabPanelBody(
    " "
  ),
  shiny::tabPanelBody(
    "Histogram",
    numericInput(
      inputId = "numb_bins",
      label = "Number of Bins:",
      value = 20,
      min = 1
    ),
    selectInput(
      inputId = "hist_fill",
      label = "Histogram Color:",
      choices = c("grey35", "black", "grey20", "grey50"),
      selected = "grey35",
      multiple = FALSE
    )
  ),
  shiny::tabPanelBody(
    "Boxplot",
    shiny::selectInput(
      inputId = "box_by",
      label = "Boxplot: By Variable",
      choices = c(""), 
      selected = NULL, multiple = FALSE
    )
  )
)