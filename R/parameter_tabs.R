parameter_tabs <- shiny::tabsetPanel(
  id = "params",
  type = "hidden",
  shiny::tabPanelBody(
    " "
  ),
  shiny::tabPanelBody(
    "Histogram",
    selectInput(
      inputId = "x_var_hist",
      label = "X Variable:",
      choices = c(""), selected = NULL, multiple = FALSE
    ),
    shiny::numericInput(
      inputId = "numb_bins",
      label = "Number of Bins:",
      value = 20,
      min = 1
    ),
    shiny::selectInput(
      inputId = "hist_fill",
      label = "Histogram Color:",
      choices = c("grey35", "black", "grey20", "grey50"),
      selected = "grey35",
      multiple = FALSE
    )
  ),
  shiny::tabPanelBody(
    "Boxplot",
    selectInput(
      inputId = "x_var_box",
      label = "X Variable:",
      choices = c(""), selected = NULL, multiple = FALSE
    ),
    selectInput(
      inputId = "y_var_box",
      label = "Y Variable:",
      choices = c(""), selected = NULL, multiple = FALSE
    ),
    shiny::selectInput(
      inputId = "by_box",
      label = "Boxplot: By Variable",
      choices = c(""), 
      selected = NULL, multiple = FALSE
    )
  )
)