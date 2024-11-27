#' Plot Component Functions
#' 

#### Common UI for all graph types ####
commonUI <- function(){
  htmltools::tagList(
    textInput(
      inputId = "x_lab",
      label = "X Axis Label:",
      value = NULL
      
    ),
    textInput(
      inputId = "y_lab",
      label = "Y Axis Label:",
      value = NULL
      
    ),
    numericInput(
      inputId = "x_size",
      label = "X Axis Text Size:",
      value = 7,
      min = 1
    ),
    numericInput(
      inputId = "y_size",
      label = "Y Axis Text Size:",
      value = 7,
      min = 1
    ),
    numericInput(
      inputId = "x_title_size",
      label = "X Axis Title Text Size:",
      value = 10,
      min = 1
    ),
    numericInput(
      inputId = "y_title_size",
      label = "Y Axis Title Text Size:",
      value = 10,
      min = 1
    )
  )
}

#### Histogram UI ####
histogramUI <- function(){
  htmltools::tagList(
    selectInput(
      inputId = "x_var",
      label = "X Variable:",
      choices = c(""), selected = NULL, multiple = FALSE
    ),
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
    ),
    commonUI()
  )
}


#### Boxplot UI ####
boxplotUI <- function(){
  htmltools::tagList(
    selectInput(
      inputId = "x_var",
      label = "X Variable:",
      choices = c(""), selected = NULL, multiple = FALSE
    ),
    selectInput(
      inputId = "y_var",
      label = "Y Variable:",
      choices = c(""), selected = NULL, multiple = FALSE
    ),
    commonUI()
  )
}
#### Scatterplot UI ####
scatterUI <- function(){
  htmltools::tagList(
    selectInput(
      inputId = "x_var",
      label = "X Variable:",
      choices = c(""), selected = NULL, multiple = FALSE
    ),
    selectInput(
      inputId = "y_var",
      label = "Y Variable:",
      choices = c(""), selected = NULL, multiple = FALSE
    ),
    commonUI()
  )
}
