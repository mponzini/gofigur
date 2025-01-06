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
