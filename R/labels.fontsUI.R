labels_and_fonts <- function(id, by = TRUE) {
  if (!is.logical(by)) {
    stop("'by' must be logical (TRUE/FALSE)")
  }
  
  if (by) {
    htmltools::tagList(
      textInput(
        inputId = NS(id, "x_lab"),
        label = "X-axis Label"
      ),
      textInput(
        inputId = NS(id, "y_lab"),
        label = "Y-axis Label"
      ),
      textInput(
        inputId = NS(id, "by_lab"),
        label = "Legend Label"
      ),
      numericInput(
        inputId = NS(id, "x_lab_size"),
        label = "X-axis Label Font Size",
        value = 14,
        min = 1,
        step = 0.5
      ),
      numericInput(
        inputId = NS(id, "x_text_size"),
        label = "X-axis Text Font Size",
        value = 14,
        min = 1,
        step = 0.5
      ),
      numericInput(
        inputId = NS(id, "y_lab_size"),
        label = "Y-axis Label Font Size",
        value = 14,
        min = 1,
        step = 0.5
      ),
      numericInput(
        inputId = NS(id, "y_text_size"),
        label = "Y-axis Text Font Size",
        value = 14,
        min = 1,
        step = 0.5
      ),
      numericInput(
        inputId = NS(id, "by_lab_size"),
        label = "Legend Label Font Size",
        value = 14,
        min = 1,
        step = 0.5
      ),
      numericInput(
        inputId = NS(id, "by_text_size"),
        label = "Legend Text Font Size",
        value = 14,
        min = 1,
        step = 0.5
      )
    )
  } else {
    htmltools::tagList(
      textInput(
        inputId = NS(id, "x_lab"),
        label = "X-axis Label"
      ),
      textInput(
        inputId = NS(id, "y_lab"),
        label = "Y-axis Label"
      ),
      numericInput(
        inputId = NS(id, "x_lab_size"),
        label = "X-axis Label Font Size",
        value = 14,
        min = 1,
        step = 0.5
      ),
      numericInput(
        inputId = NS(id, "x_text_size"),
        label = "X-axis Text Font Size",
        value = 14,
        min = 1,
        step = 0.5
      ),
      numericInput(
        inputId = NS(id, "y_lab_size"),
        label = "Y-axis Label Font Size",
        value = 14,
        min = 1,
        step = 0.5
      ),
      numericInput(
        inputId = NS(id, "y_text_size"),
        label = "Y-axis Text Font Size",
        value = 14,
        min = 1,
        step = 0.5
      )
    )
  }
  
}
