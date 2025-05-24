histogramOutput <- function(id) {
  htmltools::tagList(
    shiny::plotOutput(NS(id, "hist"))
  )
}

scatterOutput <- function(id) {
  htmltools::tagList(
    shiny::plotOutput(NS(id, "scatter"))
  )
}

boxOutput <- function(id) {
  htmltools::tagList(
    shiny::plotOutput(NS(id, "box"))
  )
}

barOutput <- function(id) {
  htmltools::tagList(
    shiny::plotOutput(NS(id, "bar"))
  )
}

providedOutput <- function(id) {
  htmltools::tagList(
    shiny::plotOutput(NS(id, "provided"))
  )
}