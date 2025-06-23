#' @title Run the GoFiguR app
#' @export

runGoFiguR <- function() {
  appDir <- system.file("myapp", package = "gofigur")
  
  if (appDir == "") {
    stop("Could not find app. Try re-installing `gofigur`.", call. = FALSE)
  }
  
  shiny::runApp(appDir)
}