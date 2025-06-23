#' @title Run the GoFiguR app
#' @export

runGoFiguR <- function() {
  appDir <- system.file("app", package = "gofigur")
  
  if (appDir == "") {
    stop("Could not find app. Try re-installing `gofigur`.", call. = FALSE)
  }
  
  shiny::runApp(appDir)
}