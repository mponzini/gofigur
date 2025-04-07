#' @title Data Module UI
#' @export

dataUI <- function(id) {
  htmltools::tagList(
    mainPanel(
      fluidRow(
        column(
          11,
          p(textOutput(NS(id, "dataInfo")))
        )
      ),
      fluidRow(
        column(
          11,
          DT::dataTableOutput("info")
        )
      )
    )
  )
}

#' @title Data Module Server
#' @export

dataServer <- function(id, data) {
  shiny::moduleServer(id, function(input, output, session) {
    # Display message to import data file or summary of data file (nrow, ncol)
    data_info <- reactive({
      if ((!is.null(data()))){
        
        if (any(class(data()) == "gg")) {
          test <- "The data is a {ggplot2} figure object."
        } else {
          text <- paste0("The data has ", nrow(data()), " rows and ", ncol(data()),
                       " columns. The variable types are displayed below.",
                       " Please review each variable and check that its class",
                       " (numeric or character) is as expected.")
        }
        
      } else {
        
        text <- paste0("Import your data file.
                     Accepted file types are xls, xlsx",
                     ", csv, R (rds), SAS (sas7bdat), Stata (dta), or SPSS (sav).")
      }
      text
    })
    
    output$dataInfo <- renderText(data_info())
    
    # get class of each variable
    reactive({
      if ((!is.null(data()))) {
        data.frame(
          "Variable" = colnames(data()),
          "Class" = sapply(1:ncol(data()), function(x){class(data()[[x]])})
        )
      }
    })
  })
}
