## select input modules
# ui

#' @export
select.x.var.input <- function(id) {
  selectInput(NS(id, "x.var"), "X Variable", choices = NULL)
}

#' @export
select.y.var.input <- function(id) {
  selectInput(NS(id, "y.var"), "Y Variable", choices = NULL)
}

#' @export
select.by.var.input <- function(id) {
  selectInput(NS(id, "by.var"), "By Variable", choices = NULL)
}
# server
#' @export
select.x.var.server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      updateSelectInput(session, "x.var", choices = colnames(data()))
    })
    
    reactive(input$x.var)
    
  })
}

#' @export
select.y.var.server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      updateSelectInput(session, "y.var", choices = colnames(data()))
    })
    
    reactive(input$y.var)
    
  })
}

#' @export
select.by.var.server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      updateSelectInput(session, "by.var", choices = c("No group", colnames(data())))
    })
    
    reactive(input$by.var)
    
  })
}
