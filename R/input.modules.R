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

#' @export
select.time.var.input <- function(id) {
  selectInput(NS(id, "time.var"), "Time Variable", choices = NULL)
}

#' @export
select.event.var.input <- function(id) {
  selectInput(NS(id, "event.var"), "Event Variable", choices = NULL)
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

#' @export
select.time.var.server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      updateSelectInput(session, "time.var", choices = colnames(data()))
    })
    
    reactive(input$time.var)
    
  })
}

#' @export
select.event.var.server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data(), {
      updateSelectInput(session, "event.var", choices = colnames(data()))
    })
    
    reactive(input$event.var)
    
  })
}
