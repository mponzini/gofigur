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
select.x.var.server <- function(id, data_class, filter = c("character", "numeric")) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data_class(), {
      updateSelectInput(
        session, "x.var", 
        choices = data_class() |> 
          dplyr::filter(Class %in% filter) |> 
          dplyr::pull(Variable)
      )
    })
    
    reactive(input$x.var)
    
  })
}

#' @export
select.y.var.server <- function(id, data_class, filter = c("character", "numeric")) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data_class(), {
      updateSelectInput(
        session, "y.var", 
        choices = data_class() |> 
          dplyr::filter(Class %in% filter) |> 
          dplyr::pull(Variable)
      )
    })
    
    reactive(input$y.var)
    
  })
}

#' @export
select.by.var.server <- function(id, data_class, filter = c("character", "numeric")) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data_class(), {
      updateSelectInput(
        session, "by.var", choices = 
          c("No group", 
            data_class() |> 
              dplyr::filter(Class %in% filter) |> 
              dplyr::pull(Variable)
          )
      )
    })
    
    reactive(input$by.var)
    
  })
}

#' @export
select.time.var.server <- function(id, data_class, filter = c("character", "numeric")) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data_class(), {
      updateSelectInput(
        session, "time.var", 
        choices = data_class() |> 
          dplyr::filter(Class %in% filter) |> 
          dplyr::pull(Variable)
      )
    })
    
    reactive(input$time.var)
    
  })
}

#' @export
select.event.var.server <- function(id, data_class, filter = c("character", "numeric")) {
  moduleServer(id, function(input, output, session) {
    observeEvent(data_class(), {
      updateSelectInput(
        session, "event.var", 
        choices = data_class() |> 
          dplyr::filter(Class %in% filter) |> 
          dplyr::pull(Variable)
      )
    })
    
    reactive(input$event.var)
    
  })
}
