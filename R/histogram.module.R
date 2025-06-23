histogramUI <- function(id) {
  tabPanel(
    "Histogram",
    sidebarLayout(
      sidebarPanel(
        select_x.var.input(NS(id, "x.var")),
        numericInput(
          inputId = NS(id, "numb_bins"),
          label = "Number of Bins:",
          value = 20,
          min = 1
        ),
        selectInput(
          inputId = NS(id, "hist_fill"),
          label = "Histogram Color:",
          choices = c("grey35", "black", "grey20", "grey50"),
          selected = "grey35", 
          multiple = FALSE
        ),
        labels_and_fonts("hist", by = FALSE)
      ),
      mainPanel(
        plotOutput(NS(id, "hist")),
        downloadUI("hist")
      )
    )
  )
}

histogramServer <- function(id, data, data_class) {
  shiny::moduleServer(id, function(input, output, session) {
    # select x var
    x_var <- select_x.var.server("x.var", data_class, "numeric")
    # Update axis labels
    x_label <- reactive({
      ifelse(input$x_lab == "", paste(input$x_var), input$x_lab)
    })
    y_label <- reactive({
      ifelse(input$y_lab == "", "count", input$y_lab)
    })

    # create tmp data for plot
    plot_data <- reactive({
      shiny::req(data())
      shiny::req(all(class(data()) != "gg"))
      
      data()
    })
    
    # plot
    plot <- reactive({
      plot_data() |> 
        ggplot2::ggplot() +
        ggplot2::aes(
          x = .data[[x_var()]]
        ) +
        ggplot2::geom_histogram(
          fill = input$hist_fill,
          bins = input$numb_bins
        ) +
        ggplot2::labs(
          x = x_label(),
          y = y_label()
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(
          # x-axis
          axis.title.x = ggtext::element_markdown(
            size = input$x_lab_size
          ),
          axis.text.x = ggtext::element_markdown(
            size = input$x_text_size
          ),
          # y-axis
          axis.title.y = ggtext::element_markdown(
            size = input$y_lab_size
          ),
          axis.text.y = ggtext::element_markdown(
            size = input$y_text_size
          )
        )
    })
    
    output$hist <- renderPlot({plot()})
    
    # download handler
    opts <- reactive({
      if(input$plot_device == "rds") {
        list(
          textInput(
            inputId = NS(id, "plot_name"),
            label = "Figure File Name:",
            value = "figure-name"
          )
        )
      } else {
        list(
          textInput(
            inputId = NS(id, "plot_name"),
            label = "Figure File Name:",
            value = "figure-name"
          ),
          numericInput(
            inputId = NS(id, "plot_height"),
            label = "Figure Height:",
            value = 6,
            min = 1
          ),
          numericInput(
            inputId = NS(id, "plot_width"),
            label = "Figure Width:",
            value = 6,
            min = 1
          ),
          numericInput(
            inputId = NS(id, "plot_dpi"),
            label = "Figure Dots per Inch (DPI):",
            value = 300,
            min = 1,
            max = 600
          )
        )
      }
    })
    
    output$downloadOpts <- renderUI(opts())
    
    output$downloadPlot <- downloadHandler(
      filename = function(file) {
        paste(input$plot_name, input$plot_device, sep = ".")
      },
      content = function(file) {
        if (input$plot_device == "rds") {
          saveRDS(object = plot(), file)
        } else {
          ggplot2::ggsave(
            file,
            ,
            plot = plot(),
            width = input$plot_width,
            height = input$plot_height,
            dpi = input$plot_dpi,
            device = input$plot_device
          )
        }
      }
    )
  })
}

