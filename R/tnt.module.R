tntUI <- function(id) {
  tabPanel(
    "Barplot",
    sidebarLayout(
      sidebarPanel(
        select_x.var.input(NS(id, "x.var")),
        select_y.var.input(NS(id, "y.var")),
        select_by.var.input(NS(id, "by.var")),
        shiny::selectInput(
          inputId = NS(id, "var"),
          label = "Measure of Variability",
          choices = c("SD", "SE", "CI"),
          selected = NULL
        ),
        labels_and_fonts("tnt")
      ),
      mainPanel(
        plotOutput(NS(id, "tnt")),
        downloadUI("tnt")
      )
    )
  )
}

tntServer <- function(id, data, data_class) {
  shiny::moduleServer(id, function(input, output, session) {
    # select x and by variables
    x_var <- select_x.var.server("x.var", data_class)
    y_var <- select_y.var.server("y.var", data_class, "numeric")
    by_var <- select_by.var.server("by.var", data_class)
    # Update axis labels
    x_label <- reactive({
      ifelse(input$x_lab == "", paste(x_var()), input$x_lab)
    })
    y_label <- reactive({
      ifelse(input$y_lab == "", paste(y_var()), input$y_lab)
    })
    by_label <- reactive({
      ifelse(input$by_lab == "", paste(by_var()), input$by_lab)
    })
    
    # create tmp data for plot
    summary_data <- reactive({
      shiny::req(data())
      shiny::req(nrow(data()) > 0)
      shiny::req(all(class(data()) != "gg"))
      
      if (by_var() != "No group") {
        data() |> 
          dplyr::summarise(
            N = sum(!is.na(.data[[y_var()]])),
            Mean = mean(.data[[y_var()]], na.rm = TRUE),
            SD = sd(.data[[y_var()]], na.rm = TRUE),
            SE = SD / sqrt(N),
            lower.ci = Mean - (qt(0.975, N - 1) * SE),
            upper.ci = Mean + (qt(0.975, N - 1) * SE),
            .by = dplyr::all_of(c(x_var(), by_var()))
          ) |> 
          dplyr::mutate(
            dplyr::across(
              .cols = dplyr::any_of(c(x_var(), by_var())),
              ~ .x |> factor()
            )
          )
      } else {
        data() |> 
          dplyr::summarise(
            N = sum(!is.na(.data[[y_var()]])),
            Mean = mean(.data[[y_var()]], na.rm = TRUE),
            SD = sd(.data[[y_var()]], na.rm = TRUE),
            SE = SD / sqrt(N),
            lower.ci = Mean - (qt(0.975, N - 1) * SE),
            upper.ci = Mean + (qt(0.975, N - 1) * SE),
            .by = dplyr::all_of(x_var())
          ) |> 
          dplyr::mutate(
            dplyr::across(
              .cols = dplyr::any_of(c(x_var(), by_var())),
              ~ .x |> factor()
            )
          )
      }
    })
    
    plot_data <- reactive({
      shiny::req(summary_data())
      
      if (input$var == "SD") {
        summary_data() |> 
          dplyr::mutate(
            y_min = Mean - SD,
            y_max = Mean + SD
          )
      } else if (input$var == "SE") {
        summary_data() |> 
          dplyr::mutate(
            y_min = Mean - SE,
            y_max = Mean + SE
          )
      } else if (input$var == "CI") {
        summary_data() |> 
          dplyr::mutate(
            y_min = lower.ci,
            y_max = upper.ci
          )
      }
    })
    
    
    # conditional aes
    gg_aes <- reactive({
      if (by_var() != "No group") {
        ggplot2::aes(
          x = .data[[x_var()]],
          y = Mean,
          ymin = y_min,
          ymax = y_max,
          fill = .data[[by_var()]]
        )
      } else {
        ggplot2::aes(
          x = .data[[x_var()]],
          y = Mean,
          ymin = y_min,
          ymax = y_max
        )
      }
    })
    
    
    # plot
    plot <- reactive({
      plot_data() |> 
        ggplot2::ggplot() +
        gg_aes() +
        ggplot2::geom_col(
          position = ggplot2::position_dodge(1)
        ) +
        ggplot2::geom_errorbar(
          width = 0.25, 
          position = ggplot2::position_dodge(1)
        ) +
        ggplot2::labs(
          x = x_label(),
          y = y_label(),
          fill = by_label()
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
          ),
          # legend
          legend.title = ggtext::element_markdown(
            size = input$by_lab_size
          ),
          legend.text = ggtext::element_markdown(
            size = input$by_text_size
          )
        )
    })
    
    output$tnt <- renderPlot({plot()})
    
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
