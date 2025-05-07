kmUI <- function(id) {
  tabPanel(
    "Kaplan-Meier Curve",
    sidebarLayout(
      sidebarPanel(
        select.time.var.input(NS(id, "time.var")),
        select.event.var.input(NS(id, "event.var")),
        select.by.var.input(NS(id, "by.var")),
        checkboxInput(
          NS(id, "p.val"),
          label = "P-value",
          value = FALSE
        ),
        checkboxInput(
          NS(id, "conf.int"),
          label = "Confidence Interval",
          value = FALSE
        ),
        selectInput(
          NS(id, "median.lines"),
          label = "Median Survival Lines",
          choices = c("None", "Horizontal", "Vertical", "Horizontal & Vertical"),
          selected = "None"
        ),
        textInput(
          NS(id, "by.levels"),
          label = "By variable labels (separate by '|')",
          value = ""
        ),
        labels_and_fonts("km"),
      ),
      mainPanel(
        plotOutput(NS(id, "km")),
        downloadUI("km")
      )
    )
  )
}

kmServer <- function(id, data, data_class) {
  shiny::moduleServer(id, function(input, output, session) {
    # select x and by variables
    time_var <- select.time.var.server("time.var", data)
    event_var <- select.event.var.server("event.var", data)
    by_var <- select.by.var.server("by.var", data)
    # Update axis labels
    x_label <- reactive({
      ifelse(input$x_lab == "", paste(time_var()), input$x_lab)
    })
    y_label <- reactive({
      ifelse(input$y_lab == "", "Survival", input$y_lab)
    })
    by_label <- reactive({
      ifelse(input$by_lab == "", paste(by_var()), input$by_lab)
    })

    median_lines <-reactive({
      if (input$median.lines == "None") {
        "none"
      } else if (input$median.lines == "Horizontal") {
        "h"
      } else if (input$median.lines == "Vertical") {
        "v"
      } else if (input$median.lines == "Horizontal & Vertical") {
        "hv"
      }
    })
    
    # create tmp data for plot
    plot_data <- reactive({
      shiny::req(data())
      shiny::req(all(class(data()) != "gg"))
      
      data() |> 
        dplyr::mutate(
          dplyr::across(
            .cols = dplyr::any_of(c(by_var())),
            ~ .x |> as.factor()
          )
        )
    })
    
    
    by_levels <- reactive({
      if (input$by.levels == "") {
        levels(plot_data()[[by_var()]])
      } else {
        stringr::str_split(input$by.levels, pattern = "\\|") |> unlist()
      }
    })
        
    
    km_fit <- reactive({
      req(plot_data())
      
      survival::survfit(
        survival::Surv(plot_data()[[time_var()]], plot_data()[[event_var()]]) ~ 
          plot_data()[[by_var()]]
      )
    })
    
    tmp_p <- reactive({
      if (input$p.val) {
        paste0(
          "p = ",
          survival::survdiff(
            survival::Surv(plot_data()[[time_var()]], plot_data()[[event_var()]]) ~
              plot_data()[[by_var()]]
          )$pvalue |> round(3) |> format(nsmall = 3)
        )
      } else {
        ""
      }
    })

    p_value <- reactive({
      if( tmp_p() == "p = 0.000") {"p < 0.001"} else { tmp_p()}
    })
    
    # plot
    p <- reactive({
      survminer::ggsurvplot(
        fit = km_fit(),
        data = plot_data(),
        # pval = input$p.val,
        pval = p_value(),
        conf.int = input$conf.int,
        surv.median.line = median_lines(),
        # labels
        xlab = x_label(),
        ylab = y_label(),
        legend.labs = by_levels(),
        legend.title = by_label(),
        # fonts
        font.x = input$x_lab_size,
        font.y = input$y_lab_size,
        font.legend = input$by_lab_size
      )
      # $plot +
      #   ggplot2::labs(
      #     x = x_label(),
      #     y = y_label()
      #   )
      #   ggplot2::theme(
      #     # x-axis
      #     axis.title.x = ggtext::element_markdown(
      #       size = input$x_lab_size
      #     ),
      #     axis.text.x = ggtext::element_markdown(
      #       size = input$x_text_size
      #     ),
      #     # y-axis
      #     axis.title.y = ggtext::element_markdown(
      #       size = input$y_lab_size
      #     ),
      #     axis.text.y = ggtext::element_markdown(
      #       size = input$y_text_size
      #     ),
      #     # legend
      #     legend.title = ggtext::element_markdown(
      #       size = input$by_lab_size
      #     ),
      #     legend.text = ggtext::element_markdown(
      #       size = input$by_text_size
      #     )
      #   )
    })
    
    output$km <- renderPlot({p()})
    
    # download handler
    output$downloadPlot <- downloadHandler(
      filename = function(file) {
        paste(input$plot_name, input$plot_device, sep = ".")
      },
      content = function(file) {
        ggplot2::ggsave(
          file,
          ,
          plot = p()$plot,
          width = input$plot_width,
          height = input$plot_height,
          dpi = input$plot_dpi,
          device = input$plot_device
        )
      }
    )
  })
}