# Module UI
tenderMarketAnalysisUI <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(4,
        selectInput(ns("status_filter"),
          "Tender Detail Status:",
          choices = c(
            "Award by interface record",
            "Awarded by items", 
            "Awarded to Suppliers"
          )
        )
      ),
      column(8,
        plotlyOutput(ns("trend_plot"))
      )
    ),
    fluidRow(
      column(12,
        # Add table to show detailed data
        DT::dataTableOutput(ns("detail_table"))
      )
    )
  )
}

# Module Server
tenderMarketAnalysis <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    
    # Filter data based on selected status
    filtered_data <- reactive({
      data %>%
        filter(tender_detail_status == input$status_filter) %>%
        group_by(year, lda_category) %>%
        summarise(
          total_amount = sum(awarded_amt, na.rm = TRUE),
          avg_amount = mean(awarded_amt, na.rm = TRUE),
          count = n()
        )
    })
    
    # Create trend plot
    output$trend_plot <- renderPlotly({
      p <- ggplot(filtered_data(), 
        aes(x = year, y = total_amount, color = lda_category)) +
        geom_line() +
        geom_point() +
        labs(
          title = paste("Tender Amount Trends by Category -", input$status_filter),
          x = "Year",
          y = "Total Awarded Amount",
          color = "Tender Category"
        ) +
        theme_minimal()
      
      ggplotly(p)
    })
    
    # Create detailed table
    output$detail_table <- DT::renderDataTable({
      filtered_data() %>%
        arrange(desc(year), desc(total_amount)) %>%
        mutate(
          total_amount = scales::dollar(total_amount),
          avg_amount = scales::dollar(avg_amount)
        )
    })
  })
} 