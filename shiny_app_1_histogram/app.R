source("BTRY 4100/FRPM Project/frmp_analysis.R")



# SHINY APP 1
ui <- fluidPage(
    titlePanel(" Interactive Histogram displaying distribution of Select Variables"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("county", "Filter by County:",
                        choices = c("All", sort(unique(df_clean$`County Name`))),
                        selected = "All"),
            
            selectInput("school_type", "Filter by School Type:",
                        choices = c("All", sort(unique(df_clean$`School Type`))),
                        selected = "All"),
            
            selectInput("metric", "Select Variable for Histogram:",
                        choices = c(
                            "Enrollment (K-12)",
                            "Enrollment (Ages 5-17)",
                            "Free Meal Count (K-12)",
                            "Free Meal Count (Ages 5-17)",
                            "FRPM Count (K-12)",
                            "FRPM Count (Ages 5-17)",
                            "Percent (%) Eligible Free (K-12)",
                            "Percent (%) Eligible FRPM (K-12)",
                            "Percent (%) Eligible Free (Ages 5-17)",
                            "Percent (%) Eligible FRPM (Ages 5-17)"
                        ),
                        selected = "Percent (%) Eligible FRPM (K-12)")
        ),
        
        mainPanel(
            plotOutput("hist_plot")
        )
    )
)

# SERVER 
server <- function(input, output, session) {
    filtered_data <- reactive({
        data <- df_clean
        if (input$county != "All") {
            data <- data[data$`County Name` == input$county, ]
        }
        if (input$school_type != "All") {
            data <- data[data$`School Type` == input$school_type, ]
        }
        data <- data %>%
            mutate(temp_metric = as.numeric(.data[[input$metric]])) %>%
            filter(!is.na(temp_metric))
        return(data)
    })
    
    output$hist_plot <- renderPlot({
        data <- filtered_data()
        ggplot(data, aes(x = temp_metric)) +
            geom_histogram(bins = 30, fill = "#1f77b4", color = "black") +
            labs(
                x = input$metric,
                y = "Number of Schools",
                title = paste("Distribution of", input$metric)
            ) +
            theme_minimal()
    })
}

# RUN 
shinyApp(ui, server)