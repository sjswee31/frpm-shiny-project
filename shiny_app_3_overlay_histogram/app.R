source("../frmp_analysis.R")

# Shiny App 3
# UI 
ui <- fluidPage(
    titlePanel("Overlayed Histogram: % Free vs FRPM Eligibility"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("school_type", "Select School Type:",
                        choices = c("All", sort(unique(df_clean$`School Type`))),
                        selected = "All")
        ),
        
        mainPanel(
            plotOutput("hist_overlay"),
            verbatimTextOutput("enrollment_total")
        )
    )
)

# SERVER 
server <- function(input, output, session) {
    
    filtered_data <- reactive({
        data <- df_clean
        if (input$school_type != "All") {
            data <- data[data$`School Type` == input$school_type, ]
        }
        data <- data %>%
            filter(!is.na(`Percent (%) Eligible Free (K-12)`),
                   !is.na(`Percent (%) Eligible FRPM (K-12)`))
        return(data)
    })
    
    output$hist_overlay <- renderPlot({
        data <- filtered_data()
        
        ggplot(data) +
            geom_histogram(aes(x = `Percent (%) Eligible Free (K-12)`, fill = "Free Eligibility"), 
                           bins = 30, alpha = 0.5, position = "identity", color = "black") +
            geom_histogram(aes(x = `Percent (%) Eligible FRPM (K-12)`, fill = "FRPM Eligibility"), 
                           bins = 30, alpha = 0.5, position = "identity", color = "black") +
            scale_fill_manual(values = c("Free Eligibility" = "skyblue", "FRPM Eligibility" = "orange")) +
            labs(title = "Overlayed Distribution of Free vs FRPM Eligibility",
                 x = "% Eligible",
                 y = "Number of Schools",
                 fill = "Metric") +
            theme_minimal()
    })
    
    output$enrollment_total <- renderPrint({
        data <- filtered_data()
        total <- sum(data$`Enrollment (K-12)`, na.rm = TRUE)
        cat("Total Enrollment (K-12):", format(total, big.mark = ","), "\n")
    })
}

# RUN APP 
shinyApp(ui, server)

