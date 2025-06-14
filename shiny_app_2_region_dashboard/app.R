# Shiny App 2

# DEFINE REGIONS 
bay_area <- c("Alameda", "Contra Costa", "Marin", "Napa", "San Francisco", "San Mateo", "Santa Clara", "Solano", "Sonoma")
northern <- c("Del Norte", "Humboldt", "Lassen", "Modoc", "Shasta", "Siskiyou", "Tehama", "Trinity", "Butte", "Colusa", "Glenn", "Lake", "Mendocino", "Nevada", "Plumas", "Sierra")
central  <- c("Fresno", "Kings", "Madera", "Merced", "San Joaquin", "Stanislaus", "Tulare", "Tuolumne", "Mariposa", "Mono", "Inyo")
southern <- c("Los Angeles", "San Bernardino", "San Diego", "Orange", "Riverside", "Imperial", "Ventura", "Kern", "Santa Barbara", "San Luis Obispo")

df_clean <- df_clean %>%
    mutate(
        Region = case_when(
            `County Name` %in% bay_area ~ "Bay Area",
            `County Name` %in% northern ~ "Northern CA",
            `County Name` %in% central ~ "Central CA",
            `County Name` %in% southern ~ "Southern CA",
            TRUE ~ "Other"
        ),
        free_pct = `Percent (%) Eligible Free (K-12)`,
        frpm_pct = `Percent (%) Eligible FRPM (K-12)`,
        enrollment = `Enrollment (K-12)`
    )

# UI 
ui <- fluidPage(
    titlePanel("K-12 Eligibility Distribution by Region"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("region", "Select Region:",
                        choices = unique(df_clean$Region),
                        selected = "Bay Area"),
            
            uiOutput("county_ui"),
            uiOutput("district_ui"),
            
            radioButtons("metric", "Choose Metric:",
                         choices = c("Free Eligibility" = "free_pct",
                                     "FRPM Eligibility" = "frpm_pct"),
                         selected = "free_pct")
        ),
        
        mainPanel(
            plotlyOutput("main_plot", height = "600px"),
            verbatimTextOutput("enrollment_summary")
        )
    )
)

# SERVER 
server <- function(input, output, session) {
    
    # Update County dropdown
    output$county_ui <- renderUI({
        counties <- df_clean %>%
            filter(Region == input$region) %>%
            distinct(`County Name`) %>%
            arrange(`County Name`) %>%
            pull()
        
        selectInput("county", "Select County:",
                    choices = c("All", counties), selected = "All")
    })
    
    # Update District dropdown
    output$district_ui <- renderUI({
        if (is.null(input$county) || input$county == "All") return(NULL)
        
        districts <- df_clean %>%
            filter(Region == input$region, `County Name` == input$county) %>%
            distinct(`District Name`) %>%
            arrange(`District Name`) %>%
            pull()
        
        selectInput("district", "Select District (optional):",
                    choices = c("All", districts), selected = "All")
    })
    
    # Filtered data
    filtered <- reactive({
        data <- df_clean %>% filter(Region == input$region)
        
        if (input$county != "All") {
            data <- data %>% filter(`County Name` == input$county)
        }
        
        if (!is.null(input$district) && input$district != "All") {
            data <- data %>% filter(`District Name` == input$district)
        }
        
        data %>% filter(!is.na(.data[[input$metric]]))
    })
    
    # Histogram plot
    output$main_plot <- renderPlotly({
        data <- filtered()
        
        plot_ly(data, x = ~.data[[input$metric]], type = "histogram",
                marker = list(color = "darkcyan")) %>%
            layout(
                title = paste("Histogram of", input$metric, "in", input$region),
                xaxis = list(title = "Eligibility (%)"),
                yaxis = list(title = "Number of Schools")
            )
    })
    
    # Enrollment summary
    output$enrollment_summary <- renderPrint({
        data <- filtered()
        total <- sum(data$enrollment, na.rm = TRUE)
        cat("Total Enrollment (K–12):", format(total, big.mark = ","))
    })
}

# RUN APP 
shinyApp(ui, server)
