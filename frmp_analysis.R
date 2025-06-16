# install.packages("readxl")
# install.packages("dplyr")
# install.packages("broom")
# install.packages("sjPlot")
# install.packages("sjmisc")  # required for tab_corr()
# install.packages("tidyverse")
# install.packages(c("shiny", "ggplot2", "dplyr"))
library(readxl)
library(dplyr)
library(broom)
library(sjPlot)
library(sjmisc)
library(tidyverse)
library(shiny)
library(ggplot2)
library(plotly)

# Data Cleaning / EDA
df_clean <- read_excel("../frmp23-24.xlsx", sheet = 2, col_names = FALSE)
colnames(df_clean) <- as.character(df_clean[2, ])
df_clean <- df_clean[-c(1, 2), ]
names(df_clean) <- gsub("[\r\n]+", " ", names(df_clean)) 
names(df_clean) <- gsub(" +", " ", names(df_clean))
names(df_clean) <- trimws(names(df_clean))

# Convert relevant variables to numeric
to_numeric <- function(x) as.numeric(gsub("%", "", x))
df_clean <- df_clean %>%
    mutate(
        `Enrollment (K-12)` = as.numeric(`Enrollment (K-12)`),
        `Enrollment (Ages 5-17)` = as.numeric(`Enrollment (Ages 5-17)`),
        `Free Meal Count (K-12)` = as.numeric(`Free Meal Count (K-12)`),
        `FRPM Count (K-12)` = as.numeric(`FRPM Count (K-12)`),
        `Free Meal Count (Ages 5-17)` = as.numeric(`Free Meal Count (Ages 5-17)`),
        `FRPM Count (Ages 5-17)` = as.numeric(`FRPM Count (Ages 5-17)`),
        `Percent (%) Eligible Free (K-12)` = to_numeric(`Percent (%) Eligible Free (K-12)`),
        `Percent (%) Eligible FRPM (K-12)` = to_numeric(`Percent (%) Eligible FRPM (K-12)`),
        `Percent (%) Eligible Free (Ages 5-17)` = to_numeric(`Percent (%) Eligible Free (Ages 5-17)`),
        `Percent (%) Eligible FRPM (Ages 5-17)` = to_numeric(`Percent (%) Eligible FRPM (Ages 5-17)`)
    )

# Convert Charter School (Y/N) to numeric
df_clean$`Charter School (Y/N)` <- case_when(
    df_clean$`Charter School (Y/N)` == "Yes" ~ 1,
    df_clean$`Charter School (Y/N)` == "No" ~ 0,
)

# Filtering to clean up unuseful data points
df_clean <- df_clean %>%
    filter(`School Name` != "District Office") %>%
    filter(`District Type` != "State Special Schools") %>%
    filter(!is.na(`Enrollment (Ages 5-17)`) & `Enrollment (Ages 5-17)` != 0) %>%
    filter(!is.na(`Enrollment (K-12)`) & `Enrollment (K-12)` != 0) %>%
    select(-`Academic Year`, -`County Code`, -`School Code`, -`District Code`, -`CALPADS Fall 1 Certification Status`)

# basic counts of distinct values
num_districts <- n_distinct(df_clean$`District Name`)
num_counties <- n_distinct(df_clean$`County Name`)
num_schools <- n_distinct(df_clean$`School Name`)
num_school_types <- n_distinct(df_clean$`School Type`)
num_edu_option_types <- n_distinct(df_clean$`Educational Option Type`)
num_districts
num_counties
num_schools
num_school_types
num_edu_option_types

# MODELING

# Sample Linear Models

# Q1: Do charter schools have different rates of FRPM eligibility?
model1 <- lm(`Percent (%) Eligible FRPM (K-12)` ~ `Charter School (Y/N)`, data = df_clean)
cat("\n Q1: Charter status vs. FRPM eligibility \n")
summary(model1)
# p-value of 0.2051
# p-value not statistically significant and therefore
# cannot conclude that FRPM eligiblity differs between charter and non-charter schools
# charter status alone is not a strong predictor of FRPM eligibility in dataset

# Q2: Are school types associated with differeces in FRPM eligibility?
model2 <- lm(`Percent (%) Eligible FRPM (K-12)` ~ `School Type`, data = df_clean)
cat("\n Q2: School type vs. FRPM eligibility \n")
summary(model2)
# p-value of 2.2e-16
# extremely significant, indicating school type is strongly associated with FRPM.
# School structure contributes to poverty rates and systemic economic disadvantages

# Q3: Do charter schools tend to be smaller or larger than non-charter schools?
model3 <- lm(`Enrollment (Ages 5-17)` ~ `Charter School (Y/N)`, data = df_clean)
cat("\n Q3: Charter status vs. Enrollment \n")
summary(model3)
# p-value of 0.003087
# Charter schools tend to be smaller in school size

# Q4: What best explains percent FRPM eligibility (multivariable)?
model4 <- lm(`Percent (%) Eligible FRPM (K-12)` ~ `Charter School (Y/N)` + `School Type` + `Educational Option Type`, data = df_clean)
cat("\n Q4: Multi-factor explanation of FRPM eligibility \n")
summary(model4)
# p-value of 2.2e-16
# charter status, school type, and educational option type significantly explain variation in FRPM eligibility

# Q5: Are some counties more disadvantaged than others?
model5 <- lm(`Percent (%) Eligible FRPM (K-12)` ~ `County Name`, data = df_clean)
cat("\n Q5: County vs. FRPM eligibility \n")
summary(model5)
# p-value of 2.2e-16
# counties are strongly associated with FRPM elibility
# suggesting it is a good indicator for geographical inequity

# verifying p-value difference between Enrollment K-12 and Ages 5-17

model6 <- lm(`Percent (%) Eligible FRPM (K-12)` ~ `Enrollment (K-12)`, data = df_clean)
summary(model6)
# p-value: 7.9009e-13

model7 <- lm(`Percent (%) Eligible FRPM (K-12)` ~ `Enrollment (Ages 5-17)`, data = df_clean)
summary(model7)
# p-value: 5.673e-12
# From this, we know that both are significant indicators, will have slight variances but still statistically significant

# ======MULTIVARIATE ANALYSIS ======

# Q1: Do County and Enrollment explain % Eligible Free (K-12)?
model8 <- lm(`Percent (%) Eligible Free (K-12)` ~ `County Name` + `Enrollment (K-12)`, data = df_clean)
cat("\nQ1: County + Enrollment vs. % Eligible Free (K-12)\n")
summary(model8)
# p-value: 2.2e-16

# Q2: Do County and Enrollment explain % Eligible FRPM (K-12)?
model9 <- lm(`Percent (%) Eligible FRPM (K-12)` ~ `County Name` + `Enrollment (K-12)`, data = df_clean)
cat("\nQ2: County + Enrollment vs. % Eligible FRPM (K-12)\n")
summary(model9)
# p-value: 2.2e-16

# Q3: Do County and Enrollment explain % Eligible Free (Ages 5–17)?
model10 <- lm(`Percent (%) Eligible Free (Ages 5-17)` ~ `County Name` + `Enrollment (K-12)`, data = df_clean)
cat("\nQ3: County + Enrollment vs. % Eligible Free (Ages 5–17)\n")
summary(model10)
# p-value 2.2e-16

# Q4: Do County and Enrollment explain % Eligible FRPM (Ages 5–17)?
model11 <- lm(`Percent (%) Eligible FRPM (Ages 5-17)` ~ `County Name` + `Enrollment (K-12)`, data = df_clean)
cat("\nQ4: County + Enrollment vs. % Eligible FRPM (Ages 5–17)\n")
summary(model11)
# p-value 2.2e-16

# Ultimately, the p-values tell us that these predictors are statistically significant
# due to both large sample size and structural differences.

# ======= CORRELATION MATRIX =======
# Select relevant numeric columns
corr_data <- df_clean %>%
    transmute(
        enrollment_k_12 = as.numeric(`Enrollment (K-12)`),
        free_meal_count_k_12 = as.numeric(`Free Meal Count (K-12)`),
        percent_eligible_free_k_12 = as.numeric(gsub("%", "", `Percent (%) Eligible Free (K-12)`)),
        frpm_count_k_12 = as.numeric(`FRPM Count (K-12)`),
        percent_eligible_frpm_k_12 = `Percent (%) Eligible FRPM (K-12)`,
        enrollment_ages_5_17 = `Enrollment (Ages 5-17)`,
        free_meal_count_ages_5_17 = as.numeric(`Free Meal Count (Ages 5-17)`),
        percent_eligible_free_ages_5_17 = as.numeric(gsub("%", "", `Percent (%) Eligible Free (Ages 5-17)`)),
        frpm_count_ages_5_17 = as.numeric(`FRPM Count (Ages 5-17)`),
        percent_eligible_frpm_ages_5_17 = as.numeric(gsub("%", "", `Percent (%) Eligible FRPM (Ages 5-17)`))
    )

# Output correlation matrix to HTML
tab_corr(
    corr_data,
    triangle = "upper",
    p.numeric = TRUE,
    use = "pairwise.complete.obs",
    file = "FRPM_Correlation_Matrix.html"
)
# uncomment to view correlation matrix
# browseURL("FRPM_Correlation_Matrix.html")

# note: correlation matrices more useful for columns that are numeric
# all p-values are less than 0.05 meaning they are statistically significant
# which makes sense considering the sample size is very large
# we chose these categories as they are socioeconomic indicators 
# such as school size, meal counts, percent eligibility, understanding free vs frpm

######
# Numeric columns only for plotting
# Identify numeric variables
numeric_vars <- names(df)[sapply(df, is.numeric)]

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

