# Load necessary libraries
library(dplyr)
library(shiny)
library(ggplot2)

# Set the local time zone
Sys.setenv(TZ = "Africa/Nairobi")

# Function to clean cost values and convert to KES
clean_and_convert_cost <- function(cost_str, conversion_rate) {
    cost_str <- gsub(" ", "", cost_str)
    cost_str <- gsub(",", "", cost_str)
    cost_numeric <- suppressWarnings(as.numeric(gsub("^(0+)", "", cost_str)))
    if (is.na(cost_numeric)) {
        warning("Invalid cost value: ", cost_str)
        return(NA)
    }
    # Convert to KES
    cost_in_kes <- cost_numeric * conversion_rate
    return(cost_in_kes)
}

# Function to remove outliers using the IQR method
remove_outliers <- function(data) {
    Q1 <- quantile(data$Cost_numeric, 0.25, na.rm = TRUE)
    Q3 <- quantile(data$Cost_numeric, 0.75, na.rm = TRUE)
    IQR <- Q3 - Q1
    
    data_filtered <- data %>%
        filter(Cost_numeric >= (Q1 - 1.5 * IQR) & Cost_numeric <= (Q3 + 1.5 * IQR))
    
    return(data_filtered)
}

ui <- fluidPage(
    titlePanel("iPhone Price List and Ratings on Amazon"),
    
    sidebarLayout(
        sidebarPanel(
            selectInput("plot_choice", "Choose a Plot Type:",
                        choices = list("Scatter Plot (Ratings vs Cost)" = "scatter",
                                       "Box Plot (Cost by Rating)" = "box")),
            downloadButton("download_data", "Download Data as CSV")
        ),
        
        mainPanel(
            plotOutput("iphone_plot"),   # Display the plot at the top
            tableOutput("iphone_table"), # Display the table below the plot
            verbatimTextOutput("error_message"),
            verbatimTextOutput("current_time")
        )
    )
)

server <- function(input, output) {
    conversion_rate <- 16.06  # Set the conversion rate from INR to KES
    
    # Reactive expression to read, clean data, and remove outliers
    cleaned_data <- reactive({
        if (file.exists("iphone_data.csv")) {
            iphone_data <- read.table("iphone_data.csv", sep = ",", header = TRUE)
            
            # Clean and convert cost values
            iphone_data$Cost_numeric <- sapply(iphone_data$Cost, clean_and_convert_cost, conversion_rate)
            iphone_data$Ratings_numeric <- as.numeric(gsub(" out of 5 stars", "", iphone_data$Ratings))
            
            # Remove outliers
            iphone_data <- remove_outliers(iphone_data)
            
            return(iphone_data)
        } else {
            output$error_message <- renderText({
                "File 'iphone_data.csv' not found. Please ensure the file exists."
            })
            return(NULL)
        }
    })
    
    output$iphone_table <- renderTable({
        cleaned_data()  # Use the cleaned data for the table
    })
    
    output$current_time <- renderText({
        format(Sys.time(), tz = "Africa/Nairobi", usetz = TRUE)
    })
    
    output$download_data <- downloadHandler(
        filename = function() {
            paste("iphone_data_", Sys.Date(), ".csv", sep = "")
        },
        content = function(file) {
            if (file.exists("iphone_data.csv")) {
                file.copy("iphone_data.csv", file)
            } else {
                stop("File 'iphone_data.csv' not found. Please ensure the file exists.")
            }
        }
    )
    
    # Render the chosen plot based on user selection
    output$iphone_plot <- renderPlot({
        data <- cleaned_data()
        
        if (!is.null(data)) {
            plot_type <- input$plot_choice
            
            if (plot_type == "scatter") {
                # Scatter plot: Ratings vs Cost
                ggplot(data, aes(x = Ratings_numeric, y = Cost_numeric)) +
                    geom_point(aes(color = Titles), size = 3, alpha = 0.7) +
                    labs(title = "Scatter Plot of Ratings vs Cost",
                         x = "Ratings", y = "Cost (KES)") +
                    scale_y_continuous(labels = scales::comma) +
                    theme_minimal()
                
            } else if (plot_type == "box") {
                # Box plot: Cost by Rating
                data$Ratings <- factor(data$Ratings)  # Convert Ratings to factor
                ggplot(data, aes(x = Ratings, y = Cost_numeric)) +
                    geom_boxplot(aes(fill = Ratings), alpha = 0.7) +
                    labs(title = "Box Plot of Cost by Rating",
                         x = "Rating", y = "Cost (KES)") +
                    scale_y_continuous(labels = scales::comma) +
                    theme_minimal()
            }
            
        } else {
            output$error_message <- renderText("No data available for plotting.")
        }
    })
}

# Run the Shiny app
shinyApp(ui = ui, server = server)
