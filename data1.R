# Load necessary libraries
library(rvest)
library(dplyr)
library(shiny)
library(ggplot2)

# Scraping function to get iPhone data
scrape_iphone_data <- function() {
  amazon <- tryCatch({
    read_html("https://www.amazon.in/s?k=iphone&crid=154N2SMA50ZCL&sprefix=iphone%2Caps%2C321&ref=nb_sb_noss_1")
  }, error = function(e) {
    message("Error in loading the webpage: ", e$message)
    return(NULL)
  })
  
  if (is.null(amazon)) return(NULL)
  
  titles <- amazon %>% html_nodes(".a-size-medium") %>% html_text(trim = TRUE)
  ratings <- amazon %>% html_nodes(".a-icon-alt") %>% html_text(trim = TRUE)
  costs <- amazon %>% html_nodes(".a-price-whole") %>% html_text(trim = TRUE)
  
  min_length <- min(length(titles), length(ratings), length(costs))
  titles <- titles[1:min_length]
  ratings <- ratings[1:min_length]
  costs <- costs[1:min_length]
  
  timestamp <- Sys.time()
  
  iphone_data <- data.frame(
    Timestamp = rep(timestamp, min_length),
    Titles = titles,
    Ratings = ratings,
    Cost = costs,
    stringsAsFactors = FALSE
  )
  
  csv_file_path <- "iphone_data.csv"
  
  write.table(
    iphone_data,
    file = csv_file_path,
    sep = ",",
    row.names = FALSE,
    col.names = !file.exists(csv_file_path),
    append = TRUE
  )
  
  return(iphone_data)
}

# Run the scraping function
scraped_data <- scrape_iphone_data()