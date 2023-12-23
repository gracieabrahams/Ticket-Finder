
library(shiny)
library(DT)
library(tibble)
library(dplyr)
library(lubridate)
library(httr2)
library(rvest)
library(repurrrsive)
library(tidyverse)
library(ggplot2)
library(leaflet)
library(tidygeocoder)

# Function to fetch data from Ticketmaster API
fetch_ticketmaster_data <- function() {
  tm_client_id <- "oHpJrBEV0OKAJDbBFwOx84vWOY7hAyUH" 
  request("https://app.ticketmaster.com/discovery/v2/events.json") |>
    req_url_query(apikey = tm_client_id, stateCode = "dc", classificationName = "Music", size = 200) |>
    req_perform() ->
    tm
  
  df <- tibble(events = resp_body_json(tm)[[1]][[1]]) |>
    unnest_wider(col = events) |>
    unnest_longer(priceRanges) |>
    unnest_wider(col = priceRanges, names_sep = "_") |>
    unnest_wider(col=dates, names_sep = "_") |>
    unnest_wider(col=dates_start, names_sep = "_") |>
    unnest_wider(col = "_embedded") |>
    unnest_wider(col=venues, names_sep = "_") |>
    unnest_wider(col=venues_1, names_sep = "_") |>
    mutate(venue_name=venues_1_name) |>
    mutate(title=name) |>
    mutate(date_time = dates_start_dateTime) |>
    mutate(lowest_price = priceRanges_min) |>
    mutate(highest_price = priceRanges_max) |>
    mutate(date_time = ymd_hms(date_time),
           date = as.Date(date_time),
           time = format(date_time, format = "%H:%M:%S")) |>
    arrange(date)
  
  ticketmaster_data <- select(df, title, id, url, date, time, lowest_price, highest_price, venue_name)
  return(ticketmaster_data)
}

fetch_sg_data <- function() {
  sg_client_id <- "Mzg0NDE4NTJ8MTcwMDU1ODcwMS43OTQwNjUy"
  sg_client_secret <- "fc8f3a6efcfe9dc5417db7d5d7f3fcf62f6ec962f125ffc3029b1e7366977860"
  
  # function to fetch data from SeatGeek API
  fetch_events <- function(page = 1) {
    request("https://api.seatgeek.com/2/events") |>
      req_url_query(
        client_id = sg_client_id,
        client_secret = sg_client_secret,
        per_page = 10,
        page = page,
        type = "concert",
        venue.state="DC"
      ) |>
      req_perform() -> sg
    
    tibble(event = resp_body_json(resp = sg)[[1]]) |>
      unnest_wider(col = event) 
  }
  
  # Fetch data from multiple pages
  all_data <- map_dfr(1:5000:100, ~ fetch_events(.x))
  unnested <- all_data |>
    unnest_wider(col=stats) |>
    unnest_wider(col=venue, names_sep = "_") |>
    mutate(date_time = datetime_local) |>
    mutate(date_time = ymd_hms(date_time),
           date = as.Date(date_time),
           time = format(date_time, format = "%H:%M:%S")) |>
    arrange(date)
  
  sg <- select(unnested, title, id, url, date, time, lowest_price, highest_price, venue_name, venue_postal_code, venue_address, venue_extended_address)
  final_sg <- sg %>%
    mutate(full_address = paste(venue_address, venue_extended_address, sep = ", "))
  final_sg <- final_sg%>%
    select(-venue_extended_address, -venue_address)
  final_sg
  
  
  
  return(final_sg)
}

fetch_venue_data <- function() {
  final_sg <- fetch_sg_data()
  df_unique_venues <- final_sg %>%
    distinct(venue_name, .keep_all = TRUE)  
  df_unique_venues <- df_unique_venues %>%
    geocode(address = full_address, lat = "latitude", long = "longitude")
  
  return(df_unique_venues)
}

ui <- navbarPage(
  "Events and Tickets",
  tabPanel("Ticketmaster Table", 
           fluidRow(
             column(
               width = 12,
               DTOutput("tm_table")
             )
           )
  ),
  tabPanel("SeatGeek Table",
           fluidRow(
             column(
               width = 12,
               DTOutput("sg_table")
             )
           )
  ),
  tabPanel("Ticketmaster Graphs/Plots", 
           fluidRow(
             column(
               width = 6,
               verbatimTextOutput("tm_summary"),
               plotOutput("date_distribution"),
               plotOutput("lowest_price_distribution")
             ),
             column(
               width = 6,
               plotOutput("venue_distribution"),
               plotOutput("ticket_price_across_venues"),
               plotOutput("ticket_prices_over_time")
             )
           )
  ),
  tabPanel("SeatGeek Graphs/Plots", 
           fluidRow(
             column(
               width = 6,
               verbatimTextOutput("sg_summary"),
               plotOutput("sg_date_distribution"),
               plotOutput("sg_lowest_price_distribution")
             ), 
             column(
               width = 6,
               plotOutput("sg_venue_distribution"),
               plotOutput("sg_ticket_price_across_venues"),
               plotOutput("sg_ticket_prices_over_time")
             )
           )
  ),
  tabPanel("Both Datasets Graphs/Plots", 
           fluidRow(
             column(
               width = 6, 
               plotOutput("lowest_price_histogram"), 
               plotOutput("lowest_price_histogram1"), 
               plotOutput("scatter_low_high")
             ), 
             column(
               width = 6, 
               plotOutput("highest_price_histogram"),
               plotOutput("highest_price_histogram1"),
               plotOutput("scatter_lowest_price_by_venue"), 
               plotOutput("scatter_highest_price_by_venue")
             )
           )
  ),
  tabPanel("Map",
           fluidRow(
             column(
               width = 12,
               leafletOutput("concert_map")
             ),
             column(
               width = 6,
               verbatimTextOutput("smaller_venue_list")
             ),
             column(
               width = 6,
               verbatimTextOutput("well_known_venue_list")
             )
           )
  )
)

server <- function(input, output, session) {
  ticketmaster_data <- reactiveVal(NULL)
  seatgeek_data <- reactiveVal(NULL)
  
  observe({
    ticketmaster_data(fetch_ticketmaster_data())
    seatgeek_data(fetch_sg_data())
  })
  
  #Ticketmaster Table
  output$tm_table <- renderDT({
    req(ticketmaster_data())
    data <- ticketmaster_data()
    data$url <- sprintf('<a href="%s" target="_blank">%s</a>', data$url, data$url)
    
    datatable(data, escape = FALSE)
  })
  
  # SeatGeek Table
  output$sg_table <- renderDT({
    req(seatgeek_data())
    data <- seatgeek_data()
    data$url <- sprintf('<a href="%s" target="_blank">%s</a>', data$url, data$url)
    
    datatable(data, escape = FALSE)
  })
  
  # Summary Statistics for Ticketmaster
  output$tm_summary <- renderPrint({
    req(ticketmaster_data())
    summary(ticketmaster_data())
  })
  
  # Plot 1: Distribution of Concert Dates (Ticketmaster)
  output$date_distribution <- renderPlot({
    req(ticketmaster_data())
    ggplot(ticketmaster_data(), aes(x = date)) +
      geom_histogram(binwidth = 30, color = "#000000", fill = "#007A33") +
      labs(title = "Distribution of Concert Dates", x = "Date", y = "Frequency")
  })
  
  # Plot 2: Distribution of Lowest Ticket Prices (Ticketmaster)
  output$lowest_price_distribution <- renderPlot({
    req(ticketmaster_data())
    ggplot(ticketmaster_data(), aes(x = lowest_price)) +
      geom_histogram(color = "#000000", fill = "#007A33") +
      labs(title = "Distribution of Lowest Ticket Prices", x = "Lowest Ticket Price", y = "Frequency")
  })
  
  # Plot 3: Distribution of Events Across Venues (Ticketmaster)
  output$venue_distribution <- renderPlot({
    req(ticketmaster_data())
    ggplot(ticketmaster_data(), aes(x = venue_name)) +
      geom_bar(color = "#000000", fill = "#007A33") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Distribution of Events Across Venues", x = "Venue", y = "Frequency")
  })
  
  # Plot 4: Ticket Price Distribution Across Venues (Ticketmaster)
  output$ticket_price_across_venues <- renderPlot({
    req(ticketmaster_data())
    ggplot(ticketmaster_data(), aes(x = venue_name, y = lowest_price)) +
      geom_boxplot(color = "#000000", fill = "#007A33") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Ticket Price Distribution Across Venues", x = "Venue", y = "Lowest Ticket Price")
  })
  
  # Plot 5: Ticket Prices Over Time (Ticketmaster)
  output$ticket_prices_over_time <- renderPlot({
    req(ticketmaster_data())
    ggplot(ticketmaster_data(), aes(x = date, y = lowest_price)) +
      geom_point(color = "#007A33") +
      labs(title = "Ticket Prices Over Time", x = "Date", y = "Lowest Ticket Price")
  })
  
  # Summary Statistics for SeatGeek
  output$sg_summary <- renderPrint({
    req(seatgeek_data())
    summary(seatgeek_data())
  })
  
  # Plot 1: Distribution of Concert Dates (SeatGeek)
  output$sg_date_distribution <- renderPlot({
    req(seatgeek_data())
    sg_hist1 <- seatgeek_data() |> 
      mutate(dataset = "SeatGeek") |> 
      filter(date < "2025-01-01")
    ggplot(data = sg_hist1, aes(x = date)) +
      geom_histogram(binwidth = 30, color = "#04328C", fill = "#CC122C") +
      labs(title = "Distribution of Concert Dates (SeatGeek)", x = "Date", y = "Frequency") 
  })
  
  # Plot 2: Distribution of Lowest Ticket Prices (SeatGeek)
  output$sg_lowest_price_distribution <- renderPlot({
    req(seatgeek_data())
    sg_hist2 <- seatgeek_data() |> 
      mutate(dataset = "SeatGeek") |> 
      filter(lowest_price < 2000)
    ggplot(sg_hist2, aes(x = lowest_price)) +
      geom_histogram(color = "#04328C", fill = "#CC122C") +
      labs(title = "Distribution of Lowest Ticket Prices (SeatGeek)", x = "Lowest Ticket Price", y = "Frequency")
  })
  
  # Plot 3: Distribution of Events Across Venues (SeatGeek)
  output$sg_venue_distribution <- renderPlot({
    req(seatgeek_data())
    ggplot(seatgeek_data(), aes(x = venue_name)) +
      geom_bar(color = "#04328C", fill = "#CC122C") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Distribution of Events Across Venues (SeatGeek)", x = "Venue", y = "Frequency")
  })
  
  # Plot 4: Ticket Price Distribution Across Venues (SeatGeek)
  output$sg_ticket_price_across_venues <- renderPlot({
    req(seatgeek_data())
    sg_box1 <- seatgeek_data() |> 
      mutate(dataset = "SeatGeek") |> 
      filter(lowest_price < 2000)
    ggplot(sg_box1, aes(x = venue_name, y = lowest_price)) +
      geom_boxplot(color = "#04328C", fill = "#CC122C") +
      theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      labs(title = "Ticket Price Distribution Across Venues (SeatGeek)", x = "Venue", y = "Lowest Ticket Price")
  })
  
  # Plot 5: Ticket Prices Over Time (SeatGeek)
  output$sg_ticket_prices_over_time <- renderPlot({
    req(seatgeek_data())
    sg_point1 <- seatgeek_data() |> 
      mutate(dataset = "SeatGeek") |> 
      filter(lowest_price < 2000) |> 
      filter(date < "2025-01-01")
    ggplot(sg_point1, aes(x = date, y = lowest_price)) +
      geom_point(color = "#04328C") +
      labs(title = "Ticket Prices Over Time (SeatGeek)", x = "Date", y = "Lowest Ticket Price")
  })
  
  # Histogram of Lowest Ticket Prices (Full)
  output$lowest_price_histogram <- renderPlot({
    req(ticketmaster_data())
    req(seatgeek_data())
    
    tm_histogram_data <- mutate(ticketmaster_data(), dataset = "Ticketmaster")
    sg_histogram_data <- mutate(seatgeek_data(), dataset = "SeatGeek")
    
    ggplot(mapping = aes(x = lowest_price)) +
      geom_histogram(data = tm_histogram_data, aes(fill = dataset), alpha = 0.7, bins = 30) +
      geom_histogram(data = sg_histogram_data, aes(fill = dataset), alpha = 0.7, bins = 30) +
      labs(title = "Distribution of Lowest Ticket Prices",
           x = "Lowest Ticket Price",
           y = "Frequency") +
      theme_minimal()
  })
  
  # Histogram of Lowest Ticket Prices (Filtered)
  output$lowest_price_histogram1 <- renderPlot({
    req(ticketmaster_data())
    req(seatgeek_data())
    
    tm_histogram_data <- mutate(ticketmaster_data(), dataset = "Ticketmaster")
    sg_histogram_data <- mutate(seatgeek_data(), dataset = "SeatGeek") |> 
      filter(lowest_price < 200)
    
    ggplot(mapping = aes(x = lowest_price)) +
      geom_histogram(data = tm_histogram_data, aes(fill = dataset), alpha = 0.7, bins = 30) +
      geom_histogram(data = sg_histogram_data, aes(fill = dataset), alpha = 0.7, bins = 30) +
      labs(title = "Distribution of Lowest Ticket Prices",
           x = "Lowest Ticket Price",
           y = "Frequency") +
      theme_minimal()
  })
  
  # Histogram of Highest Ticket Prices (Full)
  output$highest_price_histogram <- renderPlot({
    req(ticketmaster_data())
    req(seatgeek_data())
    
    tm_histogram_data <- mutate(ticketmaster_data(), dataset = "Ticketmaster")
    sg_histogram_data <- mutate(seatgeek_data(), dataset = "SeatGeek")
    
    ggplot(mapping = aes(x = highest_price)) +
      geom_histogram(data = tm_histogram_data, aes(fill = dataset), alpha = 0.7, bins = 30) +
      geom_histogram(data = sg_histogram_data, aes(fill = dataset), alpha = 0.7, bins = 30) +
      labs(title = "Distribution of Highest Ticket Prices",
           x = "Highest Ticket Price",
           y = "Frequency") +
      theme_minimal()
  })
  
  # Histogram of Highest Ticket Prices (Filtered)
  output$highest_price_histogram1 <- renderPlot({
    req(ticketmaster_data())
    req(seatgeek_data())
    
    tm_histogram_data <- mutate(ticketmaster_data(), dataset = "Ticketmaster")
    sg_histogram_data <- mutate(seatgeek_data(), dataset = "SeatGeek") |> 
      filter(highest_price < 2500)
    
    ggplot(mapping = aes(x = highest_price)) +
      geom_histogram(data = tm_histogram_data, aes(fill = dataset), alpha = 0.7, bins = 30) +
      geom_histogram(data = sg_histogram_data, aes(fill = dataset), alpha = 0.7, bins = 30) +
      labs(title = "Distribution of Highest Ticket Prices",
           x = "Highest Ticket Price",
           y = "Frequency") +
      theme_minimal()
  })
  
  # Scatter Plot of Lowest vs. Highest Ticket Prices
  output$scatter_low_high <- renderPlot({
    req(ticketmaster_data())
    req(seatgeek_data())
    
    ggplot() +
      geom_point(data = mutate(ticketmaster_data(), dataset = "Ticketmaster"), aes(x = lowest_price, y = highest_price, color = 
                                                                                     "Ticketmaster"), alpha = 0.7) +
      geom_point(data = mutate(seatgeek_data(), dataset = "SeatGeek"), aes(x = lowest_price, y = highest_price, color = "SeatGeek"), 
                 alpha = 0.7) +
      labs(title = "Scatter Plot of Lowest vs. Highest Ticket Prices",
           x = "Lowest Ticket Price",
           y = "Highest Ticket Price",
           color = "Dataset") +
      theme_minimal()
  })
  
  # Scatterplot of Lowest Price by Venue
  output$scatter_lowest_price_by_venue <- renderPlot({
    req(ticketmaster_data(), seatgeek_data())
    
    ticketmaster_data <- mutate(ticketmaster_data(), id = as.character(id))
    seatgeek_data <- mutate(seatgeek_data(), id = as.character(id))
    
    combined_data <- bind_rows(
      mutate(ticketmaster_data, dataset = "Ticketmaster"),
      mutate(seatgeek_data, dataset = "SeatGeek")
    )
    
    ggplot(combined_data, aes(x = venue_name, y = lowest_price, color = dataset)) +
      geom_point(alpha = 0.7) +
      labs(title = "Scatter Plot of Lowest Ticket Prices by Venue",
           x = "Venue",
           y = "Lowest Ticket Price",
           color = "Dataset") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  # Scatterplot of Highest Price by Venue
  output$scatter_highest_price_by_venue <- renderPlot({
    req(ticketmaster_data(), seatgeek_data())
    
    ticketmaster_data <- mutate(ticketmaster_data(), id = as.character(id))
    seatgeek_data <- mutate(seatgeek_data(), id = as.character(id))
    
    combined_data <- bind_rows(
      mutate(ticketmaster_data, dataset = "Ticketmaster"),
      mutate(seatgeek_data, dataset = "SeatGeek") |> 
        filter(highest_price < 5000)
    )
    
    ggplot(combined_data, aes(x = venue_name, y = highest_price, color = dataset)) +
      geom_point(alpha = 0.7) +
      labs(title = "Scatter Plot of Highest Ticket Prices by Venue",
           x = "Venue",
           y = "Highest Ticket Price",
           color = "Dataset") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$concert_map <- renderLeaflet({
    leaflet(fetch_venue_data()) %>%
      addTiles() %>%
      addMarkers(lng = ~longitude, lat = ~latitude, popup = ~venue_name)
  })
  
  #https://www.r-bloggers.com/2022/08/difference-between-cat-and-paste-in-r/ used to concat
  output$smaller_venue_list <- renderPrint({
    req(fetch_sg_data())
    final_sg <- fetch_sg_data()
    venue_counts <- final_sg %>%
      mutate(venue_name_lower = tolower(trimws(venue_name))) %>%
      count(venue_name_lower, sort = TRUE) %>%
      mutate(
        venue_category = ifelse(n > 10, "well_known", "smaller_venue")
      )
    
    smaller_venues <- venue_counts %>%
      filter(venue_category == "smaller_venue") %>%
      pull(venue_name_lower)
    cat(paste("Smaller Venues To Check Out:\n", paste(smaller_venues, collapse = "\n")))
  })
  
  output$well_known_venue_list <- renderPrint({
    req(fetch_sg_data())
    final_sg <- fetch_sg_data()
    venue_counts <- final_sg %>%
      mutate(venue_name_lower = tolower(trimws(venue_name))) %>%
      count(venue_name_lower, sort = TRUE) %>%
      mutate(
        venue_category = ifelse(n > 10, "well_known", "smaller_venue")
      )
    
    well_known_venues <- venue_counts %>%
      filter(venue_category == "well_known") %>%
      pull(venue_name_lower)
    cat(paste("Well-Known Venues:\n", paste(well_known_venues, collapse = "\n")))
  })
}

shinyApp(ui, server)