# Airbnb Tourist Analysis Dashboard - Six Global Cities
# ALY 6070 - Communication and Visualization for Data Analytics
# Northeastern University

# Kwabena Duffuor Asante (Team Lead)
# Audrey Appraku
# Usha Rani Nallaparaju
# Gifty Afful
# ============================================================================

# Install required packages (Run once)
# install.packages(c("shiny","shinydashboard","tidyverse","plotly","DT","viridis","leaflet","leaflet.extras"))


# LIBRARIES
# ============================================================================
library(shiny)
library(shinydashboard)
library(tidyverse)
library(plotly)
library(DT)
library(viridis)
library(leaflet)
library(leaflet.extras)


# DATA LOADING & PREPARATION
# ============================================================================

# Load Airbnb datasets
austin <- read.csv("data/listings_austin.csv", stringsAsFactors = FALSE)
bangkok <- read.csv("data/listings_bangkok.csv", stringsAsFactors = FALSE)
buenos_aires <- read.csv("data/listings_buenos_aires.csv", stringsAsFactors = FALSE)
cape_town <- read.csv("data/listings_cape_town.csv", stringsAsFactors = FALSE)
istanbul <- read.csv("data/listings_istanbul.csv", stringsAsFactors = FALSE)
melbourne <- read.csv("data/listings_melbourne.csv", stringsAsFactors = FALSE)

# Convert neighbourhood to character for consistency
austin$neighbourhood <- as.character(austin$neighbourhood)
bangkok$neighbourhood <- as.character(bangkok$neighbourhood)
buenos_aires$neighbourhood <- as.character(buenos_aires$neighbourhood)
cape_town$neighbourhood <- as.character(cape_town$neighbourhood)
istanbul$neighbourhood <- as.character(istanbul$neighbourhood)
melbourne$neighbourhood <- as.character(melbourne$neighbourhood)

# Load Cost of Living data
cost_of_living <- read.csv("data/cost_of_living.csv", stringsAsFactors = FALSE)

# Add city identifiers
austin$city <- "Austin"
bangkok$city <- "Bangkok"
buenos_aires$city <- "Buenos Aires"
cape_town$city <- "Cape Town"
istanbul$city <- "Istanbul"
melbourne$city <- "Melbourne"

# Combine all datasets
airbnb_data <- bind_rows(
  austin, bangkok, buenos_aires, 
  cape_town, istanbul, melbourne
)


# DATA CLEANING & TRANSFORMATION
# ============================================================================

airbnb_clean <- airbnb_data %>%
  # Remove rows with missing critical values
  filter(
    !is.na(price), 
    !is.na(latitude), 
    !is.na(longitude)
  ) %>%
  # Remove extreme outliers
  filter(
    price > 0, 
    price <= 10000
  ) %>%
  # Clean room_type
  mutate(
    room_type = ifelse(is.na(room_type), "Unknown", room_type),
    number_of_reviews = as.numeric(number_of_reviews),
    number_of_reviews = ifelse(is.na(number_of_reviews), 0, number_of_reviews),
    availability_365 = as.numeric(availability_365),
    availability_365 = ifelse(is.na(availability_365), 0, availability_365)
  )

# Merge with cost of living data
airbnb_final <- airbnb_clean %>%
  left_join(cost_of_living, by = "city")


# GLOBAL COLOR PALETTES
# ============================================================================

# City colors - Professional, distinct palette
city_colors <- c(
  "Austin" = "#1f77b4",       # Blue
  "Bangkok" = "#ff7f0e",      # Orange
  "Buenos Aires" = "#d62728", # Red
  "Cape Town" = "#2ca02c",    # Green
  "Istanbul" = "#9467bd",     # Purple
  "Melbourne" = "#8c564b"     # Brown
)

# Room type colors - Consistent with dashboard theme
room_colors <- c(
  "Entire home/apt" = "#1f77b4",
  "Private room" = "#ff7f0e",
  "Shared room" = "#2ca02c",
  "Hotel room" = "#d62728",
  "Unknown" = "#9467bd"
)

# Trip duration colors
trip_colors <- c(
  "3 Days" = "#1f77b4",
  "5 Days" = "#ff7f0e", 
  "7 Days" = "#2ca02c"
)


# HELPER FUNCTIONS
# ============================================================================

#' Create Tourist Budget Planning Table
#' @param data Filtered Airbnb dataset
#' @return Dataframe with budget calculations
create_tourist_budget_table <- function(data) {
  data %>%
    group_by(city, room_type) %>%
    summarise(
      avg_accommodation = round(mean(price, na.rm = TRUE), 2),
      median_accommodation = round(median(price, na.rm = TRUE), 2),
      daily_living_cost = round(mean(daily_budget_usd, na.rm = TRUE), 2),
      avg_meal_cost = round(mean(meal_cost_usd, na.rm = TRUE), 2),
      total_daily_budget = round(avg_accommodation + daily_living_cost, 2),
      listings_available = n(),
      .groups = 'drop'
    ) %>%
    arrange(city, room_type)
}

#' Format currency for display
#' @param x Numeric value
#' @return Formatted currency string
format_currency <- function(x) {
  paste0("$", format(round(x, 0), big.mark = ","))
}

#' Format large numbers with commas
#' @param x Numeric value
#' @return Formatted number string
format_number <- function(x) {
  format(x, big.mark = ",")
}


# CHART FUNCTIONS
# ============================================================================

# Average Price by City - Horizontal Bar Chart
#---------------------------------------------------------------------------
create_price_by_city <- function(data) {
  city_prices <- data %>%
    group_by(city) %>%
    summarise(avg_price = mean(price, na.rm = TRUE)) %>%
    arrange(desc(avg_price))
  
  max_price <- max(city_prices$avg_price, na.rm = TRUE)
  
  plot_ly(
    data = city_prices,
    x = ~avg_price,
    y = ~reorder(city, avg_price),
    type = "bar",
    orientation = "h",
    marker = list(
      color = city_colors[city_prices$city],
      line = list(color = "#FFFFFF", width = 1)
    ),
    text = ~format_currency(avg_price),
    textposition = "outside",
    textfont = list(size = 11, color = "#333333"),
    cliponaxis = FALSE,
    hoverinfo = "text",
    hovertext = ~paste0(
      "<b>", city, "</b><br>",
      "Average Price: ", format_currency(avg_price), "<br>",
      "Listings: ", format_number(nrow(data[data$city == city,]))
    )
  ) %>%
    layout(
      title = list(
        text = "Average Nightly Rate by City",
        font = list(size = 16, color = "#2c3e50"),
        x = 0.03
      ),
      xaxis = list(
        title = "Average Price (USD)",
        titlefont = list(size = 12, color = "#7f8c8d"),
        tickfont = list(size = 11, color = "#7f8c8d"),
        range = c(0, max_price * 1.2),
        fixedrange = TRUE,
        gridcolor = "#ecf0f1",
        zerolinecolor = "#ecf0f1"
      ),
      yaxis = list(
        title = "",
        tickfont = list(size = 11, color = "#2c3e50"),
        gridcolor = "#ecf0f1"
      ),
      margin = list(l = 80, r = 60, t = 60, b = 40),
      height = 320,
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff"
    )
}


# Room Type Distribution - Pie Chart with Improved Labels
#---------------------------------------------------------------------------
create_room_type_distribution <- function(data) {
  room_counts <- data %>%
    group_by(room_type) %>%
    summarise(count = n()) %>%
    arrange(desc(count)) %>%
    mutate(
      percentage = count / sum(count) * 100,
      label = paste0(room_type, "\n", round(percentage, 1), "%")
    )
  
  plot_ly(
    room_counts,
    labels = ~room_type,
    values = ~count,
    type = 'pie',
    text = ~label,
    textinfo = 'text',
    textposition = ~ifelse(percentage < 5, 'outside', 'inside'),
    insidetextfont = list(size = 12, color = "white"),
    outsidetextfont = list(size = 11, color = "#2c3e50"),
    marker = list(
      colors = room_colors[room_counts$room_type],
      line = list(color = '#FFFFFF', width = 2)
    ),
    pull = ~ifelse(percentage < 5, 0.1, 0),
    hoverinfo = 'text',
    hovertext = ~paste0(
      "<b>", room_type, "</b><br>",
      "Count: ", format_number(count), "<br>",
      "Percentage: ", round(percentage, 1), "%"
    )
  ) %>%
    layout(
      title = list(
        text = "Room Type Distribution",
        font = list(size = 16, color = "#2c3e50"),
        x = 0.03
      ),
      showlegend = FALSE,
      margin = list(l = 40, r = 40, t = 60, b = 60),
      annotations = list(
        list(
          text = paste0("Total Listings: ", format_number(sum(room_counts$count))),
          x = 0.5, y = -0.15,
          showarrow = FALSE,
          font = list(size = 12, color = "#7f8c8d")
        )
      ),
      height = 350,
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff"
    )
}


# Average Price by Room Type - Horizontal Bar Chart
#---------------------------------------------------------------------------
create_price_by_room_type <- function(data) {
  room_prices <- data %>%
    group_by(room_type) %>%
    summarise(avg_price = mean(price, na.rm = TRUE)) %>%
    arrange(desc(avg_price))
  
  max_price <- max(room_prices$avg_price, na.rm = TRUE)
  
  plot_ly(
    room_prices,
    x = ~avg_price,
    y = ~reorder(room_type, avg_price),
    type = 'bar',
    orientation = 'h',
    marker = list(
      color = room_colors[room_prices$room_type],
      line = list(color = "#FFFFFF", width = 1)
    ),
    text = ~format_currency(avg_price),
    textposition = 'outside',
    textfont = list(size = 11, color = "#333333"),
    cliponaxis = FALSE,
    hoverinfo = 'text',
    hovertext = ~paste0(
      "<b>", room_type, "</b><br>",
      "Average Price: ", format_currency(avg_price), "<br>",
      "Listings: ", format_number(nrow(data[data$room_type == room_type,]))
    )
  ) %>%
    layout(
      title = list(
        text = "Average Rate by Room Type",
        font = list(size = 16, color = "#2c3e50"),
        x = 0.03
      ),
      xaxis = list(
        title = "Average Price (USD)",
        titlefont = list(size = 12, color = "#7f8c8d"),
        tickfont = list(size = 11, color = "#7f8c8d"),
        range = c(0, max_price * 1.2),
        fixedrange = TRUE,
        gridcolor = "#ecf0f1",
        zerolinecolor = "#ecf0f1"
      ),
      yaxis = list(
        title = "",
        tickfont = list(size = 11, color = "#2c3e50"),
        gridcolor = "#ecf0f1"
      ),
      margin = list(l = 120, r = 60, t = 60, b = 40),
      height = 320,
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff"
    )
}


# Price vs Reviews - Scatter Plot
#---------------------------------------------------------------------------
create_price_vs_reviews <- function(data) {
  scatter_data <- data %>%
    filter(
      !is.na(price), 
      !is.na(number_of_reviews),
      price <= 1000, 
      number_of_reviews <= 500
    ) %>%
    sample_n(min(2000, nrow(.)))  # Sample for performance
  
  plot_ly(
    scatter_data,
    x = ~price,
    y = ~number_of_reviews,
    color = ~city,
    colors = city_colors,
    type = 'scatter',
    mode = 'markers',
    marker = list(
      size = 6,
      opacity = 0.7,
      line = list(color = "#FFFFFF", width = 0.5)
    ),
    hoverinfo = 'text',
    hovertext = ~paste0(
      "<b>", city, "</b><br>",
      "Price: ", format_currency(price), "<br>",
      "Reviews: ", format_number(number_of_reviews), "<br>",
      "Room Type: ", room_type
    )
  ) %>%
    layout(
      title = list(
        text = "Price vs. Number of Reviews",
        font = list(size = 16, color = "#2c3e50"),
        x = 0.03
      ),
      xaxis = list(
        title = "Price (USD)",
        titlefont = list(size = 12, color = "#7f8c8d"),
        tickfont = list(size = 11, color = "#7f8c8d"),
        gridcolor = "#ecf0f1",
        zerolinecolor = "#ecf0f1"
      ),
      yaxis = list(
        title = "Number of Reviews",
        titlefont = list(size = 12, color = "#7f8c8d"),
        tickfont = list(size = 11, color = "#7f8c8d"),
        gridcolor = "#ecf0f1",
        zerolinecolor = "#ecf0f1"
      ),
      legend = list(
        font = list(size = 10, color = "#2c3e50"),
        bgcolor = "#ffffff",
        bordercolor = "#ecf0f1",
        borderwidth = 1,
        orientation = 'h',
        y = -0.4
      ),
      margin = list(l = 60, r = 40, t = 60, b = 80),
      height = 320,
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff"
    )
}


# Top 10 Neighborhoods - Horizontal Bar Chart
#---------------------------------------------------------------------------
create_top_neighborhoods <- function(data) {
  top_neighborhoods <- data %>%
    group_by(neighbourhood, city) %>%
    summarise(count = n(), .groups = 'drop') %>%
    arrange(desc(count)) %>%
    head(10) %>%
    mutate(
      display_name = ifelse(
        nchar(neighbourhood) > 25,
        paste0(substr(neighbourhood, 1, 22), "... (", city, ")"),
        paste0(neighbourhood, " (", city, ")")
      ),
      color = city_colors[city]
    )
  
  plot_ly(
    top_neighborhoods,
    x = ~count,
    y = ~reorder(display_name, count),
    type = 'bar',
    orientation = 'h',
    marker = list(
      color = ~color,
      line = list(color = "#FFFFFF", width = 1)
    ),
    text = ~format_number(count),
    textposition = 'outside',
    textfont = list(size = 11, color = "#333333"),
    cliponaxis = FALSE,
    hoverinfo = 'text',
    hovertext = ~paste0(
      "<b>", neighbourhood, "</b><br>",
      "City: ", city, "<br>",
      "Listings: ", format_number(count)
    )
  ) %>%
    layout(
      title = list(
        text = "Top 10 Neighborhoods by Listings",
        font = list(size = 16, color = "#2c3e50"),
        x = 0.03
      ),
      xaxis = list(
        title = "Number of Listings",
        titlefont = list(size = 12, color = "#7f8c8d"),
        tickfont = list(size = 11, color = "#7f8c8d"),
        range = c(0, max(top_neighborhoods$count) * 1.15),
        gridcolor = "#ecf0f1",
        zerolinecolor = "#ecf0f1"
      ),
      yaxis = list(
        title = "",
        tickfont = list(size = 10, color = "#2c3e50"),
        gridcolor = "#ecf0f1"
      ),
      margin = list(l = 140, r = 60, t = 60, b = 40),
      height = 350,
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff"
    )
}


# Availability by City - Horizontal Bar Chart
#---------------------------------------------------------------------------
create_availability_by_city <- function(data) {
  availability_data <- data %>%
    group_by(city) %>%
    summarise(
      avg_availability = mean(availability_365, na.rm = TRUE),
      median_availability = median(availability_365, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(desc(avg_availability))
  
  max_avail <- max(availability_data$avg_availability, na.rm = TRUE)
  
  plot_ly(
    availability_data,
    x = ~avg_availability,
    y = ~reorder(city, avg_availability),
    type = 'bar',
    orientation = 'h',
    marker = list(
      color = city_colors[availability_data$city],
      line = list(color = "#FFFFFF", width = 1)
    ),
    text = ~paste0(round(avg_availability, 0), " days"),
    textposition = 'outside',
    textfont = list(size = 11, color = "#333333"),
    cliponaxis = FALSE,
    hoverinfo = 'text',
    hovertext = ~paste0(
      "<b>", city, "</b><br>",
      "Average Availability: ", round(avg_availability, 0), " days<br>",
      "Median Availability: ", round(median_availability, 0), " days"
    )
  ) %>%
    layout(
      title = list(
        text = "Average Yearly Availability by City",
        font = list(size = 16, color = "#2c3e50"),
        x = 0.03
      ),
      xaxis = list(
        title = "Days Available per Year",
        titlefont = list(size = 12, color = "#7f8c8d"),
        tickfont = list(size = 11, color = "#7f8c8d"),
        range = c(0, max_avail * 1.15),
        fixedrange = TRUE,
        gridcolor = "#ecf0f1",
        zerolinecolor = "#ecf0f1"
      ),
      yaxis = list(
        title = "",
        tickfont = list(size = 11, color = "#2c3e50"),
        gridcolor = "#ecf0f1"
      ),
      margin = list(l = 80, r = 80, t = 60, b = 40),
      height = 320,
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff"
    )
}


# Price Distribution - Histogram
#---------------------------------------------------------------------------
create_price_distribution <- function(data) {
  price_data <- data %>%
    filter(price <= 500)  # Tourist-friendly range
  
  plot_ly(
    price_data,
    x = ~price,
    type = 'histogram',
    nbinsx = 25,
    marker = list(
      color = "#3498db",
      line = list(color = "#FFFFFF", width = 1)
    ),
    hoverinfo = 'text',
    hovertext = ~paste0(
      "Price: $", round(price, 0), "<br>",
      "Count: ", format_number(length(price))
    )
  ) %>%
    layout(
      title = list(
        text = "Price Distribution (Tourist Range: $0-500)",
        font = list(size = 16, color = "#2c3e50"),
        x = 0.03
      ),
      xaxis = list(
        title = "Nightly Rate (USD)",
        titlefont = list(size = 12, color = "#7f8c8d"),
        tickfont = list(size = 11, color = "#7f8c8d"),
        gridcolor = "#ecf0f1",
        zerolinecolor = "#ecf0f1"
      ),
      yaxis = list(
        title = "Number of Listings",
        titlefont = list(size = 12, color = "#7f8c8d"),
        tickfont = list(size = 11, color = "#7f8c8d"),
        gridcolor = "#ecf0f1",
        zerolinecolor = "#ecf0f1"
      ),
      bargap = 0.1,
      margin = list(l = 60, r = 40, t = 60, b = 60),
      height = 320,
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff"
    )
}


# Total Trip Cost Comparison - Grouped Bar Chart
#---------------------------------------------------------------------------
create_total_trip_cost <- function(data) {
  trip_costs <- data %>%
    group_by(city) %>%
    summarise(
      accommodation_per_night = mean(price, na.rm = TRUE),
      daily_living_cost = mean(daily_budget_usd, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    mutate(
      three_day_total = (accommodation_per_night * 3) + (daily_living_cost * 3),
      five_day_total = (accommodation_per_night * 5) + (daily_living_cost * 5),
      seven_day_total = (accommodation_per_night * 7) + (daily_living_cost * 7)
    ) %>%
    select(city, three_day_total, five_day_total, seven_day_total) %>%
    pivot_longer(
      cols = -city, 
      names_to = "trip_duration", 
      values_to = "total_cost"
    ) %>%
    mutate(
      trip_duration = case_when(
        trip_duration == "three_day_total" ~ "3 Days",
        trip_duration == "five_day_total" ~ "5 Days",
        trip_duration == "seven_day_total" ~ "7 Days"
      ),
      city = factor(city),
      trip_duration = factor(trip_duration, levels = c("3 Days", "5 Days", "7 Days"))
    )
  
  max_cost <- max(trip_costs$total_cost, na.rm = TRUE)
  
  plot_ly(
    trip_costs,
    x = ~city,
    y = ~total_cost,
    color = ~trip_duration,
    colors = trip_colors,
    type = 'bar',
    text = ~format_currency(total_cost),
    textposition = 'outside',
    textfont = list(size = 10, color = "#333333"),
    cliponaxis = FALSE,
    hovertemplate = paste(
      "<b>%{x}</b><br>",
      "%{legend}: %{y:$,.0f}<br>",
      "<extra></extra>"
    )
  ) %>%
    layout(
      title = list(
        text = "Total Trip Cost: Accommodation + Living Expenses",
        font = list(size = 16, color = "#2c3e50"),
        x = 0.03
      ),
      xaxis = list(
        title = "",
        tickfont = list(size = 11, color = "#2c3e50"),
        categoryorder = "array",
        categoryarray = sort(levels(trip_costs$city))
      ),
      yaxis = list(
        title = "Total Cost (USD)",
        titlefont = list(size = 12, color = "#7f8c8d"),
        tickfont = list(size = 11, color = "#7f8c8d"),
        range = c(0, max_cost * 1.2),
        tickformat = "$,.0f",
        gridcolor = "#ecf0f1",
        zerolinecolor = "#ecf0f1"
      ),
      barmode = 'group',
      legend = list(
        title = list(text = "Trip Duration"),
        font = list(size = 11, color = "#2c3e50"),
        bgcolor = "#ffffff",
        bordercolor = "#ecf0f1",
        borderwidth = 1,
        orientation = 'h',
        y = -0.25
      ),
      margin = list(l = 60, r = 40, t = 60, b = 80),
      height = 320,
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff"
    )
}


# Room Type Availability Heatmap
#---------------------------------------------------------------------------
create_room_availability_heatmap <- function(data) {
  heatmap_data <- data %>%
    group_by(city, room_type) %>%
    summarise(
      avg_availability = mean(availability_365, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    complete(city, room_type, fill = list(avg_availability = 0))
  
  plot_ly(
    data = heatmap_data,
    x = ~room_type,
    y = ~city,
    z = ~avg_availability,
    type = 'heatmap',
    colorscale = 'Viridis',
    hovertemplate = paste(
      "<b>%{y}</b><br>",
      "Room Type: %{x}<br>",
      "Avg Availability: %{z:.0f} days<br>",
      "<extra></extra>"
    )
  ) %>%
    layout(
      title = list(
        text = "Average Availability by City and Room Type",
        font = list(size = 16, color = "#2c3e50"),
        x = 0.03
      ),
      xaxis = list(
        title = "Room Type",
        titlefont = list(size = 12, color = "#7f8c8d"),
        tickfont = list(size = 10, color = "#2c3e50"),
        tickangle = 0
      ),
      yaxis = list(
        title = "City",
        titlefont = list(size = 12, color = "#7f8c8d"),
        tickfont = list(size = 10, color = "#2c3e50"),
        categoryorder = "array",
        categoryarray = sort(unique(heatmap_data$city))
      ),
      margin = list(l = 80, r = 40, t = 60, b = 80),
      height = 350,
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff"
    )
}


# Value for Money Analysis - Horizontal Bar Chart
#---------------------------------------------------------------------------
create_value_analysis <- function(data) {
  value_data <- data %>%
    filter(number_of_reviews > 10) %>%
    mutate(price_per_review = price / (number_of_reviews + 1)) %>%
    group_by(city) %>%
    summarise(
      avg_price_per_review = mean(price_per_review, na.rm = TRUE),
      median_price_per_review = median(price_per_review, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    arrange(avg_price_per_review)
  
  max_value <- max(value_data$avg_price_per_review, na.rm = TRUE)
  
  plot_ly(
    value_data,
    x = ~avg_price_per_review,
    y = ~reorder(city, -avg_price_per_review),
    type = 'bar',
    orientation = 'h',
    marker = list(
      color = city_colors[value_data$city],
      line = list(color = "#FFFFFF", width = 1)
    ),
    text = ~paste0("$", round(avg_price_per_review, 2)),
    textposition = 'outside',
    textfont = list(size = 11, color = "#333333"),
    cliponaxis = FALSE,
    hoverinfo = 'text',
    hovertext = ~paste0(
      "<b>", city, "</b><br>",
      "Average Price/Review: $", round(avg_price_per_review, 2), "<br>",
      "Median Price/Review: $", round(median_price_per_review, 2), "<br>",
      "<i>Lower = Better Value</i>"
    )
  ) %>%
    layout(
      title = list(
        text = "Value for Money: Price per Review Ratio",
        font = list(size = 16, color = "#2c3e50"),
        x = 0.03
      ),
      xaxis = list(
        title = "Price per Review (USD) - Lower is Better",
        titlefont = list(size = 12, color = "#7f8c8d"),
        tickfont = list(size = 11, color = "#7f8c8d"),
        range = c(0, max_value * 1.2),
        fixedrange = TRUE,
        gridcolor = "#ecf0f1",
        zerolinecolor = "#ecf0f1"
      ),
      yaxis = list(
        title = "",
        tickfont = list(size = 11, color = "#2c3e50"),
        gridcolor = "#ecf0f1"
      ),
      margin = list(l = 80, r = 80, t = 60, b = 40),
      height = 320,
      paper_bgcolor = "#ffffff",
      plot_bgcolor = "#ffffff"
    )
}


# Interactive Leaflet Map
#---------------------------------------------------------------------------
create_leaflet_map <- function(data) {
  # Sample data for performance
  map_data <- data %>%
    sample_n(min(2000, nrow(data)))
  
  # Create color palette
  pal <- colorFactor(
    palette = city_colors,
    domain = map_data$city,
    reverse = FALSE
  )
  
  # Create popup content
  popup_content <- paste0(
    "<div style='font-family: Arial, sans-serif; padding: 5px;'>",
    "<b style='font-size: 14px; color: #2c3e50;'>", map_data$city, "</b><br>",
    "<hr style='margin: 5px 0;'>",
    "<b>Neighborhood:</b> ", map_data$neighbourhood, "<br>",
    "<b>Room Type:</b> ", map_data$room_type, "<br>",
    "<b style='color: #27ae60;'>Price: $", format(round(map_data$price, 0), big.mark = ","), "</b><br>",
    "<b>Reviews:</b> ", format(map_data$number_of_reviews, big.mark = ","), "<br>",
    "<b>Availability:</b> ", round(map_data$availability_365, 0), " days/year<br>",
    "</div>"
  )
  
  leaflet(map_data) %>%
    addProviderTiles(
      providers$CartoDB.Positron,
      options = providerTileOptions(minZoom = 1, maxZoom = 18)
    ) %>%
    addCircleMarkers(
      lng = ~longitude,
      lat = ~latitude,
      radius = 5,
      color = ~pal(city),
      fillColor = ~pal(city),
      fillOpacity = 0.7,
      weight = 1,
      opacity = 0.8,
      popup = popup_content,
      label = ~paste0(city, ": ", format_currency(price)),
      labelOptions = labelOptions(
        style = list(
          "font-family" = "Arial, sans-serif",
          "font-size" = "11px",
          "padding" = "3px 6px"
        )
      ),
      clusterOptions = markerClusterOptions(
        spiderfyOnMaxZoom = TRUE,
        showCoverageOnHover = TRUE,
        zoomToBoundsOnClick = TRUE
      )
    ) %>%
    addLegend(
      position = "bottomright",
      pal = pal,
      values = ~city,
      title = "Cities",
      opacity = 0.8,
      labFormat = labelFormat(
        transform = function(x) sort(unique(map_data$city))
      )
    ) %>%
    addResetMapButton() %>%
    setView(lng = 0, lat = 20, zoom = 2)
}


# USER INTERFACE
# ============================================================================

ui <- dashboardPage(
  skin = "blue",
  
  # Header -----------------------------------------------------------------
  dashboardHeader(
    title = "Airbnb Tourist Analytics",
    titleWidth = 350
  ),
  
  # Sidebar ----------------------------------------------------------------
  dashboardSidebar(
    width = 280,
    sidebarMenu(
      id = "sidebar",
      menuItem("Overview", tabName = "overview", 
               icon = icon("globe-americas")),
      menuItem("Pricing Analysis", tabName = "pricing", 
               icon = icon("chart-bar")),
      menuItem("Availability & Value", tabName = "availability", 
               icon = icon("calendar-check")),
      menuItem("Tourist Planning", tabName = "planning", 
               icon = icon("suitcase")),
      menuItem("About", tabName = "about", 
               icon = icon("info-circle"))
    ),
    
    hr(style = "border-color: #3c8dbc;"),
    
    # Filter Panel
    tags$div(
      style = "padding: 15px; color: #ecf0f1;",
      
      h5(icon("filter"), "FILTERS", 
         style = "font-weight: bold; margin-top: 0; margin-bottom: 20px;"),
      
      selectInput("filter_city", 
                  label = tags$span(icon("city"), "Cities"),
                  choices = sort(unique(airbnb_final$city)),
                  selected = sort(unique(airbnb_final$city)),
                  multiple = TRUE,
                  selectize = TRUE,
                  width = "100%"),
      
      selectInput("filter_room",
                  label = tags$span(icon("bed"), "Room Types"),
                  choices = sort(unique(airbnb_final$room_type)),
                  selected = sort(unique(airbnb_final$room_type)),
                  multiple = TRUE,
                  selectize = TRUE,
                  width = "100%"),
      
      sliderInput("filter_price",
                  label = tags$span(icon("dollar-sign"), "Price Range"),
                  min = 0,
                  max = 10000,
                  value = c(0, 500),
                  step = 25,
                  pre = "$",
                  width = "100%"),
      
      sliderInput("filter_reviews",
                  label = tags$span(icon("star"), "Minimum Reviews"),
                  min = 0,
                  max = 100,
                  value = 0,
                  step = 5,
                  width = "100%"),
      
      hr(style = "border-color: #3c8dbc;"),
      
      actionButton("reset_filters",
                   label = "Reset All Filters",
                   icon = icon("redo"),
                   class = "btn-primary",
                   style = "width: 85%; margin-bottom: 10px;"),
      
      div(
        style = "text-align: center; margin-top: 20px; color: #b8c7ce;",
        tags$small("Data source: Airbnb & Numbeo")
      )
    )
  ),
  
  # Body -------------------------------------------------------------------
  dashboardBody(
    
    # Custom CSS
    tags$head(
      tags$style(HTML("
        /* Import Google Font */
        @import url('https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600&display=swap');
        
        /* Global Styles */
        * {
          font-family: 'Inter', sans-serif;
        }
        
        .content-wrapper, .right-side {
          background-color: #f5f7fa;
        }
        
        /* Box Styling */
        .box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.04);
          border: none;
          margin-bottom: 20px;
        }
        
        .box-header {
          background-color: #ffffff;
          border-bottom: 1px solid #ecf0f1;
          border-radius: 8px 8px 0 0;
          padding: 12px 20px;
        }
        
        .box-title {
          font-weight: 600;
          font-size: 14px;
          color: #2c3e50;
        }
        
        .box-body {
          background-color: #ffffff;
          border-radius: 0 0 8px 8px;
          padding: 15px;
        }
        
        /* Value Boxes */
        .small-box {
          border-radius: 8px;
          box-shadow: 0 2px 4px rgba(0,0,0,0.04);
          margin-bottom: 20px;
        }
        
        .small-box h3 {
          font-weight: 700;
          font-size: 28px;
        }
        
        .small-box p {
          font-size: 13px;
          font-weight: 500;
        }
        
        /* Leaflet Map */
        .leaflet-container {
          border-radius: 8px;
        }
        
        /* DataTable */
        .dataTables_wrapper {
          font-family: 'Inter', sans-serif;
        }
        
        table.dataTable {
          border-collapse: collapse !important;
        }
        
        table.dataTable thead th {
          background-color: #f8f9fa;
          color: #2c3e50;
          font-weight: 600;
          font-size: 12px;
          border-bottom: 2px solid #3498db;
        }
        
        table.dataTable tbody td {
          font-size: 12px;
          padding: 8px 12px;
        }
        
        /* Sidebar */
        .main-sidebar {
          background-color: #2c3e50;
        }
        
        .sidebar-menu > li > a {
          font-weight: 500;
          padding: 14px 15px;
        }
        
        .sidebar-menu > li.active > a {
          background-color: #3498db;
        }
        
        /* Filter Panel */
        .selectize-input {
          border-radius: 4px;
          border: 1px solid #34495e;
          background-color: #34495e;
          color: #ecf0f1;
        }
        
        .selectize-dropdown {
          border-radius: 4px;
          border: 1px solid #34495e;
        }
        
        .irs-single, .irs-bar, .irs-bar-edge {
          background-color: #3498db;
        }
        
        /* Responsive */
        @media (max-width: 768px) {
          .small-box h3 {
            font-size: 22px;
          }
          .leaflet-container {
            height: 250px !important;
          }
        }
      "))
    ),
    
    # KPI Row
    fluidRow(
      valueBoxOutput("total_listings_box", width = 3),
      valueBoxOutput("avg_price_box", width = 3),
      valueBoxOutput("avg_cost_living_box", width = 3),
      valueBoxOutput("avg_availability_box", width = 3)
    ),
    
    # Tab Content
    tabItems(
      
      #------------------- Overview Tab -------------------#
      tabItem(
        tabName = "overview",
        
        fluidRow(
          box(
            width = 12,
            title = tags$span(icon("map-marked-alt"), " Distribution of Airbnb Listings"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            leafletOutput("leaflet_map", height = "400px")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = tags$span(icon("chart-pie"), " Room Type Distribution"),
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            plotlyOutput("room_distribution_plot", height = "350px")
          ),
          box(
            width = 6,
            title = tags$span(icon("building"), " Top 10 Neighborhoods"),
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            plotlyOutput("neighborhoods_plot", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = tags$span(icon("calendar"), " Availability Patterns by City and Room Type"),
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            plotlyOutput("room_availability_heatmap", height = "350px")
          )
        )
      ),
      
      #------------------- Pricing Tab -------------------#
      tabItem(
        tabName = "pricing",
        
        fluidRow(
          box(
            width = 6,
            title = tags$span(icon("city"), " Average Nightly Rate by City"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            plotlyOutput("price_city_plot", height = "320px")
          ),
          box(
            width = 6,
            title = tags$span(icon("bed"), " Average Rate by Room Type"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            plotlyOutput("price_room_plot", height = "320px")
          )
        ),
        
        fluidRow(
          box(
            width = 6,
            title = tags$span(icon("chart-line"), " Price Distribution"),
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            plotlyOutput("price_distribution_plot", height = "320px")
          ),
          box(
            width = 6,
            title = tags$span(icon("comments"), " Price vs. Review Count"),
            status = "success",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            plotlyOutput("price_reviews_plot", height = "320px")
          )
        )
      ),
      
      #------------------- Availability & Value Tab -------------------#
      tabItem(
        tabName = "availability",
        
        fluidRow(
          box(
            width = 6,
            title = tags$span(icon("calendar-alt"), " Average Yearly Availability"),
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            plotlyOutput("availability_city_plot", height = "320px")
          ),
          box(
            width = 6,
            title = tags$span(icon("balance-scale"), " Value for Money Analysis"),
            status = "warning",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            plotlyOutput("value_analysis_plot", height = "320px")
          )
        )
      ),
      
      #------------------- Tourist Planning Tab -------------------#
      tabItem(
        tabName = "planning",
        
        fluidRow(
          box(
            width = 12,
            title = tags$span(icon("wallet"), " Complete Trip Cost Comparison"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = FALSE,
            plotlyOutput("trip_cost_plot", height = "350px")
          )
        ),
        
        fluidRow(
          box(
            width = 12,
            title = tags$span(icon("table"), " Comprehensive Daily Budget Guide"),
            status = "info",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            DTOutput("tourist_budget_table")
          )
        )
      ),
      
      #------------------- About Tab -------------------#
      tabItem(
        tabName = "about",
        
        fluidRow(
          box(
            width = 12,
            title = tags$span(icon("info-circle"), " About This Dashboard"),
            status = "primary",
            solidHeader = TRUE,
            collapsible = TRUE,
            collapsed = FALSE,
            
            h4("Project Overview"),
            p("This interactive dashboard provides comprehensive analysis of Airbnb listings across six major tourist destinations worldwide. 
              It integrates accommodation data with local cost of living metrics to help travelers make informed decisions."),
            
            hr(),
            
            h4("Key Features"),
            tags$ul(
              tags$li(tags$b("Interactive Map:") , " Explore listing locations with clustering and detailed popups"),
              tags$li(tags$b("Price Analysis:") , " Compare rates across cities and accommodation types"),
              tags$li(tags$b("Availability Patterns:") , " Understand seasonal availability and booking windows"),
              tags$li(tags$b("Value Assessment:") , " Identify best value destinations using price-to-review ratios"),
              tags$li(tags$b("Budget Planning:") , " Calculate complete trip costs including accommodation and living expenses"),
              tags$li(tags$b("Real-time Filtering:") , " Dynamic updates across all visualizations")
            ),
            
            hr(),
            
            h4("Cities Analyzed"),
            fluidRow(
              column(4, tags$ul(
                tags$li(tags$b("Austin"), " - United States"),
                tags$li(tags$b("Bangkok"), " - Thailand"),
                tags$li(tags$b("Buenos Aires"), " - Argentina")
              )),
              column(4, tags$ul(
                tags$li(tags$b("Cape Town"), " - South Africa"),
                tags$li(tags$b("Istanbul"), " - Turkey"),
                tags$li(tags$b("Melbourne"), " - Australia")
              ))
            ),
            
            hr(),
            
            h4("Data Sources"),
            tags$ul(
              tags$li(tags$b("Airbnb Listings:") , " Public dataset containing property details, prices, reviews, and availability"),
              tags$li(tags$b("Cost of Living:") , " Numbeo API - Daily budget estimates including meals, transportation, and incidentals"),
              tags$li(tags$b("Geospatial Data:") , " Latitude/longitude coordinates for property locations")
            ),
            
            hr(),
            
            h4("Project Team"),
            p(tags$b("Course:"), " ALY 6070 - Communication and Visualization for Data Analytics"),
            p(tags$b("Institution:"), " Northeastern University"),
            p(tags$b("Instructor:"), " Dr. Maria Ayala"),
            br(),
            fluidRow(
              column(6,
                     h5("Team Members:"),
                     tags$ul(
                       tags$li("Kwabena Duffuor Asante - ", 
                               tags$a("kduffuor.github.io", 
                                      href = "https://kduffuor.github.io", 
                                      target = "_blank")),
                       tags$li("Audrey Appraku"),
                       tags$li("Usha Rani Nallaparaju"),
                       tags$li("Gifty Afful")
                     )
              ),
              column(6,
                     h5("Technologies Used:"),
                     tags$ul(
                       tags$li("R Shiny - Dashboard Framework"),
                       tags$li("Leaflet - Interactive Mapping"),
                       tags$li("Plotly - Dynamic Visualizations"),
                       tags$li("DT - Data Tables"),
                       tags$li("Tidyverse - Data Processing")
                     )
              )
            ),
            
            hr(),
            
            div(
              style = "text-align: center; color: #7f8c8d;",
              p("© 2026 Airbnb Tourist Analytics Dashboard"),
              p(tags$small("Last Updated: February 2026"))
            )
          )
        )
      )
    )
  )
)


# SERVER LOGIC
# ============================================================================

server <- function(input, output, session) {
  
  
  # Reactive filtered dataset
  #-----------------------------------------------------------------------
  filtered_data <- reactive({
    req(
      input$filter_city,
      input$filter_room,
      input$filter_price,
      input$filter_reviews
    )
    
    airbnb_final %>%
      filter(
        city %in% input$filter_city,
        room_type %in% input$filter_room,
        price >= input$filter_price[1],
        price <= input$filter_price[2],
        number_of_reviews >= input$filter_reviews
      )
  })
  
  
  # Reset filters
  #-----------------------------------------------------------------------
  observeEvent(input$reset_filters, {
    updateSelectInput(session, "filter_city",
                      selected = sort(unique(airbnb_final$city)))
    updateSelectInput(session, "filter_room",
                      selected = sort(unique(airbnb_final$room_type)))
    updateSliderInput(session, "filter_price",
                      value = c(0, 500))
    updateSliderInput(session, "filter_reviews",
                      value = 0)
  })
  
  
  # KPI Value Boxes
  #-----------------------------------------------------------------------
  output$total_listings_box <- renderValueBox({
    df <- filtered_data()
    valueBox(
      value = format_number(nrow(df)),
      subtitle = "Total Listings",
      icon = icon("home"),
      color = "blue"
    )
  })
  
  output$avg_price_box <- renderValueBox({
    df <- filtered_data()
    valueBox(
      value = format_currency(mean(df$price, na.rm = TRUE)),
      subtitle = "Average Nightly Rate",
      icon = icon("dollar-sign"),
      color = "green"
    )
  })
  
  output$avg_cost_living_box <- renderValueBox({
    df <- filtered_data()
    valueBox(
      value = format_currency(mean(df$daily_budget_usd, na.rm = TRUE)),
      subtitle = "Average Daily Living Cost",
      icon = icon("utensils"),
      color = "yellow"
    )
  })
  
  output$avg_availability_box <- renderValueBox({
    df <- filtered_data()
    valueBox(
      value = paste0(round(mean(df$availability_365, na.rm = TRUE), 0), " days"),
      subtitle = "Average Yearly Availability",
      icon = icon("calendar"),
      color = "purple"
    )
  })
  
  
  # Chart Outputs
  #-----------------------------------------------------------------------
  output$leaflet_map <- renderLeaflet({
    create_leaflet_map(filtered_data())
  })
  
  output$room_distribution_plot <- renderPlotly({
    create_room_type_distribution(filtered_data())
  })
  
  output$neighborhoods_plot <- renderPlotly({
    create_top_neighborhoods(filtered_data())
  })
  
  output$room_availability_heatmap <- renderPlotly({
    create_room_availability_heatmap(filtered_data())
  })
  
  output$price_city_plot <- renderPlotly({
    create_price_by_city(filtered_data())
  })
  
  output$price_room_plot <- renderPlotly({
    create_price_by_room_type(filtered_data())
  })
  
  output$price_distribution_plot <- renderPlotly({
    create_price_distribution(filtered_data())
  })
  
  output$price_reviews_plot <- renderPlotly({
    create_price_vs_reviews(filtered_data())
  })
  
  output$availability_city_plot <- renderPlotly({
    create_availability_by_city(filtered_data())
  })
  
  output$value_analysis_plot <- renderPlotly({
    create_value_analysis(filtered_data())
  })
  
  output$trip_cost_plot <- renderPlotly({
    create_total_trip_cost(filtered_data())
  })
  
  
  # Tourist Budget Table
  #-----------------------------------------------------------------------
  output$tourist_budget_table <- renderDT({
    tourist_data <- create_tourist_budget_table(filtered_data())
    
    datatable(
      tourist_data,
      colnames = c(
        "City",
        "Room Type",
        "Avg (USD)",
        "Median (USD)",
        "Daily Living (USD)",
        "Avg Meal (USD)",
        "Total Daily (USD)",
        "Listings"
      ),
      options = list(
        pageLength = 12,
        scrollX = TRUE,
        dom = 'Bfrtip',
        buttons = list(
          list(extend = 'copy', text = 'Copy Table'),
          list(extend = 'csv', text = 'Download CSV'),
          list(extend = 'excel', text = 'Download Excel')
        ),
        order = list(list(7, 'asc')),
        columnDefs = list(
          list(className = 'dt-center', targets = '_all')
        ),
        initComplete = JS(
          "function(settings, json) {",
          "$(this.api().table().header()).css({'background-color': '#f8f9fa', 'color': '#2c3e50'});",
          "}"
        )
      ),
      rownames = FALSE,
      class = 'cell-border stripe hover display nowrap',
      filter = 'top'
    ) %>%
      formatCurrency(
        columns = c(3, 4, 5, 6, 7),
        currency = "$",
        digits = 2,
        interval = 3,
        mark = ","
      ) %>%
      formatRound(columns = 8, digits = 0) %>%
      formatStyle(
        columns = 7,
        background = styleColorBar(tourist_data$total_daily_budget, 'rgba(52, 152, 219, 0.2)'),
        fontWeight = 'bold'
      )
  })
}


# RUN APPLICATION
# ============================================================================

shinyApp(ui = ui, server = server)