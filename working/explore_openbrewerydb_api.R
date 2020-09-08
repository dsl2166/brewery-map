library(httr)
library(jsonlite)
library(dplyr)
library(tidyr)
library(leaflet)
library(ggplot2)

# get all the brewery data through the API
data <- list()
i = 1
while (TRUE) {
  res = GET("https://api.openbrewerydb.org/breweries",
            query = list(per_page = 50, page = i))
  data_from_api = fromJSON(rawToChar(res$content))
  if (length(data_from_api) == 0) {
    break
  }
  data[[i]] <- data_from_api
  i <- i + 1
}
brewery_data <- bind_rows(data) %>%
  mutate(longitude = as.numeric(longitude),
         latitude = as.numeric(latitude))

# check missing
brewery_data %>%
  summarize_all(funs(sum(is.na(.)))) %>%
  pivot_longer(everything(), names_to = "variables", values_to = "n_missing")

View(filter(brewery_data, is.na(longitude)))

# leaflet map
links = paste0("<a href = ", "'", brewery_data$website_url, "'", ">", brewery_data$website_url, "</a>" )
popup_text <- paste("Brewery Name:", brewery_data$name, "</br>",
                    "Phone:", brewery_data$phone, "</br>",
                    "Website:", links)

map <- leaflet(brewery_data) %>%
  addTiles() %>%
  addCircleMarkers(
    lng = ~longitude, 
    lat = ~latitude, 
    popup = popup_text,
    color = "#FF5733",
    fillOpacity = 0.7,
    clusterOptions = markerClusterOptions()
  )

# brewery states for dropdown menu
brewery_states <- brewery_data %>% 
  filter(state != "FL San" & !is.na(state))
brewery_states <- unique(brewery_states$state)

# function for creating % brewery type in state chart
create_brewery_type_chart <- function(us_state="All States") {
  if (us_state == "All States") {
    data <- brewery_data %>%
      count(brewery_type)
  } else {
    data <- brewery_data %>%
      filter(state == us_state) %>%
      count(brewery_type)
  }
  
  p <- ggplot(data, aes(x = reorder(brewery_type, n), y = n)) +
    geom_col() + 
    coord_flip() +
    labs(x = "", y = "") +
    theme_minimal()
  
  ggplotly(p)
}

# function for top 5 cities with most breweries chart
create_top5_chart <- function(us_state="All States") {
  if (us_state == "All States") {
    data <- brewery_data %>%
      filter(!is.na(city)) %>%
      count(city, sort = TRUE) %>%
      top_n(5)
  } else {
    data <- brewery_data %>%
      filter(state == us_state) %>%
      count(city, sort = TRUE) %>%
      head(5)
  }
  
  p <-  ggplot(data, aes(x = reorder(city, n), y = n)) +
    geom_col() +
    coord_flip() +
    labs(x = "", y = "") +
    theme_minimal()
  
  ggplotly(p)
}