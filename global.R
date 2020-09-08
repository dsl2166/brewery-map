library(httr)
library(jsonlite)
library(dplyr)
library(leaflet)
library(knitr)
library(kableExtra)

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

# leaflet map
brewery_data <- brewery_data %>%
  mutate(popup = paste("Brewery Name:", name, "</br>",
                       "Phone:", as.character(phone), "</br>",
                       "Website:", paste0("<a href = ", "'", website_url, "'", ">", website_url, "</a>" )))

labs <- as.list(brewery_data$popup)

map <- brewery_data %>%
  filter(!is.na(longitude)) %>%
  leaflet() %>%
  addProviderTiles(provider = providers$CartoDB.Voyager) %>%
  addCircleMarkers(
    lng = ~longitude, 
    lat = ~latitude, 
    label = lapply(labs, HTML),
    color = "#FF5733",
    fillOpacity = 0.7,
    clusterOptions = markerClusterOptions()
  )

# brewery states for dropdown menu
filtered_data <- brewery_data %>% 
  filter(state != "FL San" & !is.na(state))
brewery_states <- unique(filtered_data$state)

# number of breweries
get_num_breweries <- function(us_state = "All States") {
  if (us_state == "All States") {
    data <- filtered_data
  } else {
    data <- filtered_data %>%
      filter(state == us_state)
  }
  as.character(nrow(data))
}

# number of breweries by type table
brewery_type_table <- function(us_state = "All States") {
  if (us_state == "All States") {
    data <- filtered_data %>%
      count(brewery_type, sort = TRUE)
  } else {
    data <- filtered_data %>%
      filter(state == us_state) %>%
      count(brewery_type, sort = TRUE)
  }
  
  data %>% rename("#" = n, "Brewery Type" = brewery_type) %>%
    kable() %>% 
    kable_styling(full_width = FALSE, position = "left")
}

# top 3 cities table
top3_table <- function(us_state = "All States") {
  if (us_state == "All States") {
    data <- filtered_data %>%
      count(city, sort = TRUE) %>%
      head(3)
  } else {
    data <- filtered_data %>%
      filter(state == us_state) %>%
      count(city, sort = TRUE) %>%
      head(3)
  }
  
  data %>% rename("#" = n, "Top 3 Cities" = city) %>%
    kable() %>% 
    kable_styling(full_width = FALSE, position = "left")
}

# centroids
centroid_all <- filtered_data %>%
  summarize(center_long = mean(longitude, na.rm = TRUE),
            center_lat = mean(latitude, na.rm = TRUE)) %>%
  mutate(state = "All States")

zoom_data <- filtered_data %>%
  group_by(state) %>%
  summarize(center_long = mean(longitude, na.rm = TRUE),
            center_lat = mean(latitude, na.rm = TRUE)) %>%
  bind_rows(centroid_all) %>%
  mutate(zoom_level = ifelse(state == "All States", 3, 7))
