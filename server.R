server <- function(input, output) {
  # map
  output$brewery_map <- renderLeaflet({map})
  
  # number of breweries
  output$brewery_num <- renderText({
    paste("<strong>Total Number of Breweries: </strong>", get_num_breweries(input$states))
  })
  
  # brewery type table
  output$type_table <- renderText(
    brewery_type_table(input$states) 
  )
  
  # top 3 cities
  output$top3 <- renderText(
    top3_table(input$states)
  )
  
  # zoom functionality
  zooming <- reactive({
    subset(zoom_data, state == input$states)
  })

  # observe map for changing zoom
  observe({
    leafletProxy("brewery_map") %>%
      setView(lng = zooming()$center_long,
              lat = zooming()$center_lat,
              zoom = zooming()$zoom_level)
  })
  
}