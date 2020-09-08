ui <- fillPage(
  theme = "styles.css", 
  
  leafletOutput("brewery_map", height = "100%", width = "100%"),
      
  absolutePanel(
    id = "state_panel", 
    class = "panel panel-default", 
    top = 90, right = 10, left = 10, width = "20%", height = "70%", draggable = TRUE,
    
    h2("Brewery Map"),
    
    p("Credits to Chris J Mears and Wandering Leaf Studios LLC for the Open Brewery DB API."),
    
    selectInput("states", "Choose A State:", 
                choices = c("All States", brewery_states),
                width = "100%"),
    
    htmlOutput("brewery_num"),
    br(),
    
    htmlOutput("type_table"),
    htmlOutput("top3")
      
  )
)

