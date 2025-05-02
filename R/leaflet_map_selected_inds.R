
library(dplyr)
library(leaflet)


selectedIndDist_ui <- function(id){
 ns <- NS(id) 
 card(
    height="600px",
    card_header(class = 'bg-primary bg-opacity-25 text-black',
      "Geographical Distribution"),
    card_body(
      leafletOutput(ns('inds_distribution'))
    ))
}

selectedIndDist_server <- function(id, ind_data, qtn_id, designation = "Accesion"){
  moduleServer(id, function(input, output, session){
    
    output$inds_distribution <- renderLeaflet({
      selected <- event_data("plotly_selected", source = qtn_id)
      if (is.null(selected)) return(NULL)
      
      selected_inds <- ind_data %>% filter(!!sym(designation) %in% selected$key)
      
      leaflet(data = selected_inds) %>% 
        addMarkers(
          lng = ~climate.longitude,
          lat = ~climate.latitude,
          popup = ~Accesion
        ) %>% 
        addProviderTiles(providers$Esri.WorldTopoMap) 
    })
  })
}