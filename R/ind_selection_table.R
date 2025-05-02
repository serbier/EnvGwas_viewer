library(dplyr)
library(DT)


selectedIndTable_ui <- function(id){
 ns <- NS(id) 
 card(
    height="600px",
    card_header(class = 'bg-primary bg-opacity-25 text-black',
      "Genotype Detail"),
    card_body(
      DTOutput(ns("selectedIndData"))
    ))
  
}

selectedIndTable_server <- function(id, ind_data, qtn_id, designation = "Accesion"){
  moduleServer(id, function(input, output, session){
    
    output$selectedIndData <- renderDT({
      selected <- event_data("plotly_selected", source = qtn_id)
      if (is.null(selected)) return(NULL)
      ind_data %>% filter(!!sym(designation) %in% selected$key)
    }, options = list(pageLength = 10))
  })
}