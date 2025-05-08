library(dplyr)
library(DT)


selectionBag_ui <- function(id) {
  ns <- NS(id)
  card(
    height = "600px",
    card_header(
      class = "bg-primary bg-opacity-25 text-black",
      "Subset Selection"
    ),
    card_body(
      
    )
  )
}

selectionBag_server <- function(id, bag) {
  moduleServer(id, function(input, output, session) {
    
  })
}
