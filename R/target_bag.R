library(dplyr)
library(DT)


targetBag_ui <- function(id) {
  ns <- NS(id)
  card(
    height = "600px",
    card_header(
      class = "bg-primary bg-opacity-25 text-black",
      "Selected Genotypes Detail"
    ),
    card_body(
      DTOutput(ns("targetBagIndData"))
    )
  )
}

targetBag_server <- function(id, marker_info, ind_data, bag) {
  moduleServer(id, function(input, output, session) {
    group_data <- reactive({
      gruped_df <- bag$bag %>% 
        group_by(Accesion) %>% 
        mutate(concatenated = paste(reason, collapse = ", "))
    })
    output$targetBagIndData <- renderDT(
      {
        group_data()
      },
      options = list(pageLength = 10)
    )
  })
}
