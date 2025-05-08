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
      fluidRow(DTOutput(ns("targetBagIndData")),
               downloadButton(ns("download_bag"))
               )
    )
  )
}

targetBag_server <- function(id, marker_info, ind_data, bag) {
  moduleServer(id, function(input, output, session) {
    
    group_data <- reactive({
      gruped_df <- bag$bag %>%
        select(Accesion, country, characterization.seed.color,
               characterization.seed.shine, characterization.seed.shape,
               characterization.seed.100.weight,
               characterization.growth.habit, characterization.phaseolin,
               characterization.days.flowering, characterization.BCMV_EN,
               characterization.EMPOASCA_EN,
               climate.latitude, climate.longitude,
               favorable.counts.SNP_Imp, favorable.counts.SILICO_Imp, reason) %>% 
        group_by(Accesion) %>% 
        mutate(concatenated = paste(reason, collapse = ", ")) %>% 
        slice_head(n = 1)
      
    })
    output$targetBagIndData <- renderDataTable(
      {
        datatable(group_data())
      },
      options = list(pageLength = 10),
      server = FALSE
    )
    output$download_bag <- downloadHandler(
      filename = function() {
        paste0("selection_bag.csv")
      },
      content = function(file) {
        write.csv(group_data(), file)
      }
    )
    
  })
}
