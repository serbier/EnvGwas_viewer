library(dplyr)
library(DT)


selectedIndTable_ui <- function(id) {
  ns <- NS(id)
  card(
    height = "600px",
    card_header(
      class = "bg-primary bg-opacity-25 text-black",
      "Genotype Detail"
    ),
    card_body(
      DTOutput(ns("selectedIndData"))
    )
  )
}

selectedIndTable_server <- function(id, marker_info, ind_data, qtn_id, bag, designation = "Accesion") {
  moduleServer(id, function(input, output, session) {
    selected_inds <- reactiveVal(NULL)
    
    get_data <- reactive({
      selected <- event_data("plotly_selected", source = qtn_id)
      selected_inds_df <- ind_data %>%
        filter(!!sym(designation) %in% selected$key) %>%
        mutate(reason = marker_info$qtn_id)
      selected_inds_df
    })
    output$selectedIndData <- renderDataTable({
        req(get_data())
        selected_inds(get_data())
        datatable(selected_inds())
      },
      options = list(pageLength = 10),
      server = FALSE
    )

      ##event_data("plotly_selected", source = qtn_id),
    observeEvent(selected_inds(),{
      if (dim(bag$bag)[2] > 0) {
        newbag <- bag$bag %>% filter(reason != marker_info$qtn_id)
        bag$bag <- rbind(newbag, selected_inds())
      } else {
        bag$bag <- selected_inds()
      }
    }, ignoreInit = TRUE)
  })
}

