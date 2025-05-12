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
      uiOutput(ns("selectedIndDataUI"))
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

    output$selectedIndDataUI <- renderUI({
      selected <- get_data()
      if (dim(selected)[1] > 0) {
        selected_inds(get_data())
        renderDataTable(
          {
            selected
          },
          options = list(pageLength = 10),
          server = FALSE
        )
      } else {
        tagList(h3("Please select genotypes from Boxplots"))
      }
    })

    output$selectedIndTab <- renderDataTable(
      {
        selected_inds(get_data())
      },
      options = list(pageLength = 10),
      server = FALSE
    )
    ## event_data("plotly_selected", source = qtn_id),
    observeEvent(selected_inds(),
      {
        if (dim(bag$bag)[2] > 0) {
          newbag <- bag$bag %>% filter(reason != marker_info$qtn_id)
          bag$bag <- rbind(newbag, selected_inds())
          bag$target_markers <- c(marker_info$AlleleID,bag$target_markers)
          
        } else {
          bag$bag <- selected_inds()
          bag$target_markers <- c(marker_info$AlleleID)
        }
      },
      ignoreInit = TRUE
    )
  })
}
