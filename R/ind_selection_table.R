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

selectedIndTable_server <- function(id, ind_data, qtn_id, bag, designation = "Accesion") {
  moduleServer(id, function(input, output, session) {
    get_data <- reactive({
      selected <- event_data("plotly_selected", source = qtn_id)
      selected_inds <- ind_data %>%
        filter(!!sym(designation) %in% selected$key) %>%
        mutate(reason = qtn_id)
      selected_inds
    })

    observeEvent(
      event_data("plotly_selected", source = qtn_id),
      {
        selected <- event_data("plotly_selected", source = qtn_id)
        print(qtn_id)
        print(selected)
        selected_inds <- ind_data %>%
          filter(!!sym(designation) %in% selected$key) %>%
          mutate(reason = qtn_id)

        if (dim(bag$bag)[2] > 0) {
          newbag <- bag$bag %>% filter(reason != qtn_id)
          bag$bag <- rbind(newbag, selected_inds)
        } else {
          print("flag")
          print(bag$bag)
          bag$bag <- selected_inds
        }


        output$selectedIndData <- renderDT(
          {
            selected_inds
          },
          options = list(pageLength = 10)
        )
      },
      ignoreNULL = TRUE
    )
  })
}

