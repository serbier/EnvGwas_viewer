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
      fluidRow(
        inputPanel(
          selectInput(ns("selectBagCols"),
            label = "Select Columns to display",
            choices = c(),
            multiple = TRUE
          )
        ),
        DTOutput(ns("targetBagIndData")),
        downloadButton(ns("download_bag"))
      )
    )
  )
}

targetBag_server <- function(id, marker_info, ind_data, bag) {
  moduleServer(id, function(input, output, session) {
    observeEvent(bag$bag, {
      updateSelectInput(session, "selectBagCols", choices = colnames(bag$bag))
    })


    group_data <- reactive({
      gruped_df <- bag$bag %>%
        group_by(Accesion) %>%
        mutate(concatenated = paste(reason, collapse = ", ")) %>%
        slice_head(n = 1)
    })
    output$targetBagIndData <- renderDataTable(
      {
        datatable(group_data() %>% select(input$selectBagCols))
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
