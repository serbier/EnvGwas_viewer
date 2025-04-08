library(readxl)
library(dplyr)
library(shiny)
library(bslib)
library(DT)
library(plotly)

ui <- page_sidebar(
  title = "EnvGWAS V1.0",
  sidebar = sidebar(
    title = "Control Panel",
    selectInput(
      inputId = "select_dataset",
      label = "Select a dataset",
      choices = NULL
    ),
    selectInput(
      inputId = "select_genepool",
      label = "Select a Genepool",
      choices = c("Mesoamerican", "Andean")
    )
  ),
  uiOutput("main_area")
)

server <- function(input, output, session) {
  data_dir <- "data/"

  observe({
    files <- list.files(path = data_dir, pattern = "\\.xlsx$", full.names = FALSE)
    updateSelectInput(session, "select_dataset", choices = files)
  })

  read_data <- reactive({
    req(input$select_dataset)
    req(input$select_genepool)
    file <- glue::glue("{data_dir}/{input$select_dataset}")
    df <- read_excel(path = file, sheet = "Marker info") %>%
      filter(Subset == input$select_genepool)
    df
  })
  read_gt_data <- reactive({
    req(read_data())
    req(input$select_genepool)
    file <- glue::glue("{data_dir}/{input$select_dataset}")
    df <- read_excel(path = file, sheet = input$select_genepool)
    df
  })

  output$main_area <- renderUI({
    req(read_data())
    trait_group <- read_data() %>%
      group_by(variable) %>%
      summarize(count = n())

    gt_df <- read_gt_data()
    tabs <- lapply(trait_group$variable, function(x) {
      target_qtls <- read_data() %>%
        filter(variable == x)
      target_gt <- read_gt_data()
      i_data <- lapply(target_qtls$qtn_id, function(y) {
        i_metadata <- target_qtls %>%
          filter(qtn_id == y)
        i_gt <- target_gt %>%
          select(Accesion, !!sym(x), !!sym(y))
        list(
          pev = i_metadata$`Phenotype_Variance_Explained(%)`,
          lod = -log10(i_metadata$`BLINK P value (Boferroni adjusted)`),
          fa = i_metadata$Favorable.allele,
          eff = i_metadata$effect,
          maf = i_metadata$maf2,
          gt = i_gt
        )
      })

      nav_panel(
        x,
        layout_column_wrap()
      )
    })


    navset_card_tab(
      title = "Traits",
      !!!tabs
    )
  })
}

shinyApp(ui, server)

