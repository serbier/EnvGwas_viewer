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
  page_fluid(uiOutput("main_area"))
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
    tabs <- lapply(trait_group$variable, function(trait) {
      # Get the qtl info data for each trait
      target_qtls <- read_data() %>%
        filter(variable == trait)

      i_boxplots <- lapply(target_qtls$qtn_id, function(y) {
        allelicEffectPlot_ui(glue::glue("bxpl-{y}"))
      })
      nav_panel(
        title = trait,
        layout_column_wrap(
          width = 1 / 2,
          !!!i_boxplots
        )
      )
    })
    navset_card_tab(
      title = "Traits",
      !!!tabs
    )
  })

  observe({
    req(read_data())
    req(read_gt_data())
    trait_data <- read_data()
    gt_data <- read_gt_data()
    lapply(trait_data$qtn_id, function(iqtn) {
      i_metadata <- read_data() %>%
        filter(qtn_id == iqtn)
      i_gt <- gt_data %>%
        select(Accesion, !!sym(i_metadata$AlleleID), !!sym(i_metadata$variable))

      allelicEffectPlot_server(
        glue::glue("bxpl-{iqtn}"),
        i_gt,
        i_metadata$AlleleID,
        "Accesion",
        i_metadata$variable,
        i_metadata$`Phenotype_Variance_Explained(%)`,
        -log10(i_metadata$`BLINK P value (Boferroni adjusted)`),
        i_metadata$Favorable.allele,
        i_metadata$effect,
        i_metadata$maf2
      )
    })
  })
}

shinyApp(ui, server)
