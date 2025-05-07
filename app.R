library(readxl)
library(dplyr)
library(shiny)
library(bslib)
library(DT)
library(plotly)
library(magrittr)

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
    ),
    selectInput(
      inputId = "select_trait",
      label = "Select Trait",
      choices = NULL
    )
  ),
  page_fluid(uiOutput("main_area"))
)

server <- function(input, output, session) {
  data_dir <- "data/"

  target_bag_data <- reactiveValues(
    bag = data.frame()
  )
  observe({
    files <- list.files(path = data_dir, pattern = "\\.xlsx$", full.names = FALSE)
    updateSelectInput(session, "select_dataset", choices = files)
  })

  read_data <- reactive({
    req(input$select_dataset)
    req(input$select_genepool)
    file <- glue::glue("{data_dir}/{input$select_dataset}")
    read_excel(path = file, sheet = "Marker.info") %>%
      filter(Subset == input$select_genepool)
  })

  read_data_trait <- reactive({
    req(read_data())
    req(input$select_trait)
    read_data() %>% filter(variable == input$select_trait)
  })

  read_gt_data <- reactive({
    req(read_data())
    req(input$select_genepool)
    file <- glue::glue("{data_dir}/{input$select_dataset}")
    df <- read_excel(path = file, sheet = input$select_genepool)
    df
  })

  observe({
    req(read_data())
    req(input$select_genepool)

    marker_info <- read_data()
    updateSelectInput(session,
      "select_trait",
      choices = marker_info$variable
    )
  })

  observeEvent(input$select_genepool, {
    print("Erase bag")
    data <- read_data()
    for (plot_id in data$AlleleID) {
      plotlyProxy(glue::glue("{plot_id}_Imp"), session) %>%
        plotlyProxyInvoke("restyle", list(selectedpoints = list(NULL)))
      print(glue::glue("{plot_id}_Imp"))
      selected <- event_data("plotly_selected", source = glue::glue("{plot_id}_Imp"))
      print(selected)
    }
    target_bag_data$bag <- data.frame()
  })

  output$main_area <- renderUI({
    req(read_data_trait())
    trait_df <- read_data_trait()
    gt_df <- read_gt_data()

    tabs <- lapply(trait_df$qtn_id, function(qtn_name) {
      i_boxplots <- allelicEffectPlot_ui(glue::glue("bxpl-{qtn_name}"))
      i_tables <- selectedIndTable_ui(glue::glue("table-{qtn_name}"))
      i_maps <- selectedIndDist_ui(glue::glue("map-{qtn_name}"))

      nav_panel(
        title = qtn_name,
        layout_column_wrap(
          width = 1,
          i_boxplots,
          i_tables,
          i_maps
        )
      )
    })
    navset_card_tab(
      nav_panel(
        title = "QTNs",
        navset_card_tab(
          !!!tabs
        )
      ),
      nav_panel(
        title = "Target Bag",
        targetBag_ui("target_bag")
      )
    )
  })

  observe({
    req(read_data_trait())
    req(read_gt_data())

    m_info_df <- read_data_trait()
    m_gt_df <- read_gt_data()

    lapply(m_info_df$qtn_id, function(iqtn) {
      sdf <- m_info_df %>% filter(qtn_id == iqtn)

      m_info <- get_qtl_info(m_info_df, sdf$AlleleID)
      gt_data <- get_qtl_sample_data(m_gt_df, sdf$AlleleID)

      allelicEffectPlot_server(
        id = glue::glue("bxpl-{iqtn}"),
        marker_info = m_info,
        ind_data = gt_data,
        response = sdf$variable,
        GT = paste0(sdf$AlleleID, "_Imp")
      )

      selectedIndTable_server(
        id = glue::glue("table-{iqtn}"),
        ind_data = gt_data,
        qtn_id = paste0(sdf$AlleleID, "_Imp"),
        bag = target_bag_data
      )

      selectedIndDist_server(
        id = glue::glue("map-{iqtn}"),
        ind_data = gt_data,
        qtn_id = paste0(sdf$AlleleID, "_Imp")
      )
    })
  })

  observe({
    req(read_data_trait())
    req(read_gt_data())

    m_info_df <- read_data_trait()
    m_gt_df <- read_gt_data()
    targetBag_server("target_bag",
      marker_info = m_info_df,
      ind_data = m_gt_df,
      bag = target_bag_data
    )
  })
}

shinyApp(ui, server)
