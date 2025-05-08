library(readxl)
library(dplyr)
library(shiny)
library(bslib)
library(DT)
library(plotly)
library(magrittr)

source("./R/data_model.R")

allelicEffectPlot_ui <- function(id) {
  ns <- NS(id)
  card(
    height = "600px",
    card_header(
      class = "bg-primary bg-opacity-25 text-black",
      "QTN Metrics"
    ),
    card_body(
      min_height = "400px",
      layout_column_wrap(
        min_height = "200px",
        value_box(
          title = "PEV",
          value = textOutput(ns("pevValue")),
          theme = "green"
        ),
        value_box(
          title = "LOD",
          value = textOutput(ns("lodValue")),
        ),
        value_box(
          title = "Favorable Allele",
          value = textOutput(ns("faValue")),
          theme = "blue"
        ),
        value_box(
          title = "Effect",
          value = textOutput(ns("effValue")),
        ),
        value_box(
          title = "MAF",
          value = textOutput(ns("mafValue")),
        ),
      ),
      plotlyOutput(ns("allelicEffectBoxplot")),
    )
  )
}

allelicEffectPlot_server <- function(id, marker_info,
                                     ind_data,
                                     response,
                                     GT,
                                     designation = "Accesion") {
  moduleServer(id, function(input, output, session) {
    lod <- -log10(marker_info$p_value)
    output$pevValue <- renderText(glue::glue("{round(marker_info$PVE,1)}%"))
    output$lodValue <- renderText(glue::glue("{round(lod,1)}"))
    output$faValue <- renderText(glue::glue("{marker_info$Favorable.allele}"))
    output$effValue <- renderText(glue::glue("{round(marker_info$effect,3)}"))
    output$mafValue <- renderText(glue::glue("{round(marker_info$maf.Imputed*100,2)}%"))

    pre_process_data <- reactive({
      clean <- ind_data %>%
        filter(!is.na(!!sym(response))) %>%
        filter(!is.na(!!sym(designation)))
    })

    output$allelicEffectBoxplot <- renderPlotly({
      req(pre_process_data)
      Gcalls <- unlist(unique(pre_process_data()[, GT]))
      annotations <- lapply(seq(1, length(Gcalls)), function(GC) {
        GC <- Gcalls[GC]
        idf <- pre_process_data() %>% filter(!!sym(GT) == GC)
        list(
          x = GC,
          y = max(idf %>% pull(!!sym(response))) * 1.1,
          text = glue::glue("n = {dim(idf)[1]}"),
          showarrow = FALSE,
          font = list(size = 10)
        )
      })


      data <- pre_process_data()
      fig <- plot_ly(
        data = data,

        x = data[[GT]],
        y = data[[response]],
        color = data[["Imputed"]],
        type = "box",
        boxpoints = "all",
        source = GT,
        key = data[[designation]]
      ) %>%
        layout(
          xaxis = list(title = GT),
          yaxis = list(title = response),
          annotations = annotations
        )
      fig
    })
  })
}

allelicEffect_demo <- function() {
  # Load data
  DB <- "data/FFAR_selections_SNP.SilicoDArT_4_v2.xlsx"
  m_info_df <- read_excel(path = DB, sheet = "Marker.info")
  marker_id <- "SNP_118390666-47-T/C"
  m_info <- get_qtl_info(m_info_df, marker_id)
  m_gt_df <- read_excel(path = DB, sheet = "Andean")
  gt_data <- get_qtl_sample_data(m_gt_df, marker_id)

  ui <- fluidPage(allelicEffectPlot_ui("x"))
  server <- function(input, output, session) {
    allelicEffectPlot_server("x", m_info, gt_data,
      response = "climate.prec",
      designation = "Accesion",
      GT = paste0(marker_id, "_Imp")
    )
  }
  shinyApp(ui, server)
}

allelicEffect_demo()
