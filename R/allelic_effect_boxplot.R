library(readxl)
library(dplyr)
library(shiny)
library(bslib)
library(DT)
library(plotly)
library(magrittr)

allelicEffectPlot_ui <- function(id){
  ns <- NS(id)
  card(
    height="600px",
    card_header(class = 'bg-primary bg-opacity-25 text-black',
      textOutput(ns("card_head"))),
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
      plotlyOutput(ns("allelicEffectBoxplot"))
    )
  )
  
}

allelicEffectPlot_server <- function(id, qtlData, gtCol = "GT",
  designation = "Designation",
  response = "Response",
  pev,
  lod,
  fa,
  eff,
  maf
) {
  moduleServer(id, function(input, output, session){
    output$card_head <- renderText(gtCol)
    output$pevValue <- renderText(glue::glue("{round(pev,1)}%"))
    output$lodValue <- renderText(glue::glue("{round(lod,1)}"))
    output$faValue <- renderText(glue::glue("{fa}"))
    output$effValue <- renderText(glue::glue("{round(eff,3)}"))
    output$mafValue <- renderText(glue::glue("{round(maf*100,2)}%"))

    pre_process_data <- reactive({
      clean <- qtlData %>% 
        filter(!is.na(!!sym(response))) %>% 
        filter(!is.na(!!sym(designation)))
    })
    output$allelicEffectBoxplot <- renderPlotly({
      req(pre_process_data)
      
      Gcalls <- unlist(unique(pre_process_data()[,gtCol]))
      names(Gcalls) <- unlist(Gcalls)
      
      traces <- lapply(Gcalls, function(GC){
        pre_process_data() %>%  filter(!!sym(gtCol) == GC) %>% pull(!!sym(response))
      })
      text <- lapply(Gcalls, function(GC){
        pre_process_data() %>%  filter(!!sym(gtCol) == GC) %>% pull(!!sym(designation))
      })
      
      fig <- plot_ly() %>% 
        layout(xaxis = list(title = gtCol), yaxis = list(title = response))
      
      annotations <- list()
      counter <- 1
      for (gt in names(traces)) {
        
        if(gt %in% c(0,1, '0', '1')){
          if(gt == 0){
            trace_name <- 'Absence'
          } else {
            trace_name <- 'Presence'
          }
        } else {
          trace_name <- gt
        }
        
        fig <- fig %>%
          add_trace(
            y = traces[[gt]],
            type = "box",
            name = trace_name,
            boxpoints = "all", jitter = 0.3,
            pointpos = -1.8,
            text = text[[gt]]
          )
        
        annotations[[counter]] <- list(
          x = trace_name,
          y = max(traces[[gt]]) * 1.1,
          text = glue::glue("n = {length(traces[[gt]])}"),
          showarrow = FALSE,
          font = list(size = 10)
        )
        counter <- counter + 1
      }
      layout(fig, annotations = annotations)
    })
  })
}

allelicEffect_demo <- function() {
  # Marker Info sheet
  DB = "data/FFAR_selections_SNP.SilicoDArT_4.xlsx"
  df <- read_excel(path = DB, sheet = "Marker info") %>% 
    filter(AlleleID == "117740086-60-G/A") %>% 
    filter(variable == "Total Precipitation (mm)")
  favorable_allele <- df$Favorable.allele
  allelic_effect <- df$effect
  maf <- df$maf2
  pev <- df$`Phenotype_Variance_Explained(%)`
  LOD <- -log10(df$`BLINK P value (Boferroni adjusted)`)
  # Genotype  & Phenotype data
  gdf <- read_excel(path = DB, sheet = "Sheet2")
  marker_id <- "117740086-60-G/A"
  trait_id <- "prec"
  designation <- "Accesion"
  marker_name <- colnames(gdf)[grep(glue::glue("{marker_id}"), colnames(gdf))]
  tgt <- gdf[,c(designation, marker_name, trait_id)]
  # TODO Filter one association
  ui <- fluidPage(allelicEffectPlot_ui("x"))
  server <- function(input, output, session) {
    allelicEffectPlot_server("x",tgt, gtCol = "SNP_117740086-60-G/A_F_G",
                             designation = "Accesion",
                             response = "prec",
                             pev,
                             LOD,
                             favorable_allele,
                             allelic_effect,
                             maf)
  }
  shinyApp(ui, server)
}

allelicEffect_demo()
