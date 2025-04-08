
allelicEffectPlot_ui <- function(id){
  ns <- NS(id)
  card(
    card_header("Allelic Effect"),
    card_body(
      layout_column_wrap(
        value_box(
          title = "PEV",
          value = textOutput(ns("pevValue")),
        ),
        value_box(
          title = "LOD",
          value = textOutput(ns("lodValue")),
        ),
        value_box(
          title = "Favorable Allele",
          value = textOutput(ns("faValue")),
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
      
      for (gt in names(traces)) {
        fig <- fig %>%
          add_trace(
            y = traces[[gt]],
            type = "box",
            name = gt,
            boxpoints = "all", jitter = 0.3,
            pointpos = -1.8,
            text = text[[gt]]
          )
      }
      fig
    })
  })
}

allelicEffect_demo <- function() {
  # Marker Info sheet
  DB = "/home/scruz/projects/EnvGwas_viewer/data/FFAR_selections_SNP.SilicoDArT_4.xlsx"
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
