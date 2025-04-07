library(readxl)
library(dplyr)
library(shiny)
library(bslib)
library(DT)
library(plotly)

ui <- page_sidebar(
  title = "EnvGWAS V1.0",
  sidebar = sidebar(
    title = "Test title"
  ),
  allelicEffectPlot_ui('x')
)

server <- function(input, output, session){
  read_data <- reactive({
    DB = 'data/FFAR_selections_SNP.SilicoDArT_4.xlsx'
    df <- read_excel(path = DB, sheet = 'Marker info')
    df
  })
  
  allelicEffectPlot_server('x', read_data())
}

shinyApp(ui, server)