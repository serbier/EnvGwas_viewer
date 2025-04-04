#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(dplyr)
library(readxl)
library(bslib)
library(shinydashboard)

# Define UI for application that draws a histogram
ui <- page_sidebar(
  title = "EnvGWAS V1.0",
  sidebar = sidebar(
    title = "Genepool",
    selectInput(inputId = 'genepool_select',
                label = "Select Genepool",
                choices = c("Andean","Mesoamerican"))
  ),
  # Main area
  fluidRow(
    layout_columns(
      value_box(
        title = "Number of Samples",
        value = textOutput('summ_box_ninds'),
        showcase = bsicons::bs_icon('diagram-3')
      ),
      value_box(
        title = "Number of QTNs",
        value = textOutput('summ_box_nqtns'),
        showcase = bsicons::bs_icon('database')
      )
    )
  )  
)

# Define server logic required to draw a histogram
server <- function(input, output) {

  DB = 'data/FFAR_selections_SNP.SilicoDArT_4.xlsx'
  
  # Reads marker info sheet
  read_marker_info <- reactive({
    req(input$genepool_select)
    
    df <- read_excel_sheet(path = DB,
                           sheet = 'Marker info')
    df %>% filter(Subset == input$genepool_select)
  })
  
  
  output$summary_value_boxes <- renderUI({
    req(read_marker_info)
    
    ninds = 1227
    output$summ_box_ninds <- renderText(ninds)
    
    nqtns = dim(read_marker_info())[1]
    output$summ_box_nqtns <- renderText(nqtns)
  })
}

read_excel_sheet <- function(path, sheet){
  table <- read_excel(path = path,
                      sheet = sheet,
                      na = c('.', '', 'NA'))
  return(table)
}
# Run the application 
shinyApp(ui = ui, server = server)
