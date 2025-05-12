library(dplyr)
library(DT)
source("R/greedy_selection.R")


selectionBag_ui <- function(id) {
  ns <- NS(id)
  card(
    height = "600px",
    card_header(
      class = "bg-primary bg-opacity-25 text-black",
      "Subset Selection"
    ),
    card_body(
      uiOutput(ns("TEST"))
    )
  )
}

selectionBag_server <- function(id, bag, marker_info) {
  moduleServer(id, function(input, output, session) {
    
    get_dosage_mt <- reactive({
      req(bag$target_markers)
      req(bag$bag)
      
      unique_markers <- sapply(unique(bag$target_markers), function(x){
        paste0(x,"_Imp")
      })
      
      print(unique_markers)
      
      selected_ind_data <- bag$bag %>%
        group_by(Accesion) %>%
        mutate(concatenated = paste(reason, collapse = ", ")) %>%
        slice_head(n = 1)
      
      dosage <- selected_ind_data[,c("Accesion", unique_markers)]
      dosage
    })
    
    get_coverage_mt <- reactive({
      req(get_dosage_mt())
      # Get favorable allleles
      markers <- unique(bag$target_markers)

      selected_markers <- marker_info %>% 
        filter(AlleleID %in% markers)
      
      cov_df <- purrr::map_dfc(markers, function(x){
        favorable <- selected_markers %>%  
          filter(AlleleID == x) %>% 
          pull(Favorable.allele) 
        
        favorable <- unique(favorable)
        
        print(favorable)
        marker_name <- paste0(x, "_Imp")
    
        get_dosage_mt() %>%
          mutate( test = if_else(!!sym(marker_name) %in% c(favorable, 
                                                           paste0(rep(favorable,2),
                                                                  collapse = "")), 1, 0)) %>% 
          pull(test)
      })
      
      colnames(cov_df) <- markers
      cov_df$Accesion <- get_dosage_mt() %>% pull(Accesion)
      
      cov_df
      
    })
    
    output$TEST <- renderUI({
      req(get_coverage_mt())
      
      target_mt <- get_coverage_mt()
      print(target_mt)
      mt <- target_mt %>% select(-Accesion) 
      print(mt)
      select <- greedy_set_cover_na(mt, 1:dim(target_mt)[1])
      print(select)
      target_inds <- target_mt[select$selected,]$Accesion
      
      sdf <- get_coverage_mt() %>% 
        filter(!!sym("Accesion") %in% target_inds)
      
      out <- tags$div(
        renderDT(datatable(sdf) %>% 
                   formatStyle(
                     columns = colnames(sdf),  # Apply to all columns
                     backgroundColor = styleEqual(
                       levels = c(0, 1, NA),
                       values = c("gray", "darkgreen", "salmon")  # Colors for 0 and 1
                     )
                   )
        ))
      out
    })
    
    
  })
}
