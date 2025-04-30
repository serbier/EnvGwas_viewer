library(dplyr)

get_qtl_info <- function(m_info_df, m_id,id_col = "AlleleID"){
  
  if(!is.data.frame(m_info_df)) {
    cli::cli_abort(glue::glue("m_info_df is not a dataframe is {class(m_info_df)}"))
  } 
  
  if(!id_col %in% colnames(m_info_df)) {
    cli::cli_warn(glue::glue("{id_col} not in m_info_df"))
  }
  
  m_df <- m_info_df %>% 
    filter(!!sym(id_col) == m_id)
  
  if(dim(m_df)[1] == 0){
    cli::cli_abort(glue::glue("{m_id} not found in m_info_df"))
  } else if (dim(m_df)[1] > 1){
    cli::cli_warn(glue::glue("{m_id} have multiple ({dim(m_df)[1]}) matches in m_info_df"))
    m_df <- m_df[1,]
  }
  
  m_info <- as.list(m_df[1,])
  return(m_info)
}

get_qtl_sample_data <- function(m_gt_df, m_id){
  if(!is.data.frame(m_gt_df)) {
    cli::cli_abort(glue::glue("m_gt_df is not a dataframe is {class(m_gt_df)}"))
  } 
  
  if(!m_id %in% colnames(m_gt_df)) {
    cli::cli_warn(glue::glue("{md_col} not in m_gt_df"))
  }
  
  fmt_gt <- m_gt_df %>% 
    mutate(GT = ifelse(!!sym(m_id) == !!sym(paste0(m_id, "_Imp")),
                       !!sym(m_id), paste0(!!sym(paste0(m_id,"_Imp")),"_Imp")
                       ))
  return(fmt_gt)
}

DB = "data/FFAR_selections_SNP.SilicoDArT_4_v2.xlsx"
m_info_df <- read_excel(path = DB, sheet = "Marker.info")
marker_id <- "SNP_118390666-47-T/C"
m_info <- get_qtl_info(m_info_df, marker_id)

m_gt_df <- read_excel(path = DB, sheet = "Andean")
gt_data <- get_qtl_sample_data(m_gt_df, marker_id)
