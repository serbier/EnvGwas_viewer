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