#' Creates a "quality check" column based on a comment column
#'
#'@param df A data frame
#'@param comment_col the name (as a string) of the comment column
#' @return A data frame with a comment column added
#' 
func_qc_col <- function(df, comment_col){
  df <- df %>%
  mutate(
    QC_1 = case_when(grepl('broken\\.|cannot meet|did not reach|delete', comment_col, ignore.case = TRUE) ~ 'BadData'),
    QC_2 = case_when(grepl('degraded', comment_col, ignore.case = TRUE) ~ 'Degraded'),
    QC_3 = case_when(grepl('poorly preserved', comment_col, ignore.case = TRUE) ~ 'PoorlyPreserved'),
    QC_4 = case_when(grepl('fragment\\.', comment_col, ignore.case = TRUE) ~ 'Fragmented')
  ) %>%
  unite(QualityCheck, starts_with('QC'), remove = TRUE, na.rm = TRUE, sep = ' ')
  
  df$QualityCheck[df$QualityCheck == ''] <- 'Good'
  
  return(df)
}

#' standardize "unknown" identifiers