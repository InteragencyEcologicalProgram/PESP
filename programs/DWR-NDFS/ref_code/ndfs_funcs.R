
standardize_cols_ndfs <- function(df, meta_df){
  # pull collection type
  df <- from_meta(df, meta_df, 'Sampling Method')
  
  df <- df %>% rename('SamplingMethod' = `Sampling Method`)
}

add_dilution_qc <- function(df){
  df_data %>%
    mutate(
      QualityCheck =
        case_when(
          Date <= '2023-06-25' & QualityCheck == 'NoCode' ~ 'DifDilution',
          Date <= '2023-06-25' ~ paste0(QualityCheck, ' DifDilution'),
          TRUE ~ QualityCheck
        )
    )
}