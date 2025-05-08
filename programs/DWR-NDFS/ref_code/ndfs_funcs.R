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