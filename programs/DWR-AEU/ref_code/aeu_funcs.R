# Standardize Columns -----------------------------------------------------

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

subset_cols_aeu <- function(df){
  keep_cols <- c('Date', 'Time', 'Station', 'Latitude', 'Longitude', 'SampleMethod', 'SampleDepth', 'OrigTaxon', 'Taxon', 'Kingdom', 'Phylum', 'Class', 'AlgalGroup', 'Genus', 'Species', 'Lab', 'Units_per_mL', 'Cells_per_mL', 'Biovolume_per_mL', 'QualityCheck', 'Debris') 
  
  df <- df %>%
    select(all_of(keep_cols))
  
  return(df) 
}
