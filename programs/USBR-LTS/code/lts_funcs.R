rename_cols_aeu <- function(df){
  df <- df %>%
    rename(
      'Genus' = 'genus',
      'Species' = 'species',
      'Taxon' = 'taxon',
      'Station' = 'station_code',
      'Date' = 'sample_date',
      'Time' = 'sample_time',
      'PhytoForm' = 'colony_filament_individual_group_code',
      'Comments' = 'comments',
      'GALD' = 'gald1'
    ) %>%
    mutate(
      Lab = 'BSA'
    )
}

calc_data_aeu <- function(df){
  df <- df %>%
    dplyr::mutate(
      Units_per_mL = round(unit_abundance * factor,2),
      Cells_per_mL = round(total_cells * factor, 2),
      Biovolume_per_mL = round(biovolume_1 * factor * total_cells, 2)
    )
  
  return(df)
}

standardize_cols_aeu <- function(df, meta_df){
  # rename columns
  df <- rename_cols_aeu(df)
  
  # calc reporting units
  df <- calc_data_aeu(df)
  
  # fix date format
  df$Date <- as.Date(df$Date,'%m/%d/%Y')
  
  # pull collection type
  df <- from_meta(df, meta_df, 'Sampling Method')
    
  df <- df %>% rename('SamplingMethod' = `Sampling Method`)
  
  # select and reorder relevant columns
  df <- df %>%
    select(all_of(c('Date', 'Time', 'Station', 'SamplingMethod', 'Lab', 'Taxon', 'Genus', 'Species', 'Units_per_mL', 'Cells_per_mL', 'Biovolume_per_mL', 'GALD', 'PhytoForm', 'Comments')))
}

add_dilution_qc <- function(df){
  df_data %>%
    mutate(
      QualityCheck =
        case_when(
          Date <= '2023-06-25' & QualityCheck == 'Good' ~ 'DifDilution',
          Date <= '2023-06-25' ~ paste0(QualityCheck, ' DifDilution'),
          TRUE ~ QualityCheck
        )
    )
}

subset_cols_aeu <- function(df){
  keep_cols <- c('Date','Time','Station','SamplingMethod','Lab','Taxon','Genus','Species','Units_per_mL','Cells_per_mL','Biovolume_per_mL','QualityCheck') 
  
  df <- df %>%
    select(all_of(keep_cols))
  
  return(df) 
}
