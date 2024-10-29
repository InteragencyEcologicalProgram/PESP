
# Standardize Columns -----------------------------------------------------

# # Helper Functions ------------------------------------------------------
# rename
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
    )
}

# calculate data
calc_data_aeu <- function(df){
  df <- df %>%
    dplyr::mutate(
      Units_per_mL = round(unit_abundance * factor,2),
      Cells_per_mL = round(total_cells * factor, 2),
      Biovolume_per_mL = round(rowMeans(select(., contains('biovolume')), na.rm = TRUE) * factor * total_cells, 2)
    )
  
  return(df)
}

# add in lat/lons
add_latlon_aeu <- function(df){
  df_latlon <- read_quiet_csv(abs_data_path('Component data sets/DWR-AEU/aeu_latlon.csv'))  %>%
    select(c('Station Name','Latitude', 'Longitude')) %>%
    rename(Station = `Station Name`)
  
  df <- df %>%
    left_join(., df_latlon, by = 'Station')
  
  return(df)
}

# # Main Function ---------------------------------------------------------
standardize_cols_aeu <- function(df){
  # rename columns
  df <- rename_cols_aeu(df)
  
  # calc reporting units
  df <- calc_data_aeu(df)
  
  # properly set PhytoForm NA
  df <- clean_phytoform_bsa(df)
  
  # add in lat/lon
  df <- add_latlon_aeu(df)
  
  # fix date format
  df$Date <- as.Date(df$Date,'%m/%d/%Y')
  

  # select and reorder relevant columns
  df <- df %>%
    select(all_of(c('Date', 'Time', 'Station', 'Latitude', 'Longitude', 'Taxon', 'Genus', 'Species', 'Units_per_mL', 'Cells_per_mL', 'Biovolume_per_mL', 'GALD', 'PhytoForm', 'Comments')))
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
  keep_cols <- c('Date', 'Time', 'Station', 'Latitude', 'Longitude', 'SamplingMethod', 'SamplingDepth', 'Taxon', 'Genus', 'Species', 'Lab', 'Units_per_mL', 'Cells_per_mL', 'Biovolume_per_mL', 'QualityCheck', 'Debris') 
  
  df <- df %>%
    select(all_of(keep_cols))
  
  return(df) 
}
