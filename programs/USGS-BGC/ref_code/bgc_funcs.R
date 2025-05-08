
# Standardize Columns -----------------------------------------------------

# # Helper Functions ------------------------------------------------------
# clean cols
clean_cols_bgc <- function(df) {
  df <- df %>%
    mutate(
      'Time' = sub('^\\d{1,2}/\\d{1,2}/\\d{4}\\s+(\\d{1,2}:\\d{2}).*$', '\\1', datetime_pst),
      'Date' = as.Date(datetime_pst, format = '%m/%d/%Y'),
      'Replicate' = case_when(
        replicate == 'y' ~ TRUE,
        replicate == 'n' ~ FALSE
      ),
      'SampleMethod' = case_when(
        SampleMethod == 'Grab sample' ~ 'Unknown',
        TRUE ~ SampleMethod
      )
    ) %>%
    filter(!(Station == 'WLS')) # remove station with no data
  
  message(
    '- standardizing Date, Time, and Replicate\n',
    '- set Unknown SampleMethod to Grab Sample\n',
    '- removed rows with Station == WLS'
  )
  
  return(df)
}

# # Main Function ---------------------------------------------------------
subset_cols_bgc <- function(df){
  keep_cols <- c('Date', 'Time', 'Station', 'Latitude', 'Longitude', 'SampleMethod', 'SampleDepth', 'OrigTaxon', 'Taxon', 'Kingdom', 'Phylum', 'Class', 'AlgalGroup', 'Genus', 'Species', 'Lab', 'Cells_per_mL', 'Biovolume_per_mL', 'QualityCheck') 
  
  df <- df %>%
    select(all_of(keep_cols))
  
  return(df) 
}
