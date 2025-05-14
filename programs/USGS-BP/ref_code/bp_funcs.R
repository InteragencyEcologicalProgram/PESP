
standardize_cols_bp <- function(df, meta_df){
  # select and reorder relevant columns
  df <- df %>%
    select(all_of(c('Date', 'Station', 'Depth_m', 'TowLength_m', 'NetRadius_cm', 'TowVolFiltered_m', 'SamplingMethod', 'Lab', 'Taxon', 'Genus', 'Species', 'Cells_per_mL', 'Biovolume_per_mL','Notes')))
}

standardize_cols_net <- function(df, meta_df, lab){
  # rename cols
  df <- rename_cols_net(df, lab)
  
  # change date format
  df$Date <- lubridate::parse_date_time(df$Date, orders = c('mdy','dmy','ymd'))

  # select and reorder relevant columns
  df <- df %>%
    select(all_of(c('Date', 'Station', 'Latitude', 'Longitude', 'Depth_m', 'TowLength_m', 'NetRadius_cm', 'TowVolFiltered_m', 'SamplingMethod', 'Lab', 'Taxon', 'Genus', 'Species', 'Cells_per_mL', 'Biovolume_per_mL','Notes')))
}


subset_cols_bp <- function(df) {
  # Define the desired column order
  desired_order <- c(
    'Date', 'Station', 'Latitude', 'Longitude', 'Depth_m', 'TowLength_m', 'NetRadius_cm', 
    'TowVolFiltered_m', 'SampleMethod', 'Lab', 'OrigTaxon', 'Taxon', 
    'Kingdom', 'Phylum', 'Class', 'AlgalGroup', 'Genus', 'Species', 
    'Cells_per_mL', 'Biovolume_per_mL', 'QualityCheck', 'Debris'
  )
  
  # Identify dropped columns
  dropped_cols <- setdiff(names(df), desired_order)
  
  # Reorder and select only the desired columns
  df <- df %>%
    select(all_of(desired_order))
  
  # Print a message if any columns were dropped
  if (length(dropped_cols) > 0) {
    message('Dropped columns: ', paste(dropped_cols, collapse = ', '))
  } else {
    message('No columns were dropped.')
  }
  
  return(df)
}