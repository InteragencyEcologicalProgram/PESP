
# Standardize Columns -----------------------------------------------------

# # Helper Functions ------------------------------------------------------
# rename columns
rename_cols_bgc <- function(df){
  df <- df %>%
    rename(
      'Phylum' = 'division',
      'Genus' = 'genus',
      'Taxon' = 'species',
      'Station' = 'site_abbrev',
      'Biovolume_per_mL' = 'biovolume_um3_per_ml',
      'Cells_per_mL' ='cells_per_ml',
      'SamplingMethod' = 'sampler',
      'Latitude' = 'latitude',
      'Longitude' = 'longitude',
      'Comments' = 'notes'
    ) %>%
    mutate(
      'Species' = gsub('^.* ', '', Taxon),
      'Time' = hms::as_hms(mdy_hms(datetime_pst, tz = 'America/Los_Angeles')),
      'Date' = as.Date(df_data$datetime_pst, format = '%m/%d/%Y'),
      'Replicate' = case_when(replicate == 'y' ~ TRUE, replicate == 'n' ~ FALSE),
      'SamplingMethod' = case_when(SamplingMethod == 'Grab sample' ~ 'Unknown', TRUE ~ SamplingMethod)
    ) %>%
    filter(!(Station == 'WLS')) # remove station with no data

  df <- df %>%
    mutate(Species =
             case_when(Genus == Species ~ 'sp.',
                       TRUE ~ Species))
}

# remove replicates
remove_reps_bgc <- function(df){
  df <- df %>%
    filter(!Replicate)
  
  return(df)
}

# # subset columns
# subset_cols_bgc <- function(df){
#   keep_cols <- c('Date', 'Time', 'Station', 'SamplingMethod', 'Replicate', 'Taxon', 'Genus', 'Species', 'Cells_per_mL', 'Biovolume_per_mL', 'QualityCheck') 
#   
#   df <- df %>%
#     select(keep_cols)
#  
#   return(df) 
# }

# # Main Function ---------------------------------------------------------
standardize_cols_bgc <- function(df){
  # rename columns
  df <- rename_cols_bgc(df)
  
  # remove replicates
  df <- remove_reps_bgc(df)

  # select and reorder relevant columns
  df <- df %>%
    select(all_of(c('Date', 'Time', 'Station', 'Latitude', 'Longitude', 'SamplingMethod', 'Taxon', 'Genus', 'Species', 'Cells_per_mL', 'Biovolume_per_mL', 'Comments')))
}

subset_cols_bgc <- function(df){
  keep_cols <- c('Date', 'Time', 'Station', 'Latitude', 'Longitude', 'SamplingMethod', 'SamplingDepth', 'Taxon', 'Genus', 'Species', 'Lab', 'Cells_per_mL', 'Biovolume_per_mL', 'QualityCheck') 
  
  df <- df %>%
    select(all_of(keep_cols))
  
  return(df) 
}
