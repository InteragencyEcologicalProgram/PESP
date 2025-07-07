
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
      )
    ) %>%
    filter(!(Station == 'WLS')) # remove station with no data
  
  message(
    '- standardizing Date, Time, and Replicate\n',
    '- removed rows with Station == WLS'
  )
  
  return(df)
}

# # Main Function ---------------------------------------------------------
subset_cols_bgc <- function(df){
  keep_cols <- c('Date',
                 'Time',
                 'Station',
                 'Latitude',
                 'Longitude',
                 'SampleMethod',
                 'SampleDepth',
                 'OrigTaxon',
                 'Taxon',
                 'Kingdom',
                 'Phylum',
                 'Class',
                 'AlgalGroup',
                 'Genus',
                 'Species',
                 'Lab',
                 'Cells_per_mL',
                 'Biovolume_per_mL',
                 'QualityCheck',
                 'Notes') 
  
  df <- df %>%
    select(all_of(keep_cols))
  
  return(df) 
}

# remove the duplicate rows that only differ by a method based on csv
filter_methods_bgc <- function(df, df_corrections) {
  df_corrections <- read_quiet_csv(abs_pesp_path('Groups/USGS-BGC/Metadata/BGC_sample_method_corrections.csv'))
  
  # construct key
  df$key <- paste(df$datetime_pst, df$site_abbrev)
  df_corrections$key <- paste(df_corrections$datetime_pst, df_corrections$site_abbrev)
  
  corrections_lookup <- df_corrections[, c('key', 'sampler')]
  
  df$has_correction <- df$key %in% corrections_lookup$key
  df <- merge(df, corrections_lookup, by = 'key', all.x = TRUE, suffixes = c('', '_corr'))
  
  to_keep <- !df$has_correction | df$sampler == df$sampler_corr
  
  df_filtered <- df[to_keep, ]
  
  # create log
  df_log <- df[!to_keep, c('datetime_pst', 'site_abbrev', 'sampler')]
  df_log <- unique(df_log)
  
  # cleanup
  df_filtered$key <- NULL
  df_filtered$has_correction <- NULL
  df_filtered$sampler_corr <- NULL
  
  attr(df_filtered, 'log') <- list(removed_method_rows = df_log)
  
  return(df_filtered)
}
