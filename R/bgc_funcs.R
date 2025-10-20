
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
                 'site_no',
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
                 'Debris',
                 'Notes') 
  
  df <- df %>%
    select(all_of(keep_cols))
  
  message(glue::glue('Columns: {toString(colnames(df))}'))
  
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

# Update Stations ---------------------------------------------------------

update_stations_bgc <- function(df_data) {
  df_stations <- read_quiet_csv(abs_pesp_path('Groups/USGS-BGC/Metadata/Stations-BGC.csv'))
  
  # Create a lookup table
  alias_lookup <- df_stations %>%
    filter(!is.na(field_id_alias) & field_id_alias != '') %>%
    rowwise() %>%
    do({
      aliases <- str_split(.$field_id_alias, ',')[[1]]
      aliases <- str_trim(aliases)
      data.frame(
        alias = aliases,
        station = .$Station,
        site_no = .$site_no,
        stringsAsFactors = FALSE
      )
    }) %>%
    ungroup()
  
  alias_lookup <- alias_lookup %>%
    mutate(site_no = as.character(site_no))
  
  df_data <- df_data %>%
    mutate(site_no = as.character(site_no))
  
  df_with_original <- df_data %>%
    mutate(Station_original = Station)
  
  df_updated <- df_with_original %>%
    left_join(
      alias_lookup %>% select(alias, site_no, station),
      by = c('Station' = 'alias', 'site_no' = 'site_no')
    ) %>%
    mutate(
      Station = ifelse(!is.na(station) & station != Station, station, Station)
    ) %>%
    select(-station)
  
  # Identify changes
  changed_rows <- df_updated %>%
    filter(Station_original != Station) %>%
    select(original = Station_original, updated = Station) %>%
    distinct()
  
  # Generate log message
  if (nrow(changed_rows) > 0) {
    change_message <- paste(
      'Updated station names:',
      paste0('  • ', changed_rows$original, ' → ', changed_rows$updated, collapse = '\n'),
      paste0('\nTotal changes: ', nrow(changed_rows)),
      sep = '\n'
    )
    message(change_message)
  } else {
    message('No station name updates were needed.')
  }
  
  df_updated <- df_updated %>%
    select(-Station_original) %>%
    mutate(Station = str_trim(Station))
  
  return(df_updated)
}



