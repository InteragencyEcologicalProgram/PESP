
# Update Phyto Dates ------------------------------------------------------

update_dates_emp <- function(df_phyto, df_fixed_dates) {
  # rename columns for clarity
  df_fix <- df_fixed_dates %>%
    rename(
      PhytoDate = PhytoDate,
      WQDate = WQDate
    )
  
  # identify matches based on Station + Date
  df_matched <- df_phyto %>%
    left_join(df_fix, by = c('Station', 'Date' = 'PhytoDate'))
  
  # log where changes will occur
  df_log <- df_matched %>%
    filter(!is.na(WQDate), Date != WQDate) %>%
    transmute(
      Station,
      OriginalDate = Date,
      UpdatedDate = WQDate
    ) %>%
    distinct()
  
  message('Dates updated for ', nrow(df_log), ' samples')
  
  # apply the new WQDate where available
  df_updated <- df_matched %>%
    mutate(Date = coalesce(WQDate, Date)) %>%
    select(-WQDate)
  
  attr(df_updated, 'log') <- list(fixed_dates = df_log)
  return(df_updated)
}

# Add WQ Data -------------------------------------------------------------

add_wq_emp <- function(df_phyto, df_wq) {
  
  df_nomatch <- dplyr::anti_join(df_phyto, df_wq, by = c('Station', 'Date'))
  
  df_joined <- df_phyto %>%
    dplyr::left_join(
      df_wq %>% dplyr::select(Station, Date, Latitude, Longitude, Time),
      by = c('Station', 'Date')
    )
  
  df_log <- df_nomatch %>%
    dplyr::select(Station, Date) %>%
    dplyr::distinct()
  
  message('Phyto rows without water quality match (Station + Date): ', nrow(df_log))
  attr(df_joined, 'log') <- list(missing_wq = df_log)
  
  return(df_joined)
}


# Subset Columns ----------------------------------------------------------

subset_cols_emp <- function(df){
  keep_cols <- c('Date',
                 'Time',
                 'Location',
                 'Station',
                 'Latitude',
                 'Longitude',
                 'Lab',
                 'CountMethod',
                 'OrigTaxon',
                 'Taxon',
                 'Kingdom',
                 'Phylum',
                 'Class',
                 'AlgalGroup',
                 'Genus',
                 'Species',
                 'Cells_per_mL',
                 'Units_per_mL',
                 'Biovolume_per_mL',
                 'GALD',
                 'PhytoForm',
                 'QualityCheck',
                 'Debris',
                 'Notes') 
  
  df <- df %>%
    select(all_of(keep_cols))
  
  return(df) 
}
