combine_taxa_lts <- function(df) {
  # ID groups with duplicates
  duplicated_groups <- df %>%
    group_by(across(-c(OrigTaxon, Cells_per_mL))) %>%
    filter(n() > 1) %>%
    filter(length(unique(OrigTaxon)) > 1) %>%
    ungroup()
  
  # remove the duplicated groups from the main dataframe
  df_no_dupes <- df %>%
    anti_join(duplicated_groups, by = setdiff(names(df), c('OrigTaxon', 'Cells_per_mL')))
  
  # summarize the duplicated groups
  df_combined <- duplicated_groups %>%
    group_by(across(-c(OrigTaxon, Cells_per_mL))) %>%
    summarise(
      Cells_per_mL = signif(sum(Cells_per_mL, na.rm = TRUE),4),
      OrigTaxon = paste(unique(ifelse(is.na(OrigTaxon), Taxon, OrigTaxon)), collapse = '; '),
      .groups = 'drop'
    )
  
  # combine the summarized groups back with the original data
  df_final <- bind_rows(df_no_dupes, df_combined) %>%
    arrange(Date, Time, Taxon)
  
  # create log
  df_log <- duplicated_groups %>%
    select(Date, Time, Taxon, OrigTaxon, Cells_per_mL) %>%
    group_by(Date, Time, Taxon) %>%
    summarise(
      OrigTaxons = paste(unique(ifelse(is.na(OrigTaxon), Taxon, OrigTaxon)), collapse = '; '),
      OldValue = paste(Cells_per_mL, collapse = '; '),
      NewValue = signif(sum(Cells_per_mL, na.rm = TRUE),4),
      .groups = 'drop'
    ) %>%
    select(Date, Time, OrigTaxons, OldValue, NewValue) %>%
    distinct()

  message('Combined taxa: ', nrow(df_log))
  attr(df_final, 'log') <- list(combine_taxa_lts = df_log)
  
  return(df_final)
}

reorder_cols_lts <- function(df) {
  desired_order <- c(
    'ICF_ID', 'Sortable_ID', 'Date', 'Time', 'ActionYear', 
    'SiteID', 'Habitat', 'Strata', 'Latitude', 'Longitude', 
    'SiteCount', 'SiteDepth', 'SampleMethod', 'SampleDepth', 'OrigTaxon', 
    'Taxon', 'Kingdom', 'Phylum', 'Class', 'AlgalGroup', 
    'Genus', 'Lab', 'Cells_per_mL'
  )

  dropped_cols <- setdiff(names(df), desired_order)
  
  df <- df %>%
    select(all_of(desired_order))
  
  if (length(dropped_cols) > 0) {
    message('Dropped columns: ', paste(dropped_cols, collapse = ', '))
  } else {
    message('No columns were dropped.')
  }
  
  return(df)
}

missing_latlon_lts <- function(df) {
  df_missing <- df %>%
    filter(is.na(Latitude) | is.na(Longitude)) %>%
    select(Date, Time, Sortable_ID, Latitude, Longitude) %>%
    distinct()
  
  df_log <- df_missing %>%
    distinct()
  
  message('Rows with missing Latitude or Longitude: ', nrow(df_log))
  attr(df, 'log') <- list(missing_latlon = df_log)
  
  return(df)
}

missing_time_lts <- function(df) {
  df_missing <- df %>%
    filter(is.na(Time)) %>%
    select(Date, Time, Sortable_ID) %>%
    distinct()
  
  df_log <- df_missing %>%
    distinct()
  
  message('Rows with missing Time: ', nrow(df_log))
  attr(df, 'log') <- list(missing_time = df_log)
  
  return(df)
}

sortable_check_lts <- function(df) {
  df_check <- df %>%
    group_by(Sortable_ID) %>%
    summarise(n = n(), .groups = 'drop') %>%
    filter(n > 1) %>%
    distinct()
  
  df_log <- df_check %>%
    distinct()
  
  message('Sortable_IDs with duplicates: ', nrow(df_log))
  attr(df, 'log') <- list(sortable_check = df_log)
  
  return(df)
}
