rename_cols_bp <- function(df, lab){
  df <- df %>%
    rename(
      'Taxon' = 'Current Species ID',
      'Station' = 'Station ID',
      'Cells_per_mL' = 'Density (cells/mL)',
      'Biovolume_per_mL' = 'Biovolume (cubic micrometers/mL)',
      'Depth_m' = 'Depth (m)'
    ) %>%
    mutate(
      Lab = lab
    )
}

rename_cols_net <- function(df, lab){
  df <- df %>%
    rename(
      'Taxon' = 'Current Species ID',
      'Station' = 'Station ID',
      'Cells_per_mL' = 'Density (cells/mL)',
      'Biovolume_per_mL' = 'Biovolume (cubic micrometers/mL)',
      'TowLength_m' = 'Tow Length (m)',
      'NetRadius_cm' = 'Net Radius (cm)',
      'TowVolFiltered_m' = 'Tow Vol Filtered (L)'
    ) %>%
    mutate(
      Lab = lab
    )
}

standardize_cols_bp <- function(df, meta_df){
  # change date format
  df$Date <- lubridate::parse_date_time(df$Date, orders = c('mdy','dmy','ymd'))
  
  # grab genus
  df$Genus <- sapply(df$Taxon, function(x) strsplit(x, ' ')[[1]][1])
  
  # grab species
  df$Species <- sapply(df$Taxon, function(x) {
    names <- strsplit(x, ' ')[[1]]
    names <- paste(names[-1], collapse = ' ')
  })
  
  df <- clean_sp(df)
  
  # add cols if needed
  if (!('Notes' %in% colnames(df))) {
    df$Notes <- NA
  }
  
  if (!('TowLength_m' %in% colnames(df))) {
    df$TowLength_m <- NA
  }
  
  if (!('NetRadius_cm' %in% colnames(df))) {
    df$NetRadius_cm <- NA
  }
  
  if (!('TowVolFiltered_m' %in% colnames(df))) {
    df$TowVolFiltered_m <- NA
  }
  
  
  # pull collection type
  # df <- from_meta(df, meta_df, 'Sampling Method')
    
  # df <- df %>% rename('SamplingMethod' = `Sampling Method`)
  
  # select and reorder relevant columns
  df <- df %>%
    select(all_of(c('Date', 'Station', 'Depth_m', 'TowLength_m', 'NetRadius_cm', 'TowVolFiltered_m', 'SamplingMethod', 'Lab', 'Taxon', 'Genus', 'Species', 'Cells_per_mL', 'Biovolume_per_mL','Notes')))
}

standardize_cols_net <- function(df, meta_df, lab){
  # rename cols
  df <- rename_cols_net(df, lab)
  
  # change date format
  df$Date <- lubridate::parse_date_time(df$Date, orders = c('mdy','dmy','ymd'))

  # grab genus
  df$Genus <- sapply(df$Taxon, function(x) strsplit(x, ' ')[[1]][1])
  
  # grab species
  df$Species <- sapply(df$Taxon, function(x) {
    names <- strsplit(x, ' ')[[1]]
    names <- paste(names[-1], collapse = ' ')
  })
  
  df <- clean_sp(df)
  
  # pull collection type
  # df <- from_meta(df, meta_df, 'Sampling Method')
  
  # df <- df %>% rename('SamplingMethod' = `Sampling Method`)

  # select and reorder relevant columns
  df <- df %>%
    select(all_of(c('Date', 'Station', 'Depth_m', 'TowLength_m', 'NetRadius_cm', 'TowVolFiltered_m', 'SamplingMethod', 'Lab', 'Taxon', 'Genus', 'Species', 'Cells_per_mL', 'Biovolume_per_mL','Notes')))
}


subset_cols_bp <- function(df){
  keep_cols <- c('Date','Station','Depth_m', 'TowLength_m', 'NetRadius_cm', 'TowVolFiltered_m', 'SamplingMethod', 'Lab', 'OrigTaxon','Taxon','Kingdom','Phylum','Class','AlgalGroup','Genus', 'Species', 'Cells_per_mL','Biovolume_per_mL','QualityCheck','Debris') 
  
  df <- df %>%
    select(all_of(keep_cols))
  
  return(df) 
}
