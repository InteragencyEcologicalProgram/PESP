fix_station_typos <- function(df_col) {
  # read in files for checks
  # # list of stations (excluding EZs)
  df_stations <- read_quiet_csv('programs/DWR-EMP/ref_data/EMP_phyto_stations.csv')
  
  # # associate station names with typos
  df_typos <- read_quiet_csv('programs/DWR-EMP/ref_data/EMP_phyto_station_typos.csv')
  
  # create logical vector of station names
  all_stations <- paste(unique(df_stations$Station), collapse = '|')
  
  # trim whitespace
  df_col <- sapply(df_col, function(x)
    trimws(x), USE.NAMES = FALSE)
  
  # remove spaces (excl. from EZs)
  df_col <-
    sapply(df_col, function(x)
      ifelse(grepl(all_stations, x), str_split_1(x, ' ')[1], x), USE.NAMES = FALSE)
  
  # remove - (excl. from EZs)
  df_col <-
    sapply(df_col, function(x)
      ifelse(grepl(all_stations, x), str_split_1(x, '-')[1], x), USE.NAMES = FALSE)
  
  # collapse what's left
  df_col <-
    sapply(df_col, function(x)
      gsub(' ', '', x), USE.NAMES = FALSE)
  
  return(df_col)
}