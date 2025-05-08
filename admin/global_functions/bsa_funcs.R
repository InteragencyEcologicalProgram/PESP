
# Read in Data ------------------------------------------------

read_bsa_xlsx <- function(path){
  # read in datetime columns as date
  vec_names <- names(readxl::read_excel(path, n_max = 0))
  
  # define col types
  vec_types <- ifelse(grepl('Date|Time', vec_names), 'date', 'guess')
  
  # read in data
  df <- readxl::read_excel(path, col_types = vec_types)
  
  return(df)
}

read_bsa_csv <- function(path){
  # read in datetime columns as date
  vec_names <- names(readxl::read_excel(path, n_max = 0))
  
  # define col types
  vec_types <- ifelse(grepl('Date|Time', vec_names), 'date', 'guess')
  
  # read in data
  df <- readxl::read_excel(path, col_types = vec_types)
  
  return(df)
}

# Standardize Column Names ------------------------------------------------

# # Helper Functions ------------------------------------------------------

# remove blank rows caused by measurement cols
remove_rows_bsa <- function(df){
  # subset columns with word 'measurement'
  mes_cols <- colnames(df)[grepl('Measurement', colnames(df))]

  # remove measurement and dimension columns
  df <- df %>%
    dplyr::select(-c(Dimension, mes_cols))

  # remove completely blank rows cause by measurement columns
  df <- janitor::remove_empty(df, which = 'rows')

  return(df)
}

# rename columns
rename_cols_bsa <- function(df){
  df <- df %>%
    dplyr::rename(
      Date = SampleDate,
      Time = SampleTime,
      Station = StationCode,
      PhytoForm = `Colony/Filament/Individual Group Code`,
      GALD = `GALD 1`,
    ) %>%
    dplyr::mutate(
      Lab = 'BSA'
    )
  
  return(df)
}

#' calc reporting units
calc_data_bsa <- function(df,
                          unit_col = 'Unit Abundance (# of Natural Units)',
                          cell_col = 'Total Number of Cells',
                          calc_cols = c('Units', 'Cells', 'Biovolume')) {
  
  calc_cols <- match.arg(calc_cols, choices = c('Units', 'Cells', 'Biovolume'), several.ok = TRUE)
  calculated <- character()
  
  # Find the case-insensitive Factor column
  factor_col <- names(df)[grepl('factor', names(df), ignore.case = TRUE)]
  if (length(factor_col) == 0) {
    stop('Factor column not found')
  }
  
  if ('Units' %in% calc_cols) {
    df <- df %>%
      dplyr::mutate(Units_per_mL = round(.data[[unit_col]] * .data[[factor_col]], 2))
    calculated <- c(calculated, 'Units_per_mL')
  }
  
  if ('Cells' %in% calc_cols) {
    df <- df %>%
      dplyr::mutate(Cells_per_mL = round(.data[[cell_col]] * .data[[factor_col]], 2))
    calculated <- c(calculated, 'Cells_per_mL')
  }
  
  if ('Biovolume' %in% calc_cols) {
    df <- df %>%
      dplyr::mutate(Biovolume_per_mL = round(
        rowMeans(select(., contains('Biovolume')), na.rm = TRUE) * .data[[factor_col]] * .data[[cell_col]],
        2
      ))
    calculated <- c(calculated, 'Biovolume_per_mL')
  }
  
  message('Calculated: ', paste(calculated, collapse = ', '))
  
  return(df)
}

# correctly identify NA values in PhytoForm
clean_phytoform_bsa <- function(df) {
  df$PhytoForm <- replace(df$PhytoForm, df$PhytoForm %in% c('n/p', 'na', 'NA'), NA_character_)
  
  unique_codes <- unique(df$PhytoForm)
  message('PhytoForm codes: ', paste(sort(unique_codes[!is.na(unique_codes)]), collapse = ', '))
  
  return(df)
}

# # Main Function ---------------------------------------------------------

# standardize columns from raw BSA data
standardize_cols_bsa <- function(df, meta_df){
  df <- remove_rows_bsa(df)
  
  df <- rename_cols_bsa(df)

  df <- calc_units_bsa(df)
  
  df <- clean_phytoform_bsa(df)
  
  # pull collection type
  df <- from_meta(df, meta_df, 'Sampling Method')
  
  df <- df %>% rename('SamplingMethod' = `Sampling Method`)
  
  # select and reorder relevant columns
  df <- df %>%
    select(all_of(c('Lab', 'Date', 'Time', 'Station', 'Taxon', 'Genus', 'Species', 'Units_per_mL', 'Cells_per_mL', 'Biovolume_per_mL', 'GALD', 'PhytoForm', 'Comments')))
}


