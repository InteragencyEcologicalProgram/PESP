
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
                          unit_col = 'Unit Abundance',
                          cell_col = 'Total Number of Cells',
                          calc_cols = c('Units', 'Cells', 'Biovolume')) {
  
  calc_cols <- match.arg(calc_cols, choices = c('Units', 'Cells', 'Biovolume'), several.ok = TRUE)
  calculated <- character()
  
  # Find the case-insensitive Factor column
  factor_col <- names(df)[grepl('factor', names(df), ignore.case = TRUE)]
  if (length(factor_col) == 0) {
    stop('Factor column not found')
  }
  
  # Make sure all relevant cols are numeric
  df <- df %>%
    dplyr::mutate(across(all_of(c(unit_col, cell_col)), as.numeric),
                  across(contains('Biovolume'), as.numeric),
                  Factor = as.numeric(.data[[factor_col]])) 
  
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

# calc reporting units
calc_data_bsa <- function(df,
                          unit_col = 'Unit Abundance',
                          cell_col = 'Total Number of Cells',
                          calc_cols = c('Units', 'Cells', 'Biovolume')) {
  
  calc_cols <- match.arg(calc_cols, choices = c('Units', 'Cells', 'Biovolume'), several.ok = TRUE)
  calculated <- character()
  
  # find factor column
  factor_col <- names(df)[grepl('factor', names(df), ignore.case = TRUE)]
  if (length(factor_col) == 0) {
    stop('Factor column not found')
  }
  
  # convert relevant cols to numeric
  df <- df %>%
    dplyr::mutate(across(all_of(c(unit_col, cell_col)), as.numeric),
                  across(matches('Biovolume', ignore.case = TRUE), as.numeric),
                  Factor = as.numeric(.data[[factor_col]])) 
  
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
  
  # biovol is calc'd by averaging biovol columns
  if ('Biovolume' %in% calc_cols) {
    df <- df %>%
      dplyr::mutate(Biovolume_per_mL = round(
        rowMeans(select(., matches('Biovolume', ignore.case = TRUE)), na.rm = TRUE) * .data[[factor_col]] * .data[[cell_col]],
        2
      ))
    calculated <- c(calculated, 'Biovolume_per_mL')
  }
  
  message('Calculated: ', paste(calculated, collapse = ', '))
  
  return(df)
}

# fix up phytoform (preliminary)
clean_phytoform_bsa <- function(df) {
  # lowercase letters
  df$PhytoForm <- tolower(df$PhytoForm)
  
  # replace NAs
  df$PhytoForm <- replace(df$PhytoForm, df$PhytoForm %in% c('n/p', 'na', 'NA', '#n/a', '0', '1'), NA_character_)
  
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


# # EA Calcs ---------------------------------------------------------------

# calc reporting units
calc_data_ea <- function(df,
                         unit_col = 'Organisms per mL',
                         cell_col = 'Cells per Unit',
                         calc_cols = c('Units', 'Cells', 'Biovolume')) {
  
  calc_cols <- match.arg(calc_cols, choices = c('Units', 'Cells', 'Biovolume'), several.ok = TRUE)
  calculated <- character()
  
  # only require factor column if Biovolume is calculated
  if ('Biovolume' %in% calc_cols) {
    factor_col <- names(df)[grepl('factor', names(df), ignore.case = TRUE)]
    if (length(factor_col) == 0) stop('Factor column not found')
  } else {
    factor_col <- NULL
  }
  
  # convert only existing numeric columns
  df <- df %>%
    mutate(across(any_of(c(unit_col, cell_col)), as.numeric),
           across(matches('Biovolume', ignore.case = TRUE), as.numeric))
  
  if (!is.null(factor_col))
    df <- df %>% mutate(Factor = as.numeric(.data[[factor_col]]))
  
  if ('Units' %in% calc_cols) {
    df <- df %>%
      mutate(Units_per_mL = round(.data[[unit_col]], 2))
    calculated <- c(calculated, 'Units_per_mL')
  }
  
  if ('Cells' %in% calc_cols && cell_col %in% names(df)) {
    df <- df %>%
      mutate(Cells_per_mL = round(.data[[unit_col]] * .data[[cell_col]], 2))
    calculated <- c(calculated, 'Cells_per_mL')
  }
  
  if ('Biovolume' %in% calc_cols && !is.null(factor_col)) {
    df <- df %>%
      mutate(Biovolume_per_mL = round(
        rowMeans(select(., matches('Biovolume', ignore.case = TRUE)), na.rm = TRUE) *
          .data[[factor_col]] * .data[[cell_col]],
        2
      ))
    calculated <- c(calculated, 'Biovolume_per_mL')
  }
  
  message('Calculated: ', paste(calculated, collapse = ', '))
  df
}


