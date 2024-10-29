
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
calc_units_bsa <- function(df){
  df <- df %>%
    dplyr::mutate(
      Units_per_mL = round(`Unit Abundance (# of Natural Units)` * Factor, 2),
      Cells_per_mL = round(`Total Number of Cells` * Factor, 2),
      Biovolume_per_mL = round(rowMeans(select(., contains('Biovolume')), na.rm = TRUE) * Factor * `Total Number of Cells`, 2)
    )
  
  return(df)
}

# correctly identify NA values in PhytoForm
clean_phytoform_bsa <- function(df){
  df$PhytoForm[df$PhytoForm == 'n/p'] <- NA_character_
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


