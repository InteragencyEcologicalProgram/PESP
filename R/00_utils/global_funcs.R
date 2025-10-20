
# Utility Operators -------------------------------------------------------

#' @noRd
'%!in%' <- function(x,y)!('%in%'(x,y))

# Input/Output Helpers ----------------------------------------------------

#' @noRd
read_quiet_csv <- function(fp, ...){
  df <- suppressWarnings(read_csv(fp, show_col_types = FALSE, ...))
  
  return(df)
}

#' @noRd
write_log_file <- function(df_log, fp) {
  if (exists('df_log') && is.data.frame(df_log) && nrow(df_log) > 0) {
    write_csv(df_log, abs_pesp_path(fp))
  }
}

#' @noRd
bullet_message <- function(header, items) {
  if (length(items) == 0) {
    message(header)
  } else {
    message(paste(c(header, paste0('  • ', items)), collapse = '\n'))
  }
}

#' @noRd
append_log <- function(df, new_log) {
  old <- attr(df, 'log')
  attr(df, 'log') <- c(old, new_log)
  
  return(df)
}

# Path and Metadata Readers -----------------------------------------------

#' @title Absolute Path to PESP Data Folder
#' @description
#' Constructs an absolute path to the shared PESP data directory under the user's home directory.
#' Optionally appends a relative path inside the folder.
#' 
#' @param fp_rel Optional relative path to append within the base PESP directory
#' @return
#' A character string giving the absolute path
#' @export
abs_pesp_path <- function(fp_rel = NULL) {
  # construct base path
  base_path <- file.path(Sys.getenv('USERPROFILE'), 'California Department of Water Resources', 'Phytoplankton synthesis - Documents')
  
  # if relative path is given, append it
  if (is.null(fp_rel)) {
    return(base_path)
  } else {
    return(file.path(base_path, fp_rel))
  }
}

#' @title Read-in Phyto Taxonomy Key List
#' 
#' @description
#' Read in phytoplankton taxonomy key list (based on AlgaeBase; Algal Groups from Tiffany Brown at DWR).
#'
#' @return
#' A dataframe of filtered metadata
#'
#' @importFrom readr read_csv
#' @importFrom dplyr select
#' @export
read_phyto_taxa <- function(){
  df <-
    read_quiet_csv(abs_pesp_path('Reference Documents/PhytoTaxonomy.csv')) %>%
    select(Kingdom,Phylum,Class,AlgalGroup,Genus,Species,Taxon,CurrentTaxon)
  
  return(df)
}

#' @title Read-in Metadata File for Programs
#' @description
#' Reads in the PESP metadata Excel file and filters it to include only rows for the specified program.
#' Any missing `Ending Date` values are replaced with the current system date.
#'
#' @param program_name Name of the program to filter by
#' @return
#' A dataframe of filtered metadata with no missing ending dates
#' @importFrom readxl read_xlsx
read_meta_file <- function(program_name){
  df <- read_xlsx(abs_pesp_path('Reference Documents/GroupMetadata.xlsx'), skip = 2)
  
  df <- df %>%
    subset(Survey == program_name) 
  
  df$`Ending Date`[is.na(df$`Ending Date`)] <- Sys.Date()
  
  return(df)
}

#' @title Download and Read Specific Files from an EDI Data Package
#' @description
#' Downloads specified files by name from the latest revision of an EDI data package,
#' and reads them into a named list of dataframes.
#' 
#' @param pkg_id The EDI package ID (e.g., "1017")
#' @param fnames A character vector of filenames (or fragments) to match against package entities
#'
#' @return
#' A named list of dataframes, where each name corresponds to a matched filename
#' 
#' @importFrom glue glue
#' @importFrom stringr str_detect str_c
#' @importFrom purrr map_chr map slowly rate_delay keep
#' @importFrom readr read_csv
#' @importFrom EDIutils list_data_entities read_data_entity_name
#' @export
get_edi_file <- function(pkg_id, fname) {
  # get latest revision
  revisions <- list_data_package_revisions(scope = 'edi', identifier = pkg_id)
  latest_revision <- max(as.numeric(revisions))
  package_id_str <- glue('edi.{pkg_id}.{latest_revision}')
  
  # get entity IDs
  entities <- list_data_entities(packageId = package_id_str)
  
  # slow wrapper (avoid rate limit)
  slow_read <- slowly(read_data_entity_name, rate_delay(pause = 1))
  
  # find the matching entity
  matched <- keep(entities, function(entity_id) {
    entity_name <- slow_read(packageId = package_id_str, entityId = entity_id)
    print(entity_name)
    identical(entity_name, fname)
  })
  
  if (length(matched) == 0) {
    stop(glue::glue("File '{fname}' not found in package edi.{pkg_id}.{latest_revision}"))
  }
  
  # construct download URL and read csv
  entity_id <- matched[[1]]
  file_url <- glue('https://pasta.lternet.edu/package/data/eml/edi/{pkg_id}/{latest_revision}/{entity_id}')
  df <- read_csv(file_url, guess_max = 1000000, show_col_types = FALSE)
  
  return(df)
}

# Modify Dataframe --------------------------------------------------------

#' @title Add Metadata Values to Main Dataframe by Date
#' 
#' @description
#' Expands date ranges from the metadata dataframe, then joins selected columns from that metadata
#' into the main dataframe based on matching dates.
#' 
#' @param df the main dataframe with a 'Date' column
#' @param df_meta A metadata dataframe with 'Starting Date' and optionally 'Ending Date' columns, 
#'        plus one or more columns to join
#' @param column A string specifying the name of the column in `df_meta` to join into `df`.
#' 
#' @return
#' A dataframe that contains the original `df` with additional columns from `df_meta` joined by matching date ranges
#' 
#' @importFrom dplyr mutate select left_join
#' @importFrom tidyr unnest
#' @importFrom purrr map2
#' @export
from_meta <- function(df, df_meta, column) {
  df_meta <- df_meta %>%
    mutate(start = `Starting Date`,
           end = if_else(is.na(`Ending Date`), Sys.Date(), `Ending Date`)) %>%
    mutate(Date = map2(start, end, ~ seq(from = .x, to = .y, by = 'day'))) %>%
    unnest(cols = Date) %>%
    select(Date, all_of(column))
  
  df_export <- left_join(df, df_meta, by = 'Date')
  
  return(df_export)
}

#' @title Rename Columns in a Dataframe
#'
#' @description
#' Renames dataframe columns according to a provided named vector (`rename_map`).
#' If no map is supplied, a default mapping is applied for common field names
#' (e.g., `SampleDate`, `SampleTime`, etc.). A summary of renamed
#' columns is printed.
#'
#' @param df Dataframe whose columns should be renamed
#' @param rename_map Optional named character vector of replacements, where names
#'   are new column names and values are old names (e.g., `c('New' = 'Old')`)
#'
#' @return
#' Dataframe with renamed columns. A message lists all applied renames.
#'
#' @importFrom dplyr rename
#' @export

rename_cols <- function(df, rename_map = NULL) {
  # default rename_map if none is provided
  if (is.null(rename_map)) {
    rename_map <- c(
      'Date' = 'SampleDate',
      'Time' = 'SampleTime',
      'Station' = 'StationCode',
      'SampleDepth' = 'Depth (m)',
      'GALD' = 'GALD 1',
      'PhytoForm' = 'Colony/Filament/Individual Group Code'
    )
  }
  
  df <- df %>%
    rename(!!!rename_map)
  
  bullet_message('Renamed columns:', paste0(rename_map, ' → ', names(rename_map)))

  return(df)
}

#' @title Rename Values in a Column
#'
#' @description
#' Renames values in a specified dataframe column according to a named vector
#' (`rename_map`). Prints a summary showing which values were changed and how many
#' rows were affected.
#'
#' @param df Dataframe containing the column to modify
#' @param column Unquoted column name whose values should be renamed
#' @param rename_map Named character vector of replacements, where names are new values
#'   and values are the old ones (e.g., `c('NewName' = 'OldName')`)
#'
#' @return
#' Dataframe with updated column values. A message is printed summarizing the
#' renamed pairs and number of affected rows.
#'
#' @importFrom dplyr mutate recode
#' @export
rename_values <- function(df, column, rename_map) {
  # original values
  orig_vals <- df[[column]]
  
  # create map
  rev_map <- setNames(names(rename_map), rename_map)
  
  # map renames
  df <- df %>%
    mutate(
      !!column := recode(.data[[column]], !!!rev_map)
    )
  
  changed_count <- sum(orig_vals %in% rename_map)
  total_count <- length(orig_vals)
  
  # display message
  rename_count <- paste(
    paste('Renamed values in column:', column),
    paste0('  • ', rename_map, ' → ', names(rename_map), collapse = '\n'),
    paste('Changed', changed_count, 'out of', total_count, 'values'),
    sep = '\n'
  )
  
  message(rename_count)
  
  return(df)
}

#' @title Convert DateTime to Fixed PST
#'
#' @description
#' Converts `Date` and `Time` columns from either Pacific (PST/PDT) or UTC
#' to a fixed PST timezone (`Etc/GMT+8`). Adjusts daylight saving offsets
#' automatically when converting from local Pacific time.
#'
#' @param df Dataframe containing `Date` and `Time` columns
#' @param orig_tz Character; original timezone of the data (`"PDT/PST"` or `"UTC"`) (default: `PDT/PST`)
#'
#' @return
#' Dataframe with corrected `Date` and `Time` values in fixed PST.
#' A `log` attribute is attached listing rows where times changed
#' due to timezone conversion.
#'
#' @importFrom dplyr mutate if_else filter select distinct
#' @importFrom lubridate ymd_hms with_tz force_tz as_date
#' @export
convert_to_pst <- function(df, orig_tz = 'PDT/PST') {
  # normalize and check timezone argument
  orig_tz <- toupper(trimws(orig_tz))
  valid_tz <- c('PDT/PST', 'UTC')
  if (!orig_tz %in% valid_tz) {
    stop("orig_tz must be either 'PDT/PST' or 'UTC'")
  }
  
  df <- df %>%
    mutate(
      Time_chr = as.character(Time),
      time_missing = is.na(Time_chr) | Time_chr == '',
      Time_fixed = if_else(time_missing, '00:00:00', Time_chr)
    )
  
  # choose the correct source timezone
  if (orig_tz == 'UTC') {
    df <- df %>%
      mutate(datetime_local = ymd_hms(paste(Date, Time_fixed), tz = 'UTC'))
  } else {
    df <- df %>%
      mutate(datetime_local = ymd_hms(paste(Date, Time_fixed), tz = 'America/Los_Angeles'))
  }
  
  # convert to fixed PST (Etc/GMT+8)
  df <- df %>%
    mutate(
      datetime_pst = with_tz(datetime_local, tzone = 'Etc/GMT+8'),
      Date_new = as_date(datetime_pst),
      Time_new = if_else(time_missing, NA_character_, format(datetime_pst, '%H:%M:%S'))
    )
  
  # log changed times
  log_df <- df %>%
    filter(!time_missing) %>%
    mutate(Date_orig = Date, Time_orig = Time_chr) %>%
    filter(Time_orig != Time_new | Date != Date_new) %>%
    select(Date = Date_orig) %>%
    distinct()
  
  # update and clean
  df <- df %>%
    mutate(Date = Date_new, Time = Time_new) %>%
    select(-Time_chr, -Time_fixed, -datetime_local, -datetime_pst,
           -Date_new, -Time_new, -time_missing)
  
  message('Times converted for ', nrow(log_df), ' dates.')
  attr(df, 'log') <- list(converted_times = log_df)
  
  return(df)
}

#' @title Add Latitude and Longitude Data
#'
#' @description
#' Merges latitude and longitude coordinates into a dataframe based on station
#' names. Reads station coordinates from a CSV file and joins them by `Station`.
#' Logs and reports any stations missing coordinate data.
#'
#' @param df Dataframe containing a `Station` column
#' @param fp_stations Relative file path to the station coordinate CSV (within the PESP directory)
#' @param merge_cols Character vector of column names to select from the station file
#'   (default: `c('Station', 'Latitude', 'Longitude')`)
#'
#' @return
#' Dataframe with `Latitude` and `Longitude` columns added.
#' If any stations are missing coordinates, a `log` attribute is attached listing them.
#'
#' @importFrom dplyr left_join select all_of
#' @importFrom readr cols col_character
#' @importFrom tibble tibble
#' @export
add_latlon <- function(df, fp_stations, merge_cols = c('Station', 'Latitude','Longitude')){
  # read-in station coordinates
  df_latlon <- read_quiet_csv(abs_pesp_path(fp_stations), col_types = cols(Station = col_character())) %>%
    select(all_of(merge_cols))
  
  # identify stations in df that are missing from `df_latlon`
  missing_stations <- setdiff(unique(df$Station), df_latlon$Station)
  
  # add latitude and longitude
  df <- df %>%
    left_join(df_latlon, by = 'Station')
  
  message('Added in latitude and longitude.')
  
  # log missing stations, if any
  if (length(missing_stations) > 0) {
    message('Missing latitude/longitude for ', length(missing_stations), ' station(s): ', paste(missing_stations, collapse = ', '))
    df_log <- tibble::tibble(MissingStation = missing_stations) %>% distinct()
    attr(df, 'log') <- list(missing_stations = df_log)
  }
  
  return(df)
}

#' @title Subset Columns in a Dataframe
#'
#' @description
#' Selects a defined set of columns from a dataframe and optionally removes
#' specified columns. The default list is PESP-relevant columns.
#' Prints a message listing dropped columns.
#'
#' @param df Dataframe to subset
#' @param subset_map Optional character vector of column names to retain.
#'   If `NULL`, a default set is used.
#' @param remove_cols Optional character vector of columns to exclude from
#'   the subset
#'
#' @return
#' Dataframe containing only the selected columns.
#' A message is printed summarizing any dropped columns.
#'
#' @importFrom dplyr select all_of
#' @export
subset_cols <- function(df, subset_map = NULL, remove_cols = NULL) {
  # define a default subset_map if none is provided
  if (is.null(subset_map)) {
    subset_map <- c(
      'Event_ID','Survey','Date','Time','SampleScheme','Location','Station','Latitude','Longitude','SampleMethod',
      'SampleDepth','DepthType','TowNetRadius','Lab','CountMethodSample','CountMethodTaxa','Magnification','OrigTaxon','Taxon',
      'Kingdom','Phylum','Class','AlgalGroup','Genus','Species',
      'Cells_per_mL','Units_per_mL','Biovolume_per_mL',
      'GALD','PhytoForm','QualityCheck','Debris','Notes'
    )
  }
  
  if (!is.null(remove_cols)) {
    subset_map <- setdiff(subset_map, remove_cols)
  }
  
  dropped_cols <- setdiff(names(df), subset_map)
  
  df <- df %>%
    select(all_of(subset_map))
  
  # print message
  if (length(dropped_cols) > 0) {
    message('Dropped columns: ', paste(dropped_cols, collapse = ', '))
  } else {
    message('No columns were dropped.')
  }
  
  return(df)
}

#' @title Coalesce Overlapping Columns
#'
#' @description
#' Merges pairs of equivalent or duplicate columns into a single column (first non-`NA` value).
#' Optionally removes the original redundant columns. If no mapping is provided,
#' a default map is used.
#'
#' @param df Dataframe containing columns to combine
#' @param combine_map Optional named list where each name is the new column name
#'   and each value is a character vector of columns to merge
#'
#' @return
#' Dataframe with combined columns. A message is printed summarizing merged pairs.
#'
#' @importFrom dplyr mutate coalesce select all_of
#' @export
coalesce_cols <- function(df, combine_map = NULL) {
  
  # define the default map
  if (is.null(combine_map)) {
    combine_map <- list(
      'Unit Abundance' = c('Unit Abundance (# of Natural Units)', 'Unit Abundance'),
      'Total Number of Cells' = c('Total Number of Cells', 'Number of cells per unit')
    )
  }
  
  combined <- character()
  
  # loop over each entry in the combine_map
  for (new_col in names(combine_map)) {
    cols_to_combine <- combine_map[[new_col]]
    
    df <- df %>%
      mutate(
        !!new_col := coalesce(.data[[cols_to_combine[1]]], .data[[cols_to_combine[2]]])
      )
    
    combined <- c(combined, paste(cols_to_combine[1], 'and', cols_to_combine[2]))
    
    if (new_col == cols_to_combine[1]) {
      df <- df %>%
        select(-all_of(cols_to_combine[2]))
    } else if (new_col == cols_to_combine[2]) {
      df <- df %>%
        select(-all_of(cols_to_combine[1]))
    } else {
      df <- df %>%
        select(-all_of(cols_to_combine))
    }
  }
  
  if (length(combined) > 0) {
    combine_message <- paste(
      'Coalesced columns:',
      paste0('  • ', combined, collapse = '\n'),
      sep = '\n'
    )
    message(combine_message)
  }
  
  return(df)
}

# Add Columns -------------------------------------------------------------

#' @title Add Automated Quality Control Flags
#'
#' @description
#' Scans comment text and measurement fields to generate standardized quality control
#' flags for phytoplankton or similar biological datasets. Creates or overwrites a
#' `QualityCheck` column summarizing detected issues.
#'
#' @param df A dataframe containing comment and measurement columns
#' @param comment_col Unquoted name of the comment column to scan for quality indicators (default: `'Comments'`)
#' @param key_cols Character vector of columns to group by when evaluating within-group consistency (default: `c('Date', 'Station')`)
#' @param taxa_col Name of the taxon column (default: `'Taxon'`)
#'
#' @return
#' The original dataframe with a new or updated `QualityCheck` column containing
#' space-separated QC flags (e.g., `"TallyNotMet Degraded"`). Rows with no detected
#' issues are labeled `"NoCode"`.
#'
#' @details
#' The following flags are assigned based on comment content or data checks (case-insensitive):
#'
#' - **BadData**: contains "delete"  
#' - **CrossContamination**: contains "cross contamination"  
#' - **TallyNotMet_Over5**: contains "CMT > 5", "CMNT > 5", or variants  
#' - **TallyNotMet_Under5**: contains "CMT < 5", "CMNT < 5", or variants  
#' - **TallyNotMet**: general phrases like "did not reach", "cannot meet tally", or when both over/under flags are present  
#' - **Degraded**: contains "degraded"  
#' - **PoorlyPreserved**: contains "poor preservation", "weak preservation", "fungus", or "mycelial growth"  
#' - **Obscured**: contains "obscured"  
#' - **BrokenDiatoms**: contains "broken diatoms"  
#' - **MucilaginousDetritus**: contains "mucilaginous detritus"  
#' - **UnitsExceedCells**: `Units_per_mL` greater than `Cells_per_mL`  

#' Rows with multiple issues are flagged with all applicable labels separated by spaces.
#' If no issues are found, `QualityCheck` is set to `"NoCode"`.
#'
#' @importFrom dplyr mutate case_when select
#' @importFrom tidyr unite
#' @importFrom rlang ensym !!
#' @export
add_qc_col <- function(df, comment_col = 'Comments', key_cols = c('Date', 'Station'), taxa_col = 'Taxon') {
  comment_col <- if (!is.null(comment_col)) rlang::ensym(comment_col) else NULL
  group_cols <- c(key_cols, taxa_col)
  
  # add QC flags
  if (!is.null(comment_col)) {
    df <- df %>%
      mutate(
        QC_1 = case_when(grepl('\\bdelete\\b', !!comment_col, ignore.case = TRUE) ~ 'BadData'),
        QC_2 = case_when(grepl('cross contamination', !!comment_col, ignore.case = TRUE) ~ 'CrossContamination'),
        QC_4 = case_when(grepl('CNMT>5|CNMT\\s>5|CNMT\\s>\\s5|CMT>5|CMT\\s>5|CMT\\s>\\s5|CMNT>5|CMNT\\s>5|CMNT\\s>\\s5', !!comment_col, ignore.case = TRUE) ~ 'TallyNotMet_Over5'),
        QC_5 = case_when(grepl('CNMT<5|CNMT\\s<5|CNMT\\s<\\s5|CMT<5|CMT\\s<5|CMT\\s<\\s5|CMNT<5|CMNT\\s<5|CMNT\\s<\\s5', !!comment_col, ignore.case = TRUE) ~ 'TallyNotMet_Under5'),
        QC_3 = case_when(
          grepl('did not reach|cannot meet tally|cannot meet natural unit|LessThan400Cells', !!comment_col, ignore.case = TRUE) ~ 'TallyNotMet',
          grepl('\\bCMT\\b|\\bCMNT\\b|\\bCounts the target of 400 natural units due to low\\b', !!comment_col, ignore.case = TRUE) ~ 'TallyNotMet'
        ),
        QC_6 = case_when(grepl('degraded', !!comment_col, ignore.case = TRUE) ~ 'Degraded'),
        QC_7 = case_when(grepl('poor preservation|poorly preserved|weak preservation|weakly preserved|fungus|fungal\\s+growth|mycelial\\s+growth|PoorlyPreserved', !!comment_col, ignore.case = TRUE) ~ 'PoorlyPreserved'),
        QC_8 = case_when(grepl('obscured', !!comment_col, ignore.case = TRUE) ~ 'Obscured'),
        QC_9 = case_when(grepl('many broken diatoms|broken diatoms|BrokenDiatoms', !!comment_col, ignore.case = TRUE) ~ 'BrokenDiatoms'),
        QC_10 = case_when(grepl('mucilaginous|mucilaginous detritus', !!comment_col, ignore.case = TRUE) ~ 'MucilaginousDetritus')
      )
    
    # handle combined case of both Over5 and Under5 being flagged
    df <- df %>%
      mutate(
        both_4_5 = !is.na(QC_4) & !is.na(QC_5),
        QC_3 = ifelse(both_4_5, 'TallyNotMet', QC_3),
        QC_4 = ifelse(both_4_5, NA_character_, QC_4),
        QC_5 = ifelse(both_4_5, NA_character_, QC_5)
      ) %>%
      select(-both_4_5)
    
  } else {
    # default to 'Unknown' if no comment_col
    df <- df %>%
      mutate(
        QC_1 = 'Unknown'
      )
  }

  # check if Units > Cells
  if (all(c('Units_per_mL', 'Cells_per_mL') %in% names(df))) {
    df <- df %>%
      mutate(
        QC_11 = case_when(
          !is.na(Units_per_mL) & !is.na(Cells_per_mL) & Units_per_mL > Cells_per_mL ~ 'UnitsExceedCells',
          TRUE ~ NA_character_
        )
      )
  }
  
  # collapse all QC columns into a single string, default to 'NoCode' if all are NA
  df <- df %>%
    unite(QualityCheck, starts_with('QC'), remove = TRUE, na.rm = TRUE, sep = ' ') %>%
    mutate(
      QualityCheck = case_when(
        # if only "Unknown" present (one or more times), reduce to single "Unknown"
        grepl('^\\s*(Unknown\\s*)+$', QualityCheck) ~ 'Unknown',
        
        # if "Unknown" is present but mixed with other codes, remove the "Unknown"
        grepl('\\bUnknown\\b', QualityCheck) ~ gsub('\\bUnknown\\b', '', QualityCheck) %>% trimws(),
        
        # if empty, set to "NoCode"
        QualityCheck == '' ~ 'NoCode',
        
        TRUE ~ QualityCheck
      )
    )
  
  return(df)
}

#' @title Add Debris Level from Comments
#'
#' @description
#' Scans a comment column for keywords related to detritus or sediment and creates
#' a new `Debris` column categorizing the level of debris as `"High"`, `"Moderate"`,
#' `"Low"`, or `"None"`. If no comment column is provided, all rows are labeled `"Unknown"`.
#'
#' @param df A dataframe containing a column with descriptive comments
#' @param comment_col Unquoted name of the comment column to evaluate (default: `Comments`)
#'
#' @return
#' The input dataframe with an added `Debris` column indicating debris level.
#' Possible values are `"High"`, `"Moderate"`, `"Low"`, `"None"`, or `"Unknown"`.
#'
#' @details
#' Debris levels are determined case-insensitively from the following keyword patterns:
#'
#' - **High**: "high detritus", "high sediment", "heavy detritus", "heavy sediment",
#'   "high amount of debris", "lots of debris" and typos  
#' - **Moderate**: "moderate detritus", "moderate sediment", "medium detritus", "medium sediment" and typos  
#' - **Low**: "low detritus", "low sediment", "light detritus", "light sediment" and typos  
#'
#' If multiple debris indicators are found, they are collapsed into a single value using
#' the highest detected level (e.g., `"High"` takes precedence over `"Moderate"` or `"Low"`).
#' Rows with no matching terms are labeled `"None"`.
#' 
#' @importFrom dplyr mutate case_when
#' @importFrom tidyr unite
#' @importFrom rlang ensym !!
#' @export
add_debris_col <- function(df, comment_col = 'Comments') {
  if (is.null(comment_col)) {
    df <- df %>%
      mutate(Debris = 'Unknown')
  } else {
    comment_col <- rlang::ensym(comment_col)
    
    df <- df %>%
      mutate(
        Db_1 = case_when(
          grepl('high detritus|high sediment|heavy detritus|heavy sediment|high amount of debris|high amounts of debris|lots of debris|High sedimnet|High sedimen', !!comment_col, ignore.case = TRUE) ~ 'High',
          grepl('moderate detritus|moderate sediment|moderat sediment|medium detritus|medium sediment', !!comment_col, ignore.case = TRUE) ~ 'Moderate',
          grepl('low detritus|low sediment|light detritus|light sediment', !!comment_col, ignore.case = TRUE) ~ 'Low',
          TRUE ~ NA_character_
        )
      ) %>%
      unite(Debris, starts_with('Db'), remove = TRUE, na.rm = TRUE, sep = ' ') %>%
      mutate(
        Debris = case_when(
          Debris == '' ~ 'None',
          TRUE ~ Debris
        )
      )
  }
  
  return(df)
}

#' @title Extract and Standardize Taxonomic Notes
#'
#' @description
#' Identifies and removes note-like keywords from taxon names (e.g. "cyst", "flagellate")
#' while recording them in a new `Notes` column. Also scans a
#' comment column for the same terms, combines both sources, and standardizes taxa that
#' would otherwise be left as "Unknown".
#'
#' @param df A dataframe containing taxon names and optionally a comment column
#' @param comment_col Unquoted name of the comment column to evaluate (default: `Comments`)
#' @param taxa_col Unquoted name of the taxon column to clean (default: `Taxon`)
#'
#' @return
#' The input dataframe with a new `Notes` column summarizing detected note terms.
#' The function also removes these note patterns from the taxon names and attaches
#' a `log` attribute containing:
#'
#' - **$taxa_notes**: data on taxon names where notes were removed  
#' - **$unmatched_notes**: parenthetical terms not matched to known note patterns  
#'
#' @details
#' The following note patterns are detected (case-insensitive):
#'
#' - **Cyst**: "cyst"  
#' - **Secondary**: "secondary"  
#' - **Ciliates**: "ciliates"  
#' - **GirdleView**: "girdle", "girdle view"  
#' - **FragmentedDiatoms**: "fragmented", "fragmented diatoms", "diatom fragment"  
#' - **Coccoid**: "coccoid"  
#' - **Filament**: "filament"  
#' - **Cymbelloid**: "cymbelloid", "cymelloid"  
#' - **Gomphonemoid**: "gomphonemoid"  
#' - **Flagellate**: "flagellate" 
#' 
#' Parenthetical expressions in taxon names are inspected for possible note content;
#' any unmatched content is logged for manual review. The resulting `Notes` column
#' concatenates all detected terms (space-separated) and replaces missing entries with `"NoNote"`.
#'
#' @importFrom dplyr mutate case_when filter distinct arrange select bind_cols
#' @importFrom tidyr unite
#' @importFrom purrr imap_dfc
#' @importFrom rlang ensym !!
#' @importFrom stringr str_detect str_extract str_remove_all str_squish str_c
#' @export
add_notes_col <- function(df, comment_col = 'Comments', taxa_col = 'Taxon') {
  df <- df %>% mutate(.orig_taxon = !!ensym(taxa_col))
  
  # note patterns
  note_patterns <- c(
    Cyst = '\\bcyst\\b',
    Secondary = '\\bsecondary\\b',
    Ciliates = '\\bciliates\\b',
    GirdleView = '\\bgirdle\\s*view\\b|\\bgirdle\\b',
    FragmentedDiatoms = '\\bfragment.?\\b|fragmented diatoms\\b|\\bdiatom fragments\\b|\\bdiatom fragment\\b',
    Coccoid = '\\bcoccoid\\b',
    Filament = '\\bfilament\\b',
    Cymbelloid = '\\bcymelloid\\b|\\bcymbelloid\\b',
    Gomphonemoid = '\\bgomphonemoid\\b',
    Flagellate = '\\bflagellate\\b'
  )
  
  comment_sym <- if (!is.null(comment_col)) ensym(comment_col) else NULL
  
  # create note columns
  df_notes <- imap_dfc(note_patterns, function(pattern, name) {
    col_name <- paste0('Note_', name)
    if (is.null(comment_sym)) {
      tibble(!!col_name := case_when(
        str_detect(df$.orig_taxon, regex(pattern, ignore_case = TRUE)) ~ name,
        TRUE ~ 'Unknown'
      ))
    } else {
      tibble(!!col_name := case_when(
        str_detect(df$.orig_taxon, regex(pattern, ignore_case = TRUE)) ~ name,
        str_detect(rlang::eval_tidy(comment_sym, df), regex(pattern, ignore_case = TRUE)) ~ name,
        TRUE ~ NA_character_
      ))
    }
  })
  
  df <- bind_cols(df, df_notes)
  
  taxa_sym <- ensym(taxa_col)
  
  # extract content for flagging
  df <- df %>%
    mutate(
      .paren_content = str_extract(!!taxa_sym, '\\(([^()]*)\\)') %>%
        str_remove_all('[()]') %>%
        str_squish()
    )
  
  # remove extra terms
  note_removal_pattern <- str_c('\\b(', str_c(note_patterns, collapse = '|'), ')\\b', collapse = '')
  df <- df %>%
    mutate(
      !!taxa_sym := str_remove_all(!!taxa_sym, regex(note_removal_pattern, ignore_case = TRUE)) %>%
        str_remove_all('\\(\\s*\\)') %>%  
        str_squish()
    )
  
  # normalize sole 'Unknown' taxa to 'Unknown sp.'
  df <- df %>%
    mutate(
      !!taxa_sym := case_when(
        str_detect(!!taxa_sym, regex('^\\s*unknown\\s*$', ignore_case = TRUE)) ~ 'Unknown sp.',
        TRUE ~ as.character(!!taxa_sym)
      )
    )
  
  # log taxon changes
  taxonnote_log <- df %>%
    filter(str_squish(.orig_taxon) != str_squish(!!taxa_sym)) %>%
    distinct(OldTaxon = .orig_taxon, UpdatedTaxon = as.character(!!taxa_sym)) %>%
    arrange(OldTaxon)
  
  if (nrow(taxonnote_log) > 0) {
    message('Total taxon note removals: ', nrow(taxonnote_log))
  } else {
    message('No taxon note removals found.')
  }
  
  # flag cases where parentheses content was removed but not matched
  pattern_union <- str_c(note_patterns, collapse = '|')
  df_flagged <- df %>%
    filter(!is.na(.paren_content), !str_detect(.paren_content, regex(pattern_union, ignore_case = TRUE))) %>%
    distinct(Taxon = !!taxa_sym, ParenContent = .paren_content)
  
  if (nrow(df_flagged) > 0) {
    message('Removed parenthetical notes with no known comment: ', nrow(df_flagged))
  }
  
  # drop temp columns
  df <- df %>% select(-.orig_taxon, -.paren_content)
  
  # combine note columns
  df <- df %>%
    unite(Notes, starts_with('Note_'), remove = TRUE, na.rm = TRUE, sep = ' ') %>%
    mutate(
      Notes = case_when(
        grepl('^\\s*(Unknown\\s*)+$', Notes) ~ 'Unknown',
        grepl('\\bUnknown\\b', Notes) ~ gsub('\\bUnknown\\b', '', Notes) %>% str_squish(),
        Notes == '' ~ 'NoNote',
        TRUE ~ Notes
      )
    )
  
  # attach logs
  attr(df, 'log') <- list(
    taxa_notes = taxonnote_log,
    unmatched_notes = df_flagged
  )
  
  return(df)
}

#' @title Add Metadata Column for Program
#'
#' @description
#' Adds a metadata column to a dataframe by joining it with a program-specific metadata sheet.
#' The join is performed based on the column specified by `col_name`.
#' After joining, the function prints the unique values found in the added column.
#'
#' @param df A dataframe to which the metadata column will be added
#' @param program A character string specifying the program used to select the metadata file
#' @param col_name A column name (unquoted) used to perform the join with the metadata sheet
#'
#' @return
#' A dataframe with the specified metadata column added
#'
#' @importFrom rlang enquo as_name
#' @importFrom dplyr pull
#' @export
add_meta_col <- function(df, program, col_name, match_cols = NULL){
  # read in metadata
  df_meta <- read_meta_file(program)
  col_str <- as_name(enquo(col_name))
  
  # check if col_name exists in df_meta
  if (!col_str %in% colnames(df_meta)) {
    stop(paste('Column', col_str, 'not found in metadata file for program', program))
  }
  
  # check if match_cols exist in both dfs
  if (!is.null(match_cols)) {
    missing_in_df <- match_cols[!match_cols %in% colnames(df)]
    missing_in_meta <- match_cols[!match_cols %in% colnames(df_meta)]
    
    if (length(missing_in_df) > 0) {
      stop(paste('Matching columns not found in df:', paste(missing_in_df, collapse = ', ')))
    }
    if (length(missing_in_meta) > 0) {
      stop(paste('Matching columns not found in metadata:', paste(missing_in_meta, collapse = ', ')))
    }
  }
  
  # convert date columns to Date type
  df_meta <- df_meta %>%
    mutate(
      `Starting Date` = as.Date(`Starting Date`, format = '%m/%d/%Y'),
      `Ending Date` = as.Date(`Ending Date`, format = '%m/%d/%Y')
    )
  
  # get date range in df
  df_min_date <- min(df$Date, na.rm = TRUE)
  df_max_date <- max(df$Date, na.rm = TRUE)
  
  # filter metadata to only include relevant date ranges
  df_meta <- df_meta %>%
    filter(
      `Ending Date` >= df_min_date | is.na(`Ending Date`), # keep rows that could overlap
      `Starting Date` <= df_max_date
    )
  
  # adjust the first start date and last end date
  first_start_index <- which.min(df_meta$`Starting Date`)
  df_meta$`Starting Date`[first_start_index] <- df_min_date
  
  # find the last row within the df date range
  last_valid_index <- which.max(ifelse(is.na(df_meta$`Ending Date`), df_max_date, df_meta$`Ending Date`))
  df_meta$`Ending Date`[last_valid_index] <- df_max_date
  
  # collect messages
  df[[col_str]] <- NA
  
  messages <- c(paste0('added ', col_str, ':'))
  
  # apply metadata values based on date ranges and additional matching columns
  for (i in seq_len(nrow(df_meta))) {
    meta_row <- df_meta[i, ]
    start_date <- meta_row$`Starting Date`
    end_date <- meta_row$`Ending Date`
    value <- meta_row[[col_str]]
    
    matching_rows <- df$Date >= start_date & df$Date <= end_date
    
    if (!is.null(match_cols)) {
      for (match_col in match_cols) {
        meta_value <- meta_row[[match_col]]
        # only apply additional filter if metadata has a non-NA value for this column
        if (!is.na(meta_value)) {
          matching_rows <- matching_rows & (df[[match_col]] == meta_value)
        }
      }
    }
    
    # apply to matching rows in df
    df[[col_str]][matching_rows] <- value
    
    # format message with matching criteria
    date_range <- paste0(format(start_date, '%m/%d/%Y'), ' - ', format(end_date, '%m/%d/%Y'))
    
    if (!is.null(match_cols)) {
      additional_criteria <- c()
      for (match_col in match_cols) {
        meta_value <- meta_row[[match_col]]
        if (!is.na(meta_value)) {
          additional_criteria <- c(additional_criteria, paste0(match_col, '=', meta_value))
        }
      }
      if (length(additional_criteria) > 0) {
        criteria_str <- paste0(' (', paste(additional_criteria, collapse = ', '), ')')
      } else {
        criteria_str <- ''
      }
    } else {
      criteria_str <- ''
    }
    
    messages <- c(messages, paste0('  • ', date_range, criteria_str, ': ', value))
  }
  
  message(paste(messages, collapse = '\n'))
  return(df)
}

#' @title Add Event ID Column
#'
#' @description
#' Creates a unique `Event_ID` for each sample based on survey, location, date,
#' time, depth type, and sample depth. The ID is formatted as
#' `Survey-Location-YYYYMMDD-DepthType-T#D#`, where `T#` identifies the grab
#' (time order) and `D#` identifies the depth rank within that grab.
#'
#' @param df Dataframe containing survey, location, date, time, and depth fields
#' @param loc_col Unquoted name of the location column (e.g., `Station` or `Site`)
#'
#' @return
#' Dataframe with a new `Event_ID` column added at the front
#'
#' @importFrom dplyr mutate group_by arrange dense_rank ungroup relocate select coalesce recode
#' @importFrom stringr str_replace str_trim
#' @importFrom lubridate ymd
#' @importFrom hms as_hms
#' @export
add_id_col <- function(df, loc_col) {
  loc_sym <- rlang::ensym(loc_col)
  
  df <- df %>%
    mutate(
      # keep only the piece after the last hyphen in Survey
      survey_short = str_replace(Survey, '^.*-', '') %>% str_trim(),
      # abbreviate depth type if desired
      dep = recode(DepthType,
                   'near surface' = 'NS',
                   'surface'      = 'S',
                   'bottom'       = 'B',
                   'epiphytic'    = 'E',
                   .default = DepthType
      ),
      SampleDepth = as.numeric(SampleDepth),
      # parse time to hms; NA stays NA
      .time_hms = suppressWarnings(hms::as_hms(Time))
    ) %>%
    # base groups for T# assignment
    group_by(survey_short, !!loc_sym, Date) %>%
    arrange(.time_hms, dep, SampleDepth, .by_group = TRUE) %>%
    mutate(
      grab_num  = dense_rank(coalesce(.time_hms, hms::as_hms('23:59:59'))),
      grab_code = paste0('T', grab_num)
    ) %>%
    # refine groups so D# is per (time, depth type)
    group_by(.time_hms, dep, .add = TRUE) %>%
    mutate(
      # D#: rank depths within each (Survey, Location, Date, Time, dep); NA depth last
      depth_num  = dense_rank(coalesce(SampleDepth, Inf)),
      depth_code = paste0('D', depth_num),
      Event_ID   = paste(
        survey_short,
        !!loc_sym,
        format(lubridate::ymd(Date), '%Y%m%d'),
        dep,
        paste0(grab_code, depth_code),
        sep = '-'
      )
    ) %>%
    ungroup() %>%
    relocate(Event_ID, .before = everything()) %>%
    select(-c(survey_short, .time_hms, dep, grab_num, grab_code, depth_num, depth_code))
  
  return(df)
}

# Taxa Related Functions --------------------------------------------------
#' @title Standardize Unknown Taxon Labels
#'
#' @description
#' Cleans and standardizes taxon names by normalizing "unknown" variants,
#' simplifying species suffixes, and optionally converting all `spp.` to `sp.`.
#'
#' Specifically:
#' - Replaces case-insensitive variants of "unknown", "unidentified", or "undetermined" with `"Unknown"`
#' - Converts trailing `sp. X` or `spp. X` to just `sp.` or `spp.`
#' - Optionally converts all `spp.` to `sp.` for consistency
#' - Convert `cf. Unknown x` to `Unknown x`
#'
#' @param df Dataframe containing a `Taxon` column
#' @param std_sp Logical; if `TRUE`, convert all `spp.` variants to `sp.` (default: `TRUE`)
#' @param std_suffix Logical; if `TRUE`, remove trailing descriptors (e.g., `sp. A`, `sp. (B)`) (default: `TRUE`)
#'
#' @return
#' Dataframe with cleaned `Taxon` values and a `log` attribute listing changed entries
#'
#' @importFrom stringr str_replace_all str_replace str_remove_all str_detect str_match str_squish str_to_lower
#' @importFrom dplyr case_when distinct
#' @importFrom tibble tibble
#' @export
clean_unknowns <- function(df, std_sp, std_suffix) {
  original_taxon <- df$Taxon
  
  # standardize unknown/unidentified/undetermined to "Unknown"
  unknown_syns <- 'unknown|unidentified|undetermined'
  df$Taxon <- dplyr::case_when(
    grepl(unknown_syns, df$Taxon, ignore.case = TRUE) ~ 
      stringr::str_replace_all(df$Taxon, regex(unknown_syns, ignore_case = TRUE), 'Unknown'),
    TRUE ~ df$Taxon
  )
  
  # standardize spp. -> sp.
  if (std_sp) {
    df$Taxon <- df$Taxon %>%
      str_replace_all('\\b(spp?|sp)\\b\\.?', 'sp.') %>%
      str_squish()
  }
  
  # simplify spp. X, spp X, sp. X, sp X -> sp.
  if (std_suffix) {
    df$Taxon <- df$Taxon %>%
      # remove anything in parentheses
      str_remove_all('\\s*\\([^\\)]*\\)') %>%
      # remove everything else that trails
      str_replace_all('\\bspp?\\.?\\s+.*$', 'sp.') %>%
      str_squish()
  }
  
  # if Unknown X sp., remove trailing sp.
  df$Taxon <- case_when(
    str_to_lower(df$Taxon) == 'unknown sp.' ~ 'Unknown sp.',
    str_detect(df$Taxon, regex('^unknown\\b', ignore_case = TRUE)) ~
      str_remove(df$Taxon, '\\bsp\\.$') %>% str_squish(),
    TRUE ~ df$Taxon
  )
  
  # replace 'cf. Unknown x' -> 'Unknown x'
  df$Taxon <- df$Taxon %>%
    str_replace(regex('^cf\\.\\s+(Unknown\\b.*)', ignore_case = TRUE), '\\1') %>%
    str_squish()
  
  # standardize case of "Unknown taxa"
  df$Taxon <- case_when(
    str_detect(df$Taxon, regex('^unknown \\w+$', ignore_case = TRUE)) ~ 
      str_replace(df$Taxon, regex('^unknown (\\w+)$', ignore_case = TRUE), function(m) {
        second_word <- str_match(m, regex('^unknown (\\w+)$', ignore_case = TRUE))[,2]
        paste('Unknown', tolower(second_word))
      }),
    TRUE ~ df$Taxon
  )
  
  # create log
  df_log <- tibble::tibble(
    OrigTaxon = original_taxon[original_taxon != df$Taxon],
    UpdatedTaxon = df$Taxon[original_taxon != df$Taxon]
  ) %>%
    distinct()
  
  message('Unique unknown taxon standardized: ', nrow(df_log))
  attr(df, 'log') <- list(clean_unknowns = df_log)
  return(df)
}

#' @title Correct Taxon Names Using a Typo Lookup Table
#'
#' @description
#' Corrects known taxon name typos based on an external lookup table.
#' Handles `cf.` qualifiers by temporarily removing them
#' during matching and re-inserting them afterward in their original position.
#' Also standardizes malformed `cf.` and `sp.` affixes before correction.
#'
#' Specifically:
#' - Normalizes all names to ASCII before comparison
#' - Standardizes affixes `cf`, `sp`, and `var` to consistent forms
#' - Performs exact case-insensitive matches against the typo correction table
#' - Reattaches `cf.` qualifiers to corrected names
#'
#' @param df Dataframe containing a `Taxon` column to correct
#'
#' @return
#' Dataframe with corrected `Taxon` values and a `log` attribute containing
#' a dataframe named `taxon_corrections` listing original and corrected names
#'
#' @importFrom dplyr mutate filter select distinct arrange
#' @importFrom purrr map_chr
#' @importFrom stringr str_split str_replace_all str_trim str_squish
#' @importFrom stringi stri_trans_general
#' @export
correct_taxon_typos <- function(df) {
  # read in typo lookup table and create map
  df_typos <- read_quiet_csv(abs_pesp_path('Reference Documents/TaxaTypos.csv'))
  typo_map <- setNames(
    str_squish(stri_trans_general(df_typos$TaxonCorrected, 'Latin-ASCII')),
    str_squish(stri_trans_general(df_typos$Taxon, 'Latin-ASCII'))
  )
  
  # df$Taxon <- iconv(df$Taxon, from = 'ISO-8859-1', to = 'UTF-8')
  # df$Taxon <- unname(df$Taxon)
  
  # helper to fix malformed 'cf' and 'sp'
  standardize_affix <- function(taxon) {
    taxon %>%
      str_replace_all('\\bcf[.,]?\\s+', 'cf. ') %>%
      str_replace_all('\\bvar(?!\\.)\\b', 'var.') %>%  
      str_replace_all('\\b(sp\\.\\s*){2,}$', 'sp.') %>% 
      str_replace_all('\\bsp[.,]?\\s*$', 'sp.') %>%
      str_replace_all('\\s+', ' ') %>%
      str_trim()
  }
  
  # store original for logging
  df <- df %>%
    mutate(.orig_taxon = Taxon)
  
  # normalize to ASCII
  df <- df %>%
    mutate(Taxon = str_squish(stri_trans_general(Taxon, 'Latin-ASCII')))
  
  # standardize cf/sp
  df <- df %>%
    mutate(Taxon = standardize_affix(Taxon))
  
  # correct known typos
  correct_taxon <- function(taxon) {
    words <- str_split(taxon, '\\s+')[[1]]
    cf_pos <- which(words == 'cf.')
    pure_words <- words[words != 'cf.']
    
    if (length(pure_words) == 0) return(taxon)
    
    pure_taxon <- paste(pure_words, collapse = ' ')
    
    # lookup (case-insensitive)
    typo_names <- names(typo_map)
    match_idx <- which(tolower(typo_names) == tolower(pure_taxon))
    
    if (length(match_idx) > 0) {
      corrected <- typo_map[[match_idx[1]]]
    } else {
      corrected <- pure_taxon
    }
    
    corrected_words <- str_split(corrected, '\\s+')[[1]]
    
    if (length(cf_pos) > 0) {
      for (pos in cf_pos) {
        insert_at <- min(pos, length(corrected_words) + 1)
        corrected_words <- append(corrected_words, 'cf.', after = insert_at - 1)
      }
    }
    
    str_trim(paste(corrected_words, collapse = ' '))
  }
  
  df <- df %>%
    mutate(Taxon = map_chr(Taxon, correct_taxon))
  
  # log corrections
  typo_log <- df %>%
    filter(str_squish(.orig_taxon) != str_squish(Taxon)) %>%
    distinct(OrigTaxon = .orig_taxon, UpdatedTaxon = Taxon) %>%
    arrange(OrigTaxon)
  
  if (nrow(typo_log) > 0) {
    message('Total taxon typos corrected: ', nrow(typo_log))
  } else {
    message('No taxon typos found.')
  }
  
  df <- df %>% select(-.orig_taxon)
  attr(df, 'log') <- list(taxon_corrections = typo_log)
  
  return(df)
}

#' @title Update Taxon Names To Current Synonyms
#'
#' @description
#' Resolves and updates outdated taxon names based on a synonym chain defined
#' in external phytoplankton taxonomy metadata (`read_phyto_taxa()`). Handles
#' both standard and `cf.` forms (e.g., `"Genus cf. species"`), replacing each
#' with its current accepted name while preserving the original.
#'
#' @details
#' - Synonym chains are traced iteratively until a terminal name
#'   (`CurrentTaxon == "None"`) is reached.  
#' - `cf.` names are normalized, resolved, and reconstructed in the same format.  
#' - Circular references are skipped safely.  
#' - Uses memoization for faster repeated lookups.  
#'
#' @param df Dataframe containing a `Taxon` column of names to standardize
#'
#' @return
#' Dataframe with updated `Taxon` names, a new `OrigTaxon` column showing
#' previous values (NA if unchanged), and a `log` attribute listing all
#' synonym updates.
#'
#' @importFrom dplyr mutate select left_join relocate distinct filter rename
#' @importFrom stringr str_detect str_match str_split str_trim str_replace_all
#' @importFrom memoise memoise
#' @export
update_synonyms <- function(df) {
  df_syn <- read_phyto_taxa() %>%
    select(Taxon, CurrentTaxon) %>%
    rename(PureTaxon = Taxon)
  
  # normalize function
  normalize_taxon <- function(x) {
    x <- tolower(stringr::str_trim(x))
    x <- stringr::str_replace_all(x, '\\s+', ' ')
    return(x)
  }
  
  # create normalized columns
  df_syn <- df_syn %>%
    mutate(
      PureTaxonNorm = normalize_taxon(PureTaxon),
      CurrentTaxonNorm = normalize_taxon(CurrentTaxon)
    )
  
  # synonym map (normalized keys → normalized values)
  synonym_map <- setNames(
    as.character(df_syn$CurrentTaxonNorm), 
    df_syn$PureTaxonNorm
  )
  
  # proper case map (normalized → proper capitalization)
  proper_case_map <- setNames(
    as.character(df_syn$PureTaxon),
    df_syn$PureTaxonNorm
  )
  
  # also map CurrentTaxon to proper case
  current_proper_case <- setNames(
    as.character(df_syn$CurrentTaxon),
    df_syn$CurrentTaxonNorm
  )
  proper_case_map <- c(proper_case_map, current_proper_case)
  
  # go down the synonym chain (case-insensitive)
  newest_taxon <- function(taxon_normalized) {
    seen <- character()
    current <- taxon_normalized
    
    while (!is.na(current) && current %in% names(synonym_map)) {
      next_taxon <- synonym_map[[current]]
      
      if (is.na(next_taxon) || next_taxon == 'none' || next_taxon %in% seen) {
        break
      }
      
      seen <- c(seen, current)
      current <- next_taxon
    }
    
    # return properly capitalized version
    if (current %in% names(proper_case_map)) {
      return(proper_case_map[[current]])
    }
    
    # if not in map, return as-is (shouldn't happen)
    return(current)
  }
  
  # resolve cf. and standard names
  resolve_synonym <- function(taxon) {
    if (is.na(taxon)) return(NA_character_)
    
    taxon_normalized <- normalize_taxon(taxon)
    
    # pattern 1: Genus cf. species (middle position)
    cf_middle_pattern <- '^([\\w-]+)\\s+cf\\.\\s+([\\w-]+)$'
    
    # pattern 2: cf. Genus species (front position)
    cf_front_pattern <- '^cf\\.\\s+(.+)$'
    
    if (str_detect(taxon_normalized, cf_middle_pattern)) {
      # handle "Genus cf. species"
      matches <- str_match(taxon_normalized, cf_middle_pattern)
      clean_taxon <- paste(matches[2], matches[3])  # remove cf., keep normalized
      resolved <- newest_taxon(clean_taxon)         # get properly capitalized result
      
      resolved_parts <- str_split(resolved, '\\s+')[[1]]
      resolved_genus <- resolved_parts[1]
      resolved_species <- if (length(resolved_parts) >= 2) {
        paste(resolved_parts[-1], collapse = ' ')
      } else {
        ''
      }
      
      return(str_trim(paste(resolved_genus, 'cf.', resolved_species)))
      
    } else if (str_detect(taxon_normalized, cf_front_pattern)) {
      # handle "cf. Genus species" or "cf. Genus"
      matches <- str_match(taxon_normalized, cf_front_pattern)
      clean_taxon <- matches[2]  # Everything after "cf. " (normalized)
      resolved <- newest_taxon(clean_taxon)  # Get properly capitalized result
      
      return(paste('cf.', resolved))
      
    } else {
      return(newest_taxon(taxon_normalized))
    }
  }
  
  # memoize for performance
  memo_resolve_synonym <- memoise::memoise(resolve_synonym)
  
  unique_taxa <- unique(df$Taxon)
  resolved_taxa_map <- setNames(
    vapply(unique_taxa, memo_resolve_synonym, character(1), USE.NAMES = FALSE),
    unique_taxa
  )
  
  df <- df %>%
    mutate(
      OrigTaxon = Taxon,
      Taxon = resolved_taxa_map[Taxon]
    ) %>%
    # only keep OrigTaxon if it differs from Taxon when case-normalized
    mutate(OrigTaxon = ifelse(normalize_taxon(OrigTaxon) == normalize_taxon(Taxon), NA, OrigTaxon)) %>%
    relocate(OrigTaxon, .before = Taxon)
  
  update_log <- df %>%
    filter(!is.na(OrigTaxon)) %>%
    distinct(OrigTaxon, UpdatedTaxon = Taxon)
  
  message('Unique synonym updates applied: ', nrow(update_log))
  attr(df, 'log') <- list(synonym_updates = update_log)
  
  return(df)
}

#' @title Remove Non-Phytoplankton Taxa from a Dataset
#'
#' @description
#' Removes rows from a dataframe where the \code{Taxon} matches an entry in an external
#' CSV file of known non-phytoplankton taxa. Returns the filtered dataframe and logs
#' the number and names of removed taxa.
#'
#' @param df A dataframe with a \code{Taxon} column
#' @param exclude_file Path to a CSV file with a single column named \code{Taxon}
#'        containing taxa to be excluded
#'
#' @return The filtered dataframe, with an attribute \code{'log'} containing
#'         a list named \code{non_phyto_removed}, including a character vector of
#'         removed taxa
#'
#' @importFrom readr read_csv
#' @importFrom dplyr filter distinct pull
#' @export
remove_non_phyto <- function(df) {
  exclude_taxa <- read_quiet_csv(abs_pesp_path('Reference Documents/TaxaNotPhyto.csv')) %>%
    dplyr::pull(Taxon)
  
  removed <- df %>%
    dplyr::filter(Taxon %in% exclude_taxa) %>%
    dplyr::pull(Taxon) %>%
    unique()
  
  if (length(removed) > 0) {
    message('Non-phytoplankton taxa removed: ', length(removed))
  } else {
    message('No non-phytoplankton taxa found.')
  }
  
  df_clean <- df %>%
    dplyr::filter(!Taxon %in% exclude_taxa)
  
  removed_log <- tibble::tibble(RemovedTaxon = removed)
  
  existing_log <- attr(df, 'log')
  invisible(attr(df_clean, 'log') <- c(existing_log, list(non_phyto_removed = removed_log)))
  
  return(df_clean)
}

#' @title Add Higher-Level Taxonomic Information
#'
#' @description
#' Appends hierarchical taxonomic classification fields to each record using a reference
#' taxonomy table read via `read_phyto_taxa()`. Matching is performed on a normalized
#' `PureTaxon` field derived from the `Taxon` column by:
#'
#' - Removing `"cf."` (case-insensitive)
#' - Trimming whitespace and collapsing multiple spaces
#' - Converting to lowercase for matching
#'
#' The output taxonomy fields include `Kingdom`, `Phylum`, `Class`, `AlgalGroup`,
#' `Genus`, and `Species`.  
#'
#' The behavior of the final `Taxon` column depends on `std_type`:
#'
#' - For **both** modes, taxa formatted as `"Genus Species cf."` are normalized to `"Genus cf. Species"`.
#' - For **`std_type = "program"`** (default):
#'   - The `Taxon` column remains unchanged.
#' - For **`std_type = "pesp"`**:
#'   - Taxa such as `"Genus cf. Species"` are rewritten as `"Genus sp."`.
#'   - Taxa of the form `"cf. Genus Species"` are preserved as-is.
#'   - `"cf. Genus"` entries are standardized to `"Unknown <algal group>"` using
#'     a predefined singular mapping (e.g., `"cf. Fragilaria"` → `"Unknown pennate diatom"`).
#'
#' The original name is preserved in an `OrigTaxon` column (added if missing).
#'
#' @param df A dataframe with a `Taxon` column to enrich with classification metadata.
#' @param after_col Optional column name after which to insert the taxonomy fields (e.g., `"Taxon"`).
#' @param std_type Character; either `"program"` (default) or `"pesp"`, controlling how `cf.` taxa are standardized.
#'
#' @return
#' A dataframe with additional taxa columns and a `log` attribute containing unmatched taxa
#' 
#' @details
#' - Synonym resolution is **not** handled here — this function only appends classification fields.  
#' - Matching is case-insensitive and diacritic-insensitive.  
#' - If `after_col` is supplied, taxonomy columns are relocated immediately after it.  
#' - For `std_type = "pesp"`, `AlgalGroup` field is mapped to its singular form when replacing `"cf."` taxa.
#'
#' @importFrom dplyr mutate select left_join relocate any_of distinct filter
#' @importFrom stringr str_replace_all str_trim str_replace str_detect str_squish
#' @importFrom readr read_csv
#' @importFrom stringi stri_trans_general stri_replace_all_regex stri_trim_both
#' @export
higher_lvl_taxa <- function(df, after_col = NULL, std_type) {
  std_type <- tolower(std_type)
  
  df <- df %>%
    mutate(
      Taxon = str_trim(Taxon),
      Taxon = str_squish(Taxon)
    )
  
  if (!std_type %in% c('program', 'pesp')) {
    stop("std_type must be either 'program' or 'PESP'")
  }
  
  # normalize "Genus Species cf." -> "Genus cf. Species"
  df <- df %>%
    mutate(
      Taxon = case_when(
        str_detect(Taxon, '^\\w+\\s+\\w+\\s+cf\\.?$') ~
          str_replace(Taxon, '^(\\w+)\\s+(\\w+)\\s+cf\\.?$', '\\1 cf. \\2'),
        TRUE ~ Taxon
      )
    )
  
  df <- df %>%
    mutate(Taxon = Taxon)
  
  # PESP standardization (if applicable) of Species cf.
  if (std_type == 'pesp') {
    df <- df %>%
      mutate(
        Taxon = case_when(
          str_detect(Taxon, '^\\w+\\s+cf(?:\\.|\\s)\\s*\\w+') ~
            str_replace(Taxon, '^(\\w+)\\s+cf(?:\\.|\\s)\\s*.*$', '\\1 sp.'),
          TRUE ~ Taxon
        ),
        OrigTaxon = case_when(
          str_detect(OrigTaxon, '^\\w+\\s+cf(?:\\.|\\s)\\s*\\w+') ~
            str_replace(OrigTaxon, '^(\\w+)\\s+cf(?:\\.|\\s)\\s*.*$', '\\1 sp.'),
          TRUE ~ OrigTaxon
        )
      )
  }
  
  # save OrigTaxon if not already present
  if (!'OrigTaxon' %in% names(df)) {
    df <- df %>%
      mutate(OrigTaxon = Taxon)
  }
  
  # create PureTaxon by removing 'cf.', standardizing 'sp.', and normalizing
  df <- df %>%
    mutate(
      PureTaxon = stringr::str_replace_all(Taxon, regex('cf\\.', ignore_case = TRUE), ''),
      PureTaxon = stringr::str_trim(PureTaxon),
      PureTaxon = stringr::str_replace_all(PureTaxon, '\\s+', ' '),
      PureTaxon = tolower(PureTaxon)
    )
  
  df$PureTaxon <- df$PureTaxon %>%
    str_replace_all('\\bspp?\\.?\\s+\\S+', 'sp.') %>% # replace 'sp. X' or 'spp. X' to 'sp.'
    stringr::str_replace('\\bsp\\.?$', 'sp.') %>%
    stringr::str_replace('\\bspp\\.?$', 'spp.')
    
  df$PureTaxon <- stringr::str_replace_all(df$PureTaxon, 'spp\\.', 'sp.')
  
  # read in taxa sheet and add PureTaxon
  df_taxa <- read_phyto_taxa()
  df_taxa <- df_taxa %>%
    mutate(
      PureTaxon = stringr::str_trim(Taxon),
      PureTaxon = stringr::str_replace_all(PureTaxon, '\\s+', ' '),
      PureTaxon = stringr::str_squish(stringi::stri_trans_general(PureTaxon, 'Latin-ASCII')),
      PureTaxon = tolower(PureTaxon)
    ) %>%
    select(-Taxon)
  
  # create unmatched log (distinct only)
  unmatched_log <- df %>%
    distinct(PureTaxon, Taxon) %>%
    filter(!PureTaxon %in% df_taxa$PureTaxon) %>%
    distinct(PureTaxon, Taxon)
    
  message('Unique taxa with no match in reference list: ', nrow(unmatched_log))
  
  # join higher level taxa and main dfs and relocate
  df_joined <- df %>%
    left_join(df_taxa, by = 'PureTaxon') %>%
    select(-PureTaxon)
  
  # capitalize OrigTaxon/Taxon if needed
  df_joined <- df_joined %>%
    mutate(
      Taxon = case_when(
        is.na(Taxon) ~ NA_character_,
        str_detect(Taxon, '^cf\\.\\s*[A-Z]') ~ Taxon,
        str_detect(Taxon, '^cf\\.\\s*[a-z]') ~ sub('^(cf\\.\\s*)([a-z])', '\\1\\U\\2', Taxon, perl = TRUE),
        str_detect(Taxon, '^[A-Z]') ~ Taxon,
        TRUE ~ str_to_sentence(Taxon)
      ),
      OrigTaxon = case_when(
        is.na(OrigTaxon) ~ NA_character_,
        str_detect(OrigTaxon, '^cf\\.\\s*[A-Z]') ~ OrigTaxon,
        str_detect(OrigTaxon, '^cf\\.\\s*[a-z]') ~ sub('^(cf\\.\\s*)([a-z])', '\\1\\U\\2', OrigTaxon, perl = TRUE),
        str_detect(OrigTaxon, '^[A-Z]') ~ OrigTaxon,
        TRUE ~ str_to_sentence(OrigTaxon)
      )
    )
  
  # relocate
  if (!is.null(after_col)) {
    df_joined <- df_joined %>%
      relocate(c(OrigTaxon, Taxon, Kingdom, Phylum, Class, AlgalGroup), .after = all_of(after_col)) %>%
      relocate(c(Genus, Species), .after = AlgalGroup)
  }

  # PESP standardization (if applicable) of Genus cf.
  if (std_type == 'pesp') {
    algal_singular <- c(
      'Bacteria' = 'bacteria',
      'Brown Algae' = 'brown algae',
      'Centric Diatoms' = 'centric diatom',
      'Chrysophytes' = 'chrysophyte',
      'Ciliates' = 'ciliate',
      'Coccolithophores' = 'coccolithophore',
      'Cryptophytes' = 'cryptophyte',
      'Cyanobacteria' = 'cyanobacterium',
      'Dinoflagellates' = 'dinoflagellate',
      'Euglenoids' = 'euglenoid',
      'Eustigmatophytes' = 'eustigmatophyte',
      'Flagellates' = 'flagellate',
      'Fungus' = 'fungi',
      'Green Algae' = 'green alga',
      'Haptophytes' = 'haptophyte',
      'Kathablepharids' = 'kathablepharid',
      'None' = 'none',
      'Ochrophytes' = 'ochrophyte',
      'Pennate Diatoms' = 'pennate diatom',
      'Raphidophytes' = 'raphidophyte',
      'Red Algae' = 'red alga',
      'Silicoflagellates' = 'silicoflagellate',
      'Synurophytes' = 'synurophyte',
      'Unknown' = 'sp.',
      'Xanthophytes' = 'xanthophyte'
    )
    
    # Check if algal_singular is missing any AlgalGroups from reference data
    reference_algal_groups <- unique(df_taxa$AlgalGroup)
    missing_from_mapping <- setdiff(reference_algal_groups, names(algal_singular))
    if (length(missing_from_mapping) > 0) {
      stop('algal_singular mapping missing AlgalGroup(s): ', paste(missing_from_mapping, collapse = ', '))
    }
    
    df_joined <- df_joined %>%
      mutate(
        Taxon = case_when(
          str_detect(Taxon, '^cf\\.\\s*') ~ paste('Unknown', algal_singular[AlgalGroup]),
          TRUE ~ Taxon
        ),
        Genus = case_when(
          str_detect(Taxon, '^Unknown\\s') ~ 'Unknown',
          TRUE ~ Genus
        ),
        Species = case_when(
          str_detect(Taxon, '^Unknown\\s') ~ 'Unknown',
          TRUE ~ Species
        )
      )
    
    df_joined <- df_joined %>%
      mutate(Taxon = Taxon)
  }
  
  # remove special characters
  df_joined <- df_joined %>%
    mutate(
      Taxon = stringi::stri_replace_all_regex(Taxon, '\\p{Zs}+', ' '),
      OrigTaxon = stringi::stri_replace_all_regex(OrigTaxon, '\\p{Zs}+', ' ')
    ) %>%
    mutate(
      Taxon = stringi::stri_trim_both(Taxon),
      OrigTaxon = stringi::stri_trim_both(OrigTaxon)
    ) %>%
    select(-CurrentTaxon)
  
  # create log
  existing_log <- attr(df_joined, 'log')
  attr(df_joined, 'log') <- c(existing_log, list(unmatched_taxa = unmatched_log))
  
  return(df_joined)
}

#' @title Combine Taxon Records
#'
#' @description
#' Aggregates multiple taxon records within the same sampling event that differ only
#' in size or label schema or are duplicate entries.  
#' 
#' Density columns are summed within each group. Text-based descriptor columns are merged
#' intelligently to retain relevant information.
#'
#' - If multiple distinct `OrigTaxon` values occur within a group, they are concatenated
#'   into a single semicolon-separated string. When `OrigTaxon` is missing, the
#'   corresponding `Taxon` is used instead.
#' - If higher-level classification fields (`Kingdom`, `Phylum`, `Class`) disagree
#'   within a group, the `Taxon` is replaced with `"Unknown <level>"` (e.g., `"Unknown phylum"`).
#' - Only one row per unique `(Date, Station, Taxon)` group is retained after aggregation.
#' - A log of all combined taxa and column conflicts is attached as an attribute.
#'
#' @param df A dataframe containing taxonomic records with columns such as
#'   `Date`, `Station`, `Taxon`, and optional metadata fields.
#' @param key_cols Character vector of columns used to group taxon entries  
#'   (default: `c("Date", "Station")`).
#' @param measurement_cols Character vector of numeric columns to sum  
#'   (default: `c("Biovolume_per_mL", "Units_per_mL", "Cells_per_mL")`).
#'
#' @return
#' A dataframe where:
#' - Duplicate taxon rows per event are aggregated into one
#' - Numeric measurements are summed
#' - Qualitative fields (e.g., `Notes`, `Debris`) are combined
#'
#' The returned dataframe includes a `log` attribute containing:
#' - **$combined_taxa**: dataframe of taxon groups that were merged  
#' - **$combined_conflicts**: dataframe of columns where multiple distinct values were found within a group
#'
#' @details
#' The following helper rules are used during aggregation:
#'
#' - **Notes**: unique non-"NoNote" terms merged; `"MultipleEntries"` added if >1 distinct entry  
#' - **QualityCheck**: merged similar to Notes, ignoring `"NoCode"`  
#' - **Debris**: highest category preserved (`High > Moderate > Low > None > Unknown`)  
#' - **PhytoForm**: merged unique forms alphabetically  
#' - **GALD**: takes the maximum non-missing value  
#' - **OrigTaxon**: concatenated with `"; "` if multiple distinct forms exist 
#'
#' @importFrom dplyr group_by ungroup mutate across slice all_of if_else summarize filter select count summarize
#' @importFrom tidyr pivot_longer
#' @importFrom stringr str_squish str_squish str_split
#' @importFrom purrr map
#' @importFrom stats na.omit
#' @export
combine_taxons <- function(df,
                           key_cols = c('Date','Station'),
                           measurement_cols = c('Biovolume_per_mL','Units_per_mL','Cells_per_mL')) {

  original_cols <- names(df)
  message('Combining on key_cols: ', paste(key_cols, collapse = ', '))
  
  measurement_cols <- intersect(measurement_cols, names(df))

  combine_origtaxon <- function(orig, taxon) {
    x <- str_squish(as.character(orig))
    x[x == ''] <- NA_character_
    non_na <- unique(na.omit(x))
    if (length(non_na) == 0) return(NA_character_)
    had_any_na <- any(is.na(x))
    t_val <- taxon[1]
    out <- if (had_any_na && !(t_val %in% non_na)) c(non_na, t_val) else non_na
    out_str <- paste(out, collapse = '; ')
    if (had_any_na) out_str <- sub('; ([^;]+)$', ', \\1', out_str)
    out_str
  }
  
  combine_textcol <- function(x, none_label, multi_label = NULL) {
    vals <- unique(str_squish(na.omit(x)))
    vals <- vals[vals != none_label]
    if (length(vals) == 0) return(none_label)
    toks <- unique(strsplit(paste(vals, collapse = ' '), '\\s+')[[1]])
    if (!is.null(multi_label) && length(vals) > 1) toks <- c(toks, multi_label)
    paste(toks, collapse = ' ')
  }
  
  combine_notes <- function(x) combine_textcol(x, 'NoNote', 'MultipleEntries')
  combine_qccheck <- function(x) combine_textcol(x, 'NoCode')
  combine_phytoform <- function(x) paste(sort(unique(na.omit(str_squish(x)))), collapse = ', ')
  combine_debris <- function(x) {
    vals <- unique(na.omit(str_squish(x)))
    if (!length(vals)) return(NA_character_)
    debris_order <- c('High','Moderate','Low','None','Unknown')
    vals <- vals[order(match(vals, debris_order))]
    paste(vals, collapse = ', ')
  }

  # ensure option columns exist
  for (col in c('GALD','Notes','QualityCheck','Debris','PhytoForm','OrigTaxon'))
    if (!col %in% names(df)) df[[col]] <- if (col == 'GALD') NA_real_ else NA_character_
  
  # handle K/P/C mismatches
  if (all(c('Kingdom','Phylum','Class') %in% names(df))) {
    df <- df %>%
      group_by(across(all_of(c(key_cols, 'Taxon')))) %>%
      mutate(
        .diff_dim = case_when(
          n_distinct(Class,   na.rm = TRUE) > 1 ~ 'Class',
          n_distinct(Phylum,  na.rm = TRUE) > 1 ~ 'Phylum',
          n_distinct(Kingdom, na.rm = TRUE) > 1 ~ 'Kingdom',
          TRUE ~ NA_character_
        ),
        .key_val = coalesce(
          if_else(.diff_dim == 'Class', Class,
                  if_else(.diff_dim == 'Phylum', Phylum,
                          if_else(.diff_dim == 'Kingdom', Kingdom, NA_character_))), .diff_dim),
        Taxon = if_else(!is.na(.diff_dim),
                        paste('Unknown', tolower(.key_val)),
                        Taxon)
      ) %>%
      ungroup() %>%
      select(-.diff_dim, -.key_val)
  }
  
  group_cols <- c(key_cols, 'Taxon')
  
  # log
  combine_log <- df %>%
    count(across(all_of(group_cols))) %>%
    filter(n > 1)
  
  # detect carry columns once
  carry_cols <- setdiff(
    names(df),
    c(group_cols, measurement_cols, 'GALD','Notes','QualityCheck','Debris','PhytoForm','OrigTaxon')
  )
  
  # conflict detection 
  conflict_log <- NULL
  if (length(carry_cols)) {
    conflict_log <- df %>%
      group_by(across(all_of(group_cols))) %>%
      summarise(across(all_of(carry_cols), \(x) n_distinct(x, na.rm = TRUE)), .groups = 'drop') %>%
      pivot_longer(all_of(carry_cols), names_to = 'column', values_to = 'n_distinct_values') %>%
      filter(n_distinct_values > 1)
    if (nrow(conflict_log)) {
      cols <- paste(unique(conflict_log$column), collapse = ', ')
      message(nrow(conflict_log), ' conflicts detected in unexpected columns (', cols, ')')
    }
  }
  
  # main aggregation (keep only orig cols)
  df_out <- df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(
      across(all_of(measurement_cols), \(x) sum(x, na.rm = TRUE)),
      GALD = if (all(is.na(GALD))) NA_real_ else max(GALD, na.rm = TRUE),
      Notes = combine_notes(Notes),
      QualityCheck = combine_qccheck(QualityCheck),
      Debris = combine_debris(Debris),
      PhytoForm = combine_phytoform(PhytoForm),
      OrigTaxon = combine_origtaxon(OrigTaxon, Taxon),
      across(all_of(carry_cols), \(x) dplyr::first(x)),
      .groups = 'drop'
    ) %>%
    select(any_of(original_cols))
  
  message('Taxon rows combined: ', nrow(combine_log))
  attr(df_out, 'log') <- list(combined_taxa = combine_log, combined_conflicts = conflict_log)
  df_out
}

#' @title Remove Taxonomic Classification Columns
#'
#' @description
#' Removes taxonomic hierarchy columns (e.g., `Kingdom`, `Phylum`, `Class`,
#' `AlgalGroup`, `Genus`, `Species`) from a dataframe if present.
#' Matching is case-insensitive. Prints a message listing removed columns.
#'
#' @param df Dataframe potentially containing taxonomic classification columns
#'
#' @return
#' Dataframe with the specified taxonomic columns removed.
#' A message is printed summarizing which columns were deleted.
#'
#' @importFrom dplyr select all_of
#' @export
remove_taxa_info <- function(df) {
  # define the taxa columns to remove (case-insensitive)
  taxa_cols <- c('Kingdom', 'Phylum', 'Class', 'AlgalGroup', 'Genus', 'Species')
  
  # find matching columns, ignoring case
  cols_to_remove <- names(df)[grepl(paste0('^(', paste(taxa_cols, collapse = '|'), ')$'), names(df), ignore.case = TRUE)]
  
  # remove the matching columns
  df <- df %>% dplyr::select(-all_of(cols_to_remove))
  
  # print only the columns that were actually removed
  if (length(cols_to_remove) > 0) {
    message('Removed columns: ', paste(cols_to_remove, collapse = ', '))
  } else {
    message('No matching columns to remove.')
  }
  
  return(df)
}

#' @title Extract Original Taxon Names from Program Data
#'
#' @description
#' Replaces the `Taxon` column with values from `OrigTaxon` when available
#' (and not containing semicolons), preserving original program-level taxon
#' labels before standardization. Then removes extra taxonomic formatting
#' using `remove_taxa_info()`. Used for PESP standardization.
#'
#' @param df Dataframe containing `OrigTaxon` and `Taxon` columns
#' @param taxon_original Name of the column holding the original program taxon
#'   (default: `'OrigTaxon'`)
#' @param taxon_current Name of the column holding the current standardized taxon
#'   (default: `'Taxon'`)
#'
#' @return
#' Dataframe with `Taxon` updated to reflect original program naming where applicable.
#'
#' @importFrom dplyr mutate case_when
#' @importFrom stringr str_detect
#' @export
extract_program_taxa <- function(df, taxon_original = 'OrigTaxon', taxon_current = 'Taxon') {
  df <- df %>%
    mutate(
      Taxon = case_when(
        !is.na(.data[[taxon_original]]) & !str_detect(.data[[taxon_original]], ';') ~ .data[[taxon_original]],
        TRUE ~ .data[[taxon_current]]
      )
    )
  
  df <- remove_taxa_info(df)
  
  return(df)
}