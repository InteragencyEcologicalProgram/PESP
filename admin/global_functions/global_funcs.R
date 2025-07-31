# Useful Functions --------------------------------------------------------

#' @title Not In Operator
#'
#' @description
#' Logical negation of `%in%`, returns `TRUE` for elements of `x` that are not in `y`.
#'
#' @param x Vector of values to test
#' @param y Vector of values to compare against
#'
#' @return A logical vector indicating if elements of `x` are not in `y`
#' @export
'%!in%' <- function(x,y)!('%in%'(x,y))

#' @title Read CSV Quietly
#'
#' @description
#' Reads a CSV file using `readr::read_csv()` without printing column type messages or warnings.
#'
#' @param fp filepath to the CSV
#'
#' @return A dataframe containing the contents of the CSV file
#'
#' @importFrom readr read_csv
#' @export
read_quiet_csv <- function(fp, ...){
  df <- suppressWarnings(readr::read_csv(fp, show_col_types = FALSE, ...))
  
  return(df)
}

#' @title Absolute Path to PESP Data Folder
#'
#' @description
#' Constructs an absolute path to the shared PESP data directory under the user's home directory.
#' Optionally appends a relative path inside the folder.
#'
#' @param fp_rel Optional relative path to append within the base PESP directory
#'
#' @return A character string giving the absolute path
#'
#' @export
abs_pesp_path <- function(fp_rel = NULL) {
  base_path <- file.path(Sys.getenv('USERPROFILE'), 'California Department of Water Resources', 'Phytoplankton synthesis - Documents')
  
  if (is.null(fp_rel)) {
    return(base_path)
  } else {
    return(file.path(base_path, fp_rel))
  }
}

#' @title Read Phyto Taxonomy File
#'
#' @description
#' Read in Tiffany Brown's phytoplankton taxonomy file.
#' 
#' @return A dataframe of filtered metadata with no missing ending dates
#'
#' @importFrom readr read_csv
read_phyto_taxa <- function(){
  df <-
    read_quiet_csv(abs_pesp_path('Reference Documents/PhytoTaxonomy.csv')) %>%
    select(Kingdom,Phylum,Class,AlgalGroup,Genus,Species,Taxon,CurrentTaxon)
  
  return(df)
}

#' @title Read-in Metadata File for Programs
#'
#' @description
#' Reads in the PESP metadata Excel file and filters it to include only rows for the specified program.
#' Any missing `Ending Date` values are replaced with the current system date.
#'
#' @param program_name Name of the program to filter by (must match a value in the 'Program' column)
#'
#' @return A dataframe of filtered metadata with no missing ending dates
#'
#' @importFrom readxl read_xlsx
#' @importFrom dplyr subset
read_meta_file <- function(program_name){
  df <-
    readxl::read_xlsx(abs_pesp_path('Reference Documents/GroupMetadata.xlsx'), skip = 2)
  
  df <- df %>%
    subset(Survey == program_name) 
  
  df$`Ending Date`[is.na(df$`Ending Date`)] <- Sys.Date()
  
  return(df)
}

#' @title Download and Read Specific Files from an EDI Data Package
#'
#' @description
#' Downloads specified files by name from the latest revision of an EDI data package,
#' and reads them into a named list of dataframes.
#'
#' @param pkg_id The EDI package ID (e.g., "1017")
#' @param fnames A character vector of filenames (or fragments) to match against package entities
#'
#' @return A named list of dataframes, where each name corresponds to a matched filename
#'
#' @importFrom glue glue
#' @importFrom stringr str_detect str_c
#' @importFrom purrr map_chr map
#' @importFrom readr read_csv
#' @export
get_edi_file <- function(pkg_id, fname) {
  # get latest revision
  revisions <- EDIutils::list_data_package_revisions(scope = 'edi', identifier = pkg_id)
  latest_revision <- max(as.numeric(revisions))
  package_id_str <- glue::glue('edi.{pkg_id}.{latest_revision}')
  
  # get entity IDs
  entities <- EDIutils::list_data_entities(packageId = package_id_str)
  
  # slow wrapper (avoid rate limit)
  slow_read <- purrr::slowly(EDIutils::read_data_entity_name, purrr::rate_delay(pause = 1))
  
  # find the matching entity
  matched <- purrr::keep(entities, function(entity_id) {
    entity_name <- slow_read(packageId = package_id_str, entityId = entity_id)
    print(entity_name)
    identical(entity_name, fname)
  })
  
  if (length(matched) == 0) {
    stop(glue::glue("File '{fname}' not found in package edi.{pkg_id}.{latest_revision}"))
  }
  
  # construct download URL and read csv
  entity_id <- matched[[1]]
  file_url <- glue::glue('https://pasta.lternet.edu/package/data/eml/edi/{pkg_id}/{latest_revision}/{entity_id}')
  df <- readr::read_csv(file_url, guess_max = 1000000, show_col_types = FALSE)
  
  return(df)
}

# Modify Dataframe --------------------------------------------------------

#' @title Add Metadata Values to Main Dataframe by Date
#'
#' Expands date ranges from the metadata dataframe, then joins selected columns from that metadata
#' into the main dataframe based on matching dates.
#'
#' @param df the main dataframe with a 'Date' column
#' @param df_meta A metadata dataframe with 'Starting Date' and optionally 'Ending Date' columns, 
#'        plus one or more columns to join
#' @param column A string specifying the name of the column in `df_meta` to join into `df`.
#'
#' @return A dataframe that contains the original `df` with additional columns from `df_meta`
#'         joined by matching date ranges
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

#' @title Add Quality Control Labels Based on Comments
#'
#' Creates a `QualityCheck` column based on predefined keywords in a comment column and on
#' within-group inconsistencies in taxon-level entries. Flags help identify records needing review.
#'
#' Intermediate columns (QC_1 to QC_10) are generated for specific conditions and collapsed
#' into a single `QualityCheck` string per row. The column is NA if no flags are triggered.
#'
#' @param df A dataframe that includes a column with comments and taxonomic data
#' @param comment_col The unquoted name of the comment column to scan for quality issues (e.g., `Comments`)
#' @param key_cols Character vector of columns to group by before checking taxon-level variation (default: `c('Date', 'Station')`)
#' @param taxa_col Name of the taxon column (default: `'Taxon'`)
#'
#' @return The original dataframe with a new `QualityCheck` column combining quality control flags
#'
#' @details
#' The following quality flags are assigned (case-insensitive):
#' - **BadData**: contains "delete"
#' - **CrossContamination**: contains "cross contamination"
#' - **TallyNotMet_Over5**: contains "CMT > 5", "CMNT > 5", or similar
#' - **TallyNotMet_Under5**: contains "CMT < 5", "CMNT < 5", or similar
#' - **TallyNotMet**: general phrases like "did not reach", "cannot meet tally", or isolated "CMT"/"CMNT"
#' - **Degraded**: contains "degraded"
#' - **PoorlyPreserved**: contains "poor preservation", "weak preservation", "fungus", "fungal growth", "mycelial growth"
#' - **Obscured**: contains "obscured"
#' - **BrokenDiatoms**: contains "many broken diatoms"
#' - **MultipleSizes**: same taxon appears more than once in a group but has differing values in other fields
#' - **CountIssues**: Units_per_mL > Cells_per_mL
#'
#' If both TallyNotMet_Over5 and TallyNotMet_Under5 are flagged for a row, they are merged into a single **TallyNotMet** flag.
#'
#' @importFrom dplyr mutate case_when group_by ungroup across all_of
#' @importFrom tidyr unite
#' @importFrom rlang ensym !!
#' @export
add_qc_col <- function(df, comment_col = 'Comments', key_cols = c('Date', 'Station'), taxa_col = 'Taxon') {
  comment_col <- if (!is.null(comment_col)) rlang::ensym(comment_col) else NULL
  group_cols <- c(key_cols, taxa_col)
  
  # Add QC flags
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
    
    # Handle combined case of both Over5 and Under5 being flagged
    df <- df %>%
      mutate(
        both_4_5 = !is.na(QC_4) & !is.na(QC_5),
        QC_3 = ifelse(both_4_5, 'TallyNotMet', QC_3),
        QC_4 = ifelse(both_4_5, NA_character_, QC_4),
        QC_5 = ifelse(both_4_5, NA_character_, QC_5)
      ) %>%
      select(-both_4_5)
    
  } else {
    # Default to 'Unknown' if no comment_col
    df <- df %>%
      mutate(
        QC_1 = 'Unknown'
      )
  }

  # Identify taxa with multiple entries per group that differ in any field
  df <- df %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(
      QC_11 = {
        # check for "sp" or "spp" in taxa columns
        if (any(grepl('\\bsp\\b|\\bspp\\b', .data[[taxa_col]], ignore.case = TRUE))) {
          NA_character_
        } else {
          # check for multiple sizes in non-group columns
          non_group_cols <- setdiff(names(pick(everything())), group_cols)
          if (n() > 1 && any(sapply(pick(everything())[non_group_cols], function(x) length(unique(x)) > 1))) {
            'MultipleSizes'
          } else {
            NA_character_
          }
        }
      }
    ) %>%
    ungroup()
  
  # Check if Units > Cells
  if (all(c('Units_per_mL', 'Cells_per_mL') %in% names(df))) {
    df <- df %>%
      mutate(
        QC_12 = case_when(
          !is.na(Units_per_mL) & !is.na(Cells_per_mL) & Units_per_mL > Cells_per_mL ~ 'UnitsExceedCells',
          TRUE ~ NA_character_
        )
      )
  }
  
  # Collapse all QC columns into a single string, default to 'NoCode' if all are NA
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


#' @title Add Debris Category Based on Comments
#'
#' Creates a `Debris` column based on predefined keywords indicating the level
#' of detritus and/or sediment. It assigns a category of 'high', 'moderate', 'low'.
#' If both detritus and sediment exists, uses the higher-level keyword.
#'
#' @param df A dataframe that includes a column with comments
#' @param comment_col The unquoted name of the comment column to evaluate
#'
#' @return The input dataframe with a new `Debris` column
#'
#' @details
#' The following labels are assigned based on keyword matches (case-insensitive):
#' - **high**: "high detritus", "high sediment", "heavy detritus", "heavy sediment"
#' - **moderate**: "moderate detritus", "moderate sediment"
#' - **low**: "low detritus", "low sediment", "light detritus", "light sediment"
#'
#' @importFrom dplyr mutate case_when
#' @importFrom tidyr unite
#' @importFrom rlang ensym !!
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

#' @title Add Metadata Column from Program-Specific Sheet
#'
#' @description
#' Adds a metadata column to a dataframe by joining it with a program-specific metadata sheet.
#' The metadata sheet is loaded using `read_meta_file(program)`, and the join is performed
#' based on the column specified by `col_name`. This is a wrapper around `from_meta()`.
#' After joining, the function prints the unique values found in the added column.
#'
#' @param df A dataframe to which the metadata column will be added
#' @param program A character string specifying the program used to select the metadata file
#' @param col_name A column name (unquoted) used to perform the join with the metadata sheet
#'
#' @return A dataframe with the specified metadata column added
#'
#' @importFrom rlang enquo as_name
#' @importFrom dplyr pull
#' @export
add_meta_col <- function(df, program, col_name, match_cols = NULL){
  # Read in metadata sheet
  df_meta <- read_meta_file(program)
  col_str <- rlang::as_name(rlang::enquo(col_name))
  
  # Check if col_name exists in df_meta
  if (!col_str %in% colnames(df_meta)) {
    stop(paste('Column', col_str, 'not found in metadata file for program', program))
  }
  
  # Check if match_cols exist in both dataframes
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
  
  # Convert date columns to Date type for comparison
  df_meta <- df_meta %>%
    mutate(
      `Starting Date` = as.Date(`Starting Date`, format = '%m/%d/%Y'),
      `Ending Date` = as.Date(`Ending Date`, format = '%m/%d/%Y')
    )
  
  # Get the actual date range in df
  df_min_date <- min(df$Date, na.rm = TRUE)
  df_max_date <- max(df$Date, na.rm = TRUE)
  
  # Filter metadata to only include relevant date ranges
  df_meta <- df_meta %>%
    filter(
      `Ending Date` >= df_min_date | is.na(`Ending Date`), # keep rows that could overlap
      `Starting Date` <= df_max_date
    )
  
  # Adjust the first start date and last end date
  first_start_index <- which.min(df_meta$`Starting Date`)
  df_meta$`Starting Date`[first_start_index] <- df_min_date
  
  # Find the last row within the df date range
  last_valid_index <- which.max(ifelse(is.na(df_meta$`Ending Date`), df_max_date, df_meta$`Ending Date`))
  df_meta$`Ending Date`[last_valid_index] <- df_max_date
  
  # Initialize new column
  df[[col_str]] <- NA
  
  # Collect messages
  messages <- c(paste0('added ', col_str, ':'))
  
  # Apply metadata values based on date ranges and additional matching columns
  for (i in seq_len(nrow(df_meta))) {
    meta_row <- df_meta[i, ]
    start_date <- meta_row$`Starting Date`
    end_date <- meta_row$`Ending Date`
    value <- meta_row[[col_str]]
    
    # Start with date-based matching
    matching_rows <- df$Date >= start_date & df$Date <= end_date
    
    # Add additional column matching criteria
    if (!is.null(match_cols)) {
      for (match_col in match_cols) {
        meta_value <- meta_row[[match_col]]
        # Only apply additional filter if metadata has a non-NA value for this column
        if (!is.na(meta_value)) {
          matching_rows <- matching_rows & (df[[match_col]] == meta_value)
        }
      }
    }
    
    # Apply to matching rows in df
    df[[col_str]][matching_rows] <- value
    
    # Format message with matching criteria
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

# Taxa Related Functions --------------------------------------------------
#' @title Standardize Unknown Taxon Labels
#'
#' @description
#' Standardizes taxon names in a dataframe by:
#' - Replacing case-insensitive variants of "unknown", "unidentified", or "undetermined" with `"Unknown"`
#' - Converting trailing `sp. X` or `spp. X` to just `sp.` or `spp.`
#' - Optionally converting all `spp.` to `sp.` for consistency
#'
#' A log of changes to the `Taxon` column is returned as an attribute.
#'
#' @param df A dataframe containing a `Taxon` column
#' @param std_sp Logical; if `TRUE`, standardizes `spp.` to `sp.` (default: `TRUE`)
#'
#' @return A dataframe with updated `Taxon` values. The dataframe includes a `log` attribute listing unique `Taxon` values that were changed.
#'
#' @importFrom stringr str_replace_all str_replace
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
#' Corrects known taxon name typos based on an external lookup table. The function
#' handles taxon names that include the qualifier \code{"cf."} by temporarily removing it
#' during matching and re-inserting it afterward at its original position. Replacements are made
#' using an exact match against a supplied typo correction table. The output includes a log of
#' corrected entries where the underlying base name (excluding \code{"cf."}) was actually changed.
#'
#' @param df A dataframe containing a \code{Taxon} column with taxonomic names to correct
#'
#' @return A dataframe with corrected \code{Taxon} values. An attribute \code{'log'} is attached
#' containing a dataframe named \code{taxon_corrections}, which lists the original and corrected names
#' (including both \code{cf.} and non-\code{cf.} variants when applicable).
#'
#' @importFrom dplyr mutate filter select distinct arrange
#' @importFrom purrr map_chr
#' @importFrom stringr str_split str_trim
#' @export

correct_taxon_typos <- function(df) {
  df_typos <- read_quiet_csv(abs_pesp_path('Reference Documents/TaxaTypos.csv'))
  typo_map <- setNames(
    str_squish(stringi::stri_trans_general(df_typos$TaxonCorrected, 'Latin-ASCII')),
    str_squish(stringi::stri_trans_general(df_typos$Taxon, 'Latin-ASCII'))
  )
  
  # df$Taxon <- iconv(df$Taxon, from = 'ISO-8859-1', to = 'UTF-8')
  # df$Taxon <- unname(df$Taxon)
  
  # Helper to fix malformed 'cf' and 'sp'
  standardize_cf_sp <- function(taxon) {
    taxon %>%
      str_replace_all('\\bcf[.,]?\\s+', 'cf. ') %>%
      str_replace_all('\\b(sp\\.\\s*){2,}$', 'sp.') %>% 
      str_replace_all('\\bsp[.,]?\\s*$', 'sp.') %>%
      str_replace_all('\\s+', ' ') %>%
      str_trim()
  }
  
  # Store original for logging
  df <- df %>%
    mutate(.orig_taxon = Taxon)
  
  # Step 1: Normalize to ASCII
  df <- df %>%
    mutate(Taxon = str_squish(stringi::stri_trans_general(Taxon, 'Latin-ASCII')))
  
  # Step 2: Standardize cf/sp
  df <- df %>%
    mutate(Taxon = standardize_cf_sp(Taxon))
  
  # Step 3: Correct known typos
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
  
  # Log corrections
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

#' @title Update Taxon Names to Reflect Current Synonym Metadata
#'
#' @description
#' Updates outdated or deprecated taxon names based on a synonym chain defined in an external
#' metadata file. The function resolves all synonym chains, including those involving `cf.` notation,
#' and replaces each name with the most current terminal taxon.
#' The original name is retained in a new `OrigTaxon` column, and changes are recorded in a log attribute.
#'
#' @param df A dataframe with a `Taxon` column containing the taxon names to standardize
#'
#' @return A dataframe with:
#' - Updated `Taxon` values reflecting the most current names
#' - An `OrigTaxon` column indicating the original names (NA if unchanged)
#' - A `log` attribute (`$synonym_updates`) listing unique updates
#'
#' @details
#' - Synonym chains are resolved iteratively until reaching a terminal name (`CurrentTaxon == "None"`).
#' - Names in the form `Genus cf. species` are normalized, resolved, and reconstructed in the same form.
#' - Circular references are avoided using a safeguard mechanism.
#'
#' @importFrom dplyr mutate select left_join relocate distinct filter
#' @importFrom stringr str_detect str_match str_split str_trim
#' @importFrom memoise memoise
#' @export
update_synonyms <- function(df) {
  df_syn <- read_phyto_taxa() %>%
    select(Taxon, CurrentTaxon) %>%
    rename(PureTaxon = Taxon)
  
  synonym_map <- setNames(as.character(df_syn$CurrentTaxon), df_syn$PureTaxon)
  
  # Go down the synonym chain
  newest_taxon <- function(taxon) {
    seen <- character()
    while (!is.na(taxon) && taxon %in% names(synonym_map)) {
      next_taxon <- synonym_map[[taxon]]
      if (is.na(next_taxon) || next_taxon == 'None' || next_taxon %in% seen) break
      seen <- c(seen, taxon)
      taxon <- next_taxon
    }
    return(taxon)
  }
  
  # Resolve cf. and standard names
  resolve_synonym <- function(taxon) {
    # find cf (only before species name)
    cf_pattern <- '^([\\w-]+)\\s+cf\\.\\s+([\\w-]+)$'
    
    # resolve taxon
    if (str_detect(taxon, cf_pattern)) {
      matches <- str_match(taxon, cf_pattern)
      clean_taxon <- paste(matches[2], matches[3])
      resolved <- newest_taxon(clean_taxon)
      resolved_parts <- str_split(resolved, '\\s+')[[1]]
      
      resolved_genus <- resolved_parts[1]
      resolved_species <- if (length(resolved_parts) >= 2) {
        paste(resolved_parts[-1], collapse = ' ')
      } else {
        ''
      }
      return(str_trim(paste(resolved_genus, 'cf.', resolved_species)))
    } else {
      return(newest_taxon(taxon))
    }
  }
  
  # improve performance
  memo_resolve_synonym <- memoise::memoise(resolve_synonym)
  
  unique_taxa <- unique(df$Taxon)
  resolved_taxa_map <- setNames(
    vapply(unique_taxa, memo_resolve_synonym, character(1)),
    unique_taxa
  )
  
  df <- df %>%
    mutate(
      OrigTaxon = Taxon,
      Taxon = resolved_taxa_map[Taxon]
    ) %>%
    mutate(OrigTaxon = ifelse(OrigTaxon == Taxon, NA, OrigTaxon)) %>%
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
#' Appends hierarchical taxonomy fields to each record using a reference classification table
#' obtained via `read_phyto_taxa()`. Matching is based on a normalized `PureTaxon` field,
#' derived from the `Taxon` column by:
#'
#' - Removing "cf." (case-insensitive)
#' - Trimming whitespace and collapsing multiple spaces
#'
#' The behavior of the final `Taxon` column depends on `std_type`:
#' 
#' - For both, taxa in the form "Genus Species cf." are normalized to "Genus cf. Species"
#'
#' - For `std_type = "program"` (default):
#'   - The `Taxon` column remains unchanged
#'
#' - For `std_type = "pesp"`:
#'   - Taxa like "Genus cf. Species" are rewritten as "Genus sp."

#'   - Taxa of the form "cf. Genus Species" are preserved as-is
#'
#' The original name is preserved in the `OrigTaxon` column (added if missing).
#'
#' @param df A dataframe with a `Taxon` column to enrich with classification metadata
#' @param after_col Column name after which to insert the new taxonomy fields (e.g., "Taxon")
#' @param std_type Character; either `"program"` (default) or `"pesp"` — controls how `cf.` taxa are standardized
#'
#' @return A dataframe with additional columns:
#' - `OrigTaxon`, `Taxon`, `Kingdom`, `Phylum`, `Class`, `AlgalGroup`, `Genus`, `Species`
#'
#' Includes a `log` attribute containing a `unmatched_taxa` dataframe of `PureTaxon` values not found in the reference list.
#'
#' @importFrom dplyr mutate select left_join relocate any_of distinct filter
#' @importFrom stringr str_replace_all str_trim str_replace str_detect
#' @importFrom readr read_csv
higher_lvl_taxa <- function(df, after_col = NULL, std_type) {
  std_type <- tolower(std_type)
  
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
  
  if (!is.null(after_col)) {
    df_joined <- df_joined %>%
      relocate(c(OrigTaxon, Taxon, Kingdom, Phylum, Class, AlgalGroup), .after = all_of(after_col)) %>%
      relocate(c(Genus, Species), .after = AlgalGroup)
  }
  
  # PESP standardization (if applicable) of Genus cf.
  if (std_type == 'pesp') {
    # define plural -> singular Algal Group map
    algal_singular <- c(
      'Centric Diatoms' = 'centric diatom',
      'Cyanobacteria' = 'cyanobacterium',
      'Pennate Diatoms' = 'pennate diatom',
      'Cryptophytes' = 'cryptophyte',
      'Euglenoids' = 'euglenoid',
      'Dinoflagellates' = 'dinoflagellate',
      'Green Algae' = 'green alga',
      'Haptophytes' = 'haptophyte',
      'Chrysophytes' = 'chrysophyte',
      'Synurophytes' = 'synurophyte',
      'Ciliates' = 'ciliate',
      'Raphidophytes' = 'raphidophyte'
    )
    
    # check for unknown Algal Groups in cf. Taxa
    unknown_algalgroups <- df_joined %>%
      filter(str_detect(Taxon, '^cf\\.\\s+\\w+(\\s+\\w+)?$')) %>%
      pull(AlgalGroup) %>%
      unique() %>%
      setdiff(names(algal_singular))
    
    if (length(unknown_algalgroups) > 0) {
      stop('Unknown AlgalGroup(s) in cf. taxa: ', paste(unknown_algalgroups, collapse = ', '))
    }
    
    # apply replacements
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

#' @title Combine Taxon Records That Differ by Size or Label Variation
#'
#' @description
#' Aggregates multiple taxon records per sampling event (same `Date`, `Station`, and `Taxon`)
#' that may differ due to size distinctions or inconsistent labeling.
#' Measurement columns (eg. `Biovolume_per_mL`, `Units_per_mL`, `Cells_per_mL`) are summed within each group.
#'
#' - If multiple distinct `OrigTaxon` values are found, they are combined into a single
#'   semicolon-separated string. If `OrigTaxon` is `NA`, the corresponding `Taxon` value is used instead.
#' - Only one row per group is retained after aggregation.
#' - A log of all merged taxon groups is returned as an attribute.
#'
#' @param df A dataframe containing taxonomic records with columns for `Date`, `Station`, `Taxon`,
#' optionally `OrigTaxon`, and numeric summary fields
#' @param key_cols Character vector of columns used to group taxon entries (default: `c("Date", "Station")`)
#' @param measurement_cols Character vector of numeric columns to sum (default: `c("Biovolume_per_mL", "Units_per_mL", "Cells_per_mL")`)
#'
#' @return A dataframe with summed numeric values and standardized `OrigTaxon` entries.
#' A log of combined taxon rows is attached as an attribute under `$combined_taxa`.
#'
#' @importFrom dplyr group_by ungroup mutate across slice all_of if_else summarize filter select
#' @importFrom stats na.omit
combine_taxons <- function(df, key_cols = c('Date', 'Station'), measurement_cols = c('Biovolume_per_mL', 'Units_per_mL', 'Cells_per_mL')) {
  measurement_cols <- intersect(measurement_cols, names(df))
  
  group_cols <- c(key_cols, 'OrigTaxon', 'Taxon')
  
  combine_log <- df %>%
    group_by(across(all_of(group_cols))) %>%
    filter(n() > 1) %>%
    ungroup()
  
  df <- df %>%
    group_by(across(all_of(group_cols))) %>%
    mutate(
      across(all_of(measurement_cols), ~ sum(.x, na.rm = TRUE), .names = '{.col}')
    ) %>%
    slice(1) %>%
    ungroup()
  
  if('GALD' %in% names(df)) {
    df <- df %>%
      group_by(across(all_of(group_cols))) %>%
      mutate(GALD = ifelse(all(is.na(GALD)), NA_real_, max(GALD, na.rm = TRUE))) %>%
      slice(1) %>%
      ungroup()
  }
  
  message('Taxon rows combined: ', nrow(combine_log))
  attr(df, 'log') <- list(combined_taxa = combine_log)
  
  return(df)
}

# Write log file
write_log_file <- function(df_log, fp) {
  if (exists('df_log') && is.data.frame(df_log) && nrow(df_log) > 0) {
    write_csv(df_log, abs_pesp_path(fp))
  }
}

# Add latlon
add_latlon <- function(df, fp_stations, merge_cols = c('Station', 'Latitude','Longitude')){
  # Read station coordinates
  df_latlon <- read_quiet_csv(abs_pesp_path(fp_stations), col_types = cols(Station = col_character())) %>%
    select(merge_cols)
  
  # Identify stations in df that are missing from df_latlon
  missing_stations <- setdiff(unique(df$Station), df_latlon$Station)
  
  # Add latitude and longitude
  df <- df %>%
    left_join(df_latlon, by = 'Station')

  message('Added in latitude and longitude.')
  
  # Log missing stations if any
  if (length(missing_stations) > 0) {
    message('Missing latitude/longitude for ', length(missing_stations), ' station(s): ', paste(missing_stations, collapse = ', '))
    df_log <- tibble::tibble(MissingStation = missing_stations) %>% distinct()
    attr(df, 'log') <- list(missing_stations = df_log)
  }
  
  return(df)
}

# rename cols
rename_cols <- function(df, rename_map = NULL) {
  # Define a default rename_map if none is provided
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
  
  # Rename columns based on the map
  df <- df %>%
    dplyr::rename(!!!rename_map)
  
  # Generate message with all rename pairs as bullet points
  rename_message <- paste(
    'Renamed columns:',
    paste0('  • ', rename_map, ' → ', names(rename_map), collapse = '\n'),
    sep = '\n'
  )
  
  message(rename_message)
  
  return(df)
}

# subset cols
subset_cols <- function(df, subset_map = NULL, remove_cols = NULL) {
  # Define a default subset_map if none is provided
  if (is.null(subset_map)) {
    subset_map <- c(
      'Survey','Date','Time','SampleScheme','Location','Station','Latitude','Longitude','SampleMethod',
      'SampleDepth','DepthType','TowNetRadius','Lab','CountMethodSample','CountMethodTaxa','Magnification','OrigTaxon','Taxon',
      'Kingdom','Phylum','Class','AlgalGroup','Genus','Species',
      'Cells_per_mL','Units_per_mL','Biovolume_per_mL',
      'GALD','PhytoForm','QualityCheck','Debris','Notes'
    )
  }
  
  # Remove specified columns from the subset_map if provided
  if (!is.null(remove_cols)) {
    subset_map <- setdiff(subset_map, remove_cols)
  }
  
  # Identify dropped columns
  dropped_cols <- setdiff(names(df), subset_map)
  
  # Reorder and select only the desired columns
  df <- df %>%
    select(all_of(subset_map))
  
  # Print a message if any columns were dropped
  if (length(dropped_cols) > 0) {
    message('Dropped columns: ', paste(dropped_cols, collapse = ', '))
  } else {
    message('No columns were dropped.')
  }
  
  return(df)
}

# Combine cols
combine_cols <- function(df, combine_map = NULL) {
  # Define the default combine_map if none is provided
  if (is.null(combine_map)) {
    combine_map <- list(
      'Unit Abundance' = c('Unit Abundance (# of Natural Units)', 'Unit Abundance'),
      'Total Number of Cells' = c('Total Number of Cells', 'Number of cells per unit')
    )
  }
  
  combined <- character()  # To store the columns that were combined
  
  # Loop over each entry in the combine_map
  for (new_col in names(combine_map)) {
    cols_to_combine <- combine_map[[new_col]]
    
    # Ensure there are exactly two columns to combine
    if (length(cols_to_combine) == 2) {
      # Combine the two columns using coalesce
      df <- df %>%
        mutate(
          !!new_col := coalesce(.data[[cols_to_combine[1]]], .data[[cols_to_combine[2]]])
        )
      
      # Add the new column to the combined list
      combined <- c(combined, paste(cols_to_combine[1], "and", cols_to_combine[2]))
      
      # Remove the columns based on the new column name
      if (new_col == cols_to_combine[1]) {
        # If new_col is the first column, remove the second column
        df <- df %>%
          select(-all_of(cols_to_combine[2]))
      } else if (new_col == cols_to_combine[2]) {
        # If new_col is the second column, remove the first column
        df <- df %>%
          select(-all_of(cols_to_combine[1]))
      } else {
        # If new_col is neither of the columns, remove both
        df <- df %>%
          select(-all_of(cols_to_combine))
      }
    }
  }
  
  # Generate message indicating which columns were combined
  if (length(combined) > 0) {
    combine_message <- paste(
      'Combined columns:',
      paste0('  • ', combined, collapse = '\n'),
      sep = '\n'
    )
    message(combine_message)
  }
  
  return(df)
}

# Remove old taxa info
remove_taxa_info <- function(df) {
  # Define the taxa columns to remove (case-insensitive)
  taxa_cols <- c('Kingdom', 'Phylum', 'Class', 'AlgalGroup', 'Genus', 'Species')
  
  # Find matching columns, ignoring case
  cols_to_remove <- names(df)[grepl(paste0('^(', paste(taxa_cols, collapse = '|'), ')$'), names(df), ignore.case = TRUE)]
  
  # Remove the matching columns
  df <- df %>% dplyr::select(-all_of(cols_to_remove))
  
  # Print only the columns that were actually removed
  if (length(cols_to_remove) > 0) {
    message('Removed columns: ', paste(cols_to_remove, collapse = ', '))
  } else {
    message('No matching columns to remove.')
  }
  
  return(df)
}


# Convert to PST from PST/PDT

convert_to_pst <- function(df) {
  df <- df %>%
    mutate(
      Time_chr = as.character(Time),
      time_missing = is.na(Time_chr) | Time_chr == '',
      Time_fixed = if_else(time_missing, '00:00:00', Time_chr),
      datetime_local = ymd_hms(paste(Date, Time_fixed), tz = 'America/Los_Angeles'),
      datetime_pst = with_tz(datetime_local, tzone = 'Etc/GMT+8'),
      Date_new = as_date(datetime_pst),
      Time_new = if_else(time_missing, NA_character_, format(datetime_pst, '%H:%M:%S'))
    )
  
  # create log of where Time changed (i.e., PDT shifted to PST)
  log_df <- df %>%
    filter(!time_missing) %>%
    mutate(Date_orig = Date, Time_orig = Time_chr) %>%
    filter(Time_orig != Time_new | Date != Date_new) %>%
    select(Date = Date_orig) %>%
    distinct()
  
  # update the dataframe
  df <- df %>%
    mutate(
      Date = Date_new,
      Time = Time_new
    ) %>%
    select(-Time_chr, -Time_fixed, -datetime_local, -datetime_pst,
           -Date_new, -Time_new, -time_missing)
  
  message('Times converted for ', nrow(log_df), ' dates.')
  attr(df, 'log') <- list(converted_times = log_df)
  return(df)
}

# Extract orig taxon from formatted program Taxons for PESP Taxons
extract_program_taxons <- function(df, taxon_original = 'OrigTaxon', taxon_current = 'Taxon') {
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

# Rename values in column
rename_values <- function(df, column, rename_map) {
  orig_vals <- df[[column]]
  
  rev_map <- setNames(names(rename_map), rename_map)
  
  df <- df %>%
    dplyr::mutate(
      !!column := dplyr::recode(.data[[column]], !!!rev_map)
    )
  
  changed_count <- sum(orig_vals %in% rename_map)
  total_count <- length(orig_vals)
  
  rename_count <- paste(
    paste('Renamed values in column:', column),
    paste0('  • ', rename_map, ' → ', names(rename_map), collapse = '\n'),
    paste('Changed', changed_count, 'out of', total_count, 'values'),
    sep = '\n'
  )
  
  message(rename_count)
  
  return(df)
}