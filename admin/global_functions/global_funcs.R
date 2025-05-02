
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
#' @param fp File path to the CSV
#'
#' @return A dataframe containing the contents of the CSV file
#'
#' @importFrom readr read_csv
#' @export
read_quiet_csv <- function(fp){
  df <- suppressWarnings(readr::read_csv(fp, show_col_types = FALSE))
  
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
  fp_full <- 'California Department of Water Resources/Phytoplankton synthesis - Documents/'
  
  if (is.null(fp_rel)) {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_full))
  } else {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_full, fp_rel))
  }
  
  return(fp_abs)
}

#' @title Read Phyto Taxonomy File
#'
#' @description
#' Read in Tiffany Brown's phyto taxonomy file.
#' 
#' @return A dataframe of filtered metadata with no missing ending dates
#'
#' @importFrom readr read_csv
read_phyto_taxa <- function(){
  df <-
    readxl::read_read(abs_pesp_path('Reference Documents/PhytoTaxonomy.csv'))
  
  return(df)
}

#' @title Read Metadata File for Programs
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
    readxl::read_xlsx(abs_pesp_path('Reference Documents/GroupMetadata.xlsx'), skip = 3)
  
  df <- df %>%
    subset(Program == program_name) 
  
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
get_edi_file = function(pkg_id, fnames){
  # get revision
  revision_url = glue::glue('https://pasta.lternet.edu/package/eml/edi/{pkg_id}')
  all_revisions = readLines(revision_url, warn = FALSE) 
  latest_revision = tail(all_revisions, 1)
  
  # get entities 
  pkg_url = glue::glue('https://pasta.lternet.edu/package/data/eml/edi/{pkg_id}/{latest_revision}')
  all_entities = readLines(pkg_url, warn = FALSE)
  name_urls = glue::glue('https://pasta.lternet.edu/package/name/eml/edi/{pkg_id}/{latest_revision}/{all_entities}')
  names(all_entities) = purrr::map_chr(name_urls, readLines, warn = FALSE)
  
  # select entities that match fnames
  fname_regex = stringr::str_c(glue::glue('({fnames})'), collapse = '|')
  included_entities = all_entities[stringr::str_detect(names(all_entities), fname_regex)]
  if(length(included_entities) != length(fnames)){
    stop('Not all specified filenames are included in package')
  }
  # download data
  dfs = purrr::map(glue::glue('https://pasta.lternet.edu/package/data/eml/edi/{pkg_id}/{latest_revision}/{included_entities}'),
                   readr::read_csv, guess_max = 1000000)
  names(dfs) = names(included_entities)
  
  if (length(dfs) == 1) {
    return(dfs[[1]])
  } else {
    return(dfs)
  }
}

# Modify Dataframe --------------------------------------------------------

#' @title Add Metadata Values to Main Dataframe by Date
#'
#' Expands date ranges from the metadata dataframe, then joins selected columns from that metadata
#' into the main dataframe based on matching dates.
#'
#' @param df the main dataframe with a 'Date' column
#' @param meta_df A metadata dataframe with 'Starting Date' and optionally 'Ending Date' columns, 
#'        plus one or more columns to join
#' @param column A string specifying the name of the column in `meta_df` to join into `df`.
#'
#' @return A dataframe that contains the original `df` with additional columns from `meta_df`
#'         joined by matching date ranges
#'
#' @importFrom dplyr mutate select left_join
#' @importFrom tidyr unnest
#' @importFrom purrr map2
from_meta <- function(df, meta_df, column) {
  meta_df <- meta_df %>%
    mutate(start = `Starting Date`,
           end = if_else(is.na(`Ending Date`), Sys.Date(), `Ending Date`)) %>%
    mutate(Date = map2(start, end, ~ seq(from = .x, to = .y, by = 'day'))) %>%
    unnest(cols = Date) %>%
    select(Date, all_of(column))
  
  df_export <- left_join(df, meta_df, by = 'Date')
  
  return(df_export)
}

#' @title Add Quality Control Labels Based on Comments
#'
#' Creates a QualityCheck column based on predefined keywords in the comment column
#' and assigns QC flags based on the presence of those terms.
#' 
#' It creates intermediate QC columns (e.g., QC_1 to QC_6) and merges them into a single
#' `QualityCheck` column; the column is NA if none are found.
#'
#' @param df A dataframe that includes a comment column
#' @param comment_col The unquoted name of the comment column to scan for quality flags (e.g., `Comments`)
#'
#' @return The input dataframe with a new `QualityCheck` column summarizing the QC flags
#'
#' @details
#' The following labels are assigned based on keyword matches (case-insensitive):
#' - **BadData**: contains "delete"
#' - **TallyNotMet**: contains "did not reach", "cannot meet tally", or "cannot meet natural unit"
#' - **Degraded**: contains "degraded"
#' - **PoorlyPreserved**: contains "poor preservation", "weak preservation", "fungus", etc.
#' - **Obscured**: contains "obscured"
#' - **BrokenDiatoms**: contains "many broken diatoms"
#' - **CrossContamination**: contains "cross contamination"
#' - **MultipleSizes**: size range for taxon varies significantly
#'
#' @importFrom dplyr mutate case_when
#' @importFrom tidyr unite
#' @importFrom rlang ensym !!
add_qc_col <- function(df, comment_col) {
  comment_col <- rlang::ensym(comment_col)
  
  # Flag known QC issues from comment text
  df <- df %>%
    mutate(
      QC_1 = case_when(grepl('delete', !!comment_col, ignore.case = TRUE) ~ 'BadData'),
      QC_2 = case_when(grepl('did not reach|cannot meet tally|cannot meet natural unit', !!comment_col, ignore.case = TRUE) ~ 'TallyNotMet'),
      QC_3 = case_when(grepl('degraded', !!comment_col, ignore.case = TRUE) ~ 'Degraded'),
      QC_4 = case_when(grepl('poor preservation|poorly preserved|weak preservation|weakly preserved|fungus', !!comment_col, ignore.case = TRUE) ~ 'PoorlyPreserved'),
      QC_5 = case_when(grepl('obscured', !!comment_col, ignore.case = TRUE) ~ 'Obscured'),
      QC_6 = case_when(grepl('many broken diatoms', !!comment_col, ignore.case = TRUE) ~ 'BrokenDiatoms'),
      QC_7 = case_when(grepl('cross contamination', !!comment_col, ignore.case = TRUE) ~ 'CrossContamination')
    )
  
  # Identify taxa with multiple entries for the same Datetime and Station
  df <- df %>%
    group_by(Datetime, Station, Taxon) %>%
    mutate(
      QC_8 = if (n() > 1) 'MultipleSizes' else NA_character_
    ) %>%
    ungroup()
  
  # Collapse all QC_* columns into a single QualityCheck string
  df <- df %>%
    unite(QualityCheck, starts_with('QC'), remove = TRUE, na.rm = TRUE, sep = ' ')
  
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
add_debris_col <- function(df, comment_col){
  comment_col <- rlang::ensym(comment_col) 
  
  df <- df %>%
    mutate(
      Db_1 = case_when(
        grepl('high detritus|high sediment|heavy detritus|heavy sediment', !!comment_col, ignore.case = TRUE) ~ 'high',
        grepl('moderate detritus|moderate sediment', !!comment_col, ignore.case = TRUE) ~ 'moderate',
        grepl('low detritus|low sediment|light detritus|light sediment', !!comment_col, ignore.case = TRUE) ~ 'low'
      )
    ) %>%
    unite(Debris, starts_with('Db'), remove = TRUE, na.rm = TRUE, sep = ' ')
  
  return(df)
}

#' @title Add Metadata Column from Program-Specific Sheet
#'
#' @description
#' Adds a metadata column to a dataframe by joining it with a program-specific metadata sheet.
#' The metadata sheet is loaded using `read_meta_file(program)` and the join is performed
#' based on the column specified by `col_name`. This is a wrapper around `from_meta()`.
#'
#' @param df A dataframe to which the metadata column will be added
#' @param program A character string specifying the program used to select the metadata file
#' @param col_name A column name (unquoted) used to perform the join with the metadata sheet
#'
#' @return A dataframe with the specified metadata column added
#'
#' @importFrom rlang enquo
add_meta_col <- function(df, program, col_name){
  # read in metadata sheet
  df_meta <- read_meta_file(program)
  
  df <- from_meta(df, df_meta, {{ col_name }})
}

# Taxa Related Functions --------------------------------------------------
#' @title Standardize Species Labels and Remove Trailing Numbers
#'
#' Standardizes taxon names by:
#' - converting 'spp.' to 'sp.'
#' - removing trailing numeric labels like 'sp. 1'
#'
#' @param df A dataframe containing `Taxon` and `Species` columns to clean
#'
#' @return A dataframe with updated `Taxon` and `Species` columns
#'
#' @importFrom stringr str_replace_all str_replace
clean_sp <- function(df) {
  df$Taxon <- stringr::str_replace_all(df$Taxon, 'spp\\.', 'sp.')
  df$Species <- stringr::str_replace_all(df$Species, 'spp\\.', 'sp.')
  
  # Remove anything after 'sp.' and clean whitespace
  df$Taxon <- stringr::str_replace(df$Taxon, 'sp\\..*', 'sp.')
  df$Species <- stringr::str_replace(df$Species, 'sp\\..*', 'sp.')
  
  # Remove any trailing number preceded by a space (e.g., ' sp. 1', ' cf. 2')
  df$Taxon <- gsub('\\s+\\d+$', '', df$Taxon)
  df$Species <- gsub('\\s+\\d+$', '', df$Species)
  
  return(df)
}

#' @title Standardize Unknown Taxon Labels
#'
#' @description
#' This function standardizes taxon names by:
#' - Converting 'spp.' to 'sp.'
#' - Normalizing unknown-like terms to 'Unknown'
#' - Removing trailing numbers in taxon names
#' Logs any changes applied to the `Taxon` column.
#'
#' @param df A dataframe containing `Taxon` and `Species` columns to clean
#'
#' @return A dataframe with updated `Taxon` and `Species` columns, and a 'log' attribute tracking changes
#'
#' @importFrom stringr str_replace_all str_replace
clean_unknowns <- function(df) {
  original_taxon <- df$Taxon
  
  unknown_syns <- 'unknown|unidentified|undetermined'
  df$Taxon <- dplyr::case_when(
    grepl(unknown_syns, df$Taxon, ignore.case = TRUE) ~ stringr::str_replace_all(df$Taxon, regex(unknown_syns, ignore_case = TRUE), 'Unknown'),
    TRUE ~ df$Taxon
  )
  
  df$Taxon <- stringr::str_replace_all(df$Taxon, 'spp\\.', 'sp.')
  
  df$Taxon <- stringr::str_replace(df$Taxon, 'sp\\..*', 'sp.')
  
  df$Taxon <- gsub('\\s+\\d+$', '', df$Taxon)
  
  log_df <- tibble::tibble(
    row = which(original_taxon != df$Taxon),
    old_Taxon = original_taxon[original_taxon != df$Taxon],
    new_Taxon = df$Taxon[original_taxon != df$Taxon],
  )
  
  message('Unknown taxon standardized: ', nrow(log_df))
  attr(df, 'log') <- list(clean_unknowns = log_df)
  return(df)
}

#' @title Correct Taxon Names Using a Typo Lookup Table
#'
#' @description
#' Corrects known taxon name typos based on an external lookup table with known errors.
#' Replaces incorrect entries with their corrected forms and logs changes.
#'
#' @param df A dataframe with a Taxon column to check for typos
#' @param typo_file Path to a CSV file containing columns Taxon and TaxonCorrected
#'
#' @return A dataframe with corrected Taxon names and a 'log' attribute containing taxon_corrections
#'
#' @importFrom dplyr left_join mutate select coalesce filter
#' @importFrom readr read_csv
correct_taxon_typos <- function(df) {
  df_typos <- df_typos #read_quiet_csv('admin/global_data/common_typos.csv')
  
  df <- df %>%
    left_join(df_typos, by = 'Taxon')
  
  if ('TaxonCorrected' %in% names(df) && any(!is.na(df$TaxonCorrected))) {
    typo_log <- df %>%
      filter(!is.na(TaxonCorrected) & TaxonCorrected != Taxon) %>%
      select(Date, Station, original_Taxon = Taxon, corrected_Taxon = TaxonCorrected)
    message('Known typos corrected: ', nrow(typo_log))
  } else {
    typo_log <- tibble::tibble()
    message('No known taxon typos found.')
  }
  
  df <- df %>%
    mutate(Taxon = coalesce(TaxonCorrected, Taxon)) %>%
    select(-TaxonCorrected)
  
  attr(df, 'log') <- list(taxon_corrections = typo_log)
  return(df)
}

resolve_final_taxon <- function(taxon, df_syn) {
  current <- taxon
  while(TRUE) {
    match_row <- df_syn %>% filter(tolower(Taxon) == tolower(current))
    if (nrow(match_row) == 0 || is.na(match_row$CurrentTaxon) || tolower(match_row$CurrentTaxon) == 'none') {
      return(current)
    }
    current <- match_row$CurrentTaxon
  }
}

#' @title Update Taxon Names to Reflect Current Synonym Metadata
#'
#' @description
#' Updates outdated taxon names to their current standardized forms using a synonym chain
#' from a metadata file. Preserves the original name in a new column and logs all replacements.
#'
#' @param df A dataframe with a `Taxon` column to standardize using synonym metadata
#'
#' @return A dataframe with current Taxon values and an OrigTaxon column (if changed),
#' plus a log attribute with synonym updates
#'
#' @importFrom dplyr mutate select left_join relocate any_of
update_synonyms <- function(df) {
  df_syn <- df_syn %>%
    select(c('Taxon', 'CurrentTaxon')) %>%
    rename(PureTaxon = Taxon)
  
  synonym_map <- setNames(as.character(df_syn$CurrentTaxon), df_syn$PureTaxon)
  
  resolve_synonym <- function(taxon) {
    # handle "Genus cf. species"
    if (stringr::str_detect(taxon, '^\\w+\\s+cf\\.\\s+\\w+$')) {
      genus <- stringr::str_match(taxon, '^(\\w+)\\s+cf\\.\\s+\\w+$')[,2]
      species <- stringr::str_match(taxon, '^\\w+\\s+cf\\.\\s+(\\w+)$')[,2]
      clean_taxon <- paste(genus, species)
      resolved <- newest_taxon(clean_taxon)
      resolved_parts <- stringr::str_split(resolved, '\\s+')[[1]]
      if (length(resolved_parts) >= 2) {
        return(paste(resolved_parts[1], 'cf.', paste(resolved_parts[-1], collapse = ' ')))
      } else {
        return(paste(resolved_parts[1], 'cf.'))
      }
    } else {
      return(newest_taxon(taxon))
    }
  }
  
  newest_taxon <- function(taxon) {
    while (!is.na(synonym_map[taxon]) && synonym_map[taxon] != 'None') {
      taxon <- synonym_map[taxon]
    }
    return(taxon)
  }
  
  df <- df %>%
    mutate(
      OrigTaxon = Taxon,
      Taxon = sapply(Taxon, resolve_synonym)
    ) %>%
    mutate(OrigTaxon = ifelse(OrigTaxon == Taxon, NA, OrigTaxon)) %>%
    relocate(OrigTaxon, .before = Taxon)
  
  update_log <- df %>%
    filter(!is.na(OrigTaxon)) %>%
    select(Date, Station, OrigTaxon, UpdatedTaxon = Taxon)
  
  message('Synonym updates applied: ', nrow(update_log))
  attr(df, 'log') <- list(synonym_updates = update_log)
  return(df)
}

#' @title Add Higher-Level Taxonomic Information
#'
#' @description
#' Appends hierarchical taxonomy fields (Kingdom, Phylum, Class, AlgalGroup, Genus, Species)
#' to each record based on a reference classification table. Matching is performed using a
#' normalized `PureTaxon` field, derived from the `Taxon` column by:
#'
#' - Removing any instance of "cf." (case-insensitive)
#' - Trimming leading/trailing whitespace
#' - Collapsing multiple internal spaces
#' - Converting to lowercase
#'
#' The behavior of the final `Taxon` column differs depending on `std_type`:
#'
#' - For `std_type = "program"` (default):
#'   - The original `Taxon` is preserved (e.g., "cf. Genus Species", "Genus cf. Species")
#'
#' - For `std_type = "PESP"`:
#'   - If `Taxon` is of the form "Genus cf. Species", it is standardized to "Genus sp."
#'   - If `Taxon` is "Genus Species cf.", it is normalized to "Genus cf. Species" first
#'   - If `Taxon` is "cf. Genus Species", it is retained as-is
#'
#' In all cases, Genus and Species are assigned based on the match to the reference taxonomy table.
#'
#' @param df A dataframe with a `Taxon` column to be enriched with taxonomic metadata
#' @param after_col Column name after which to insert taxonomy fields
#' @param std_type Character; either "program" (default) or "PESP" — controls cf. handling and taxon naming
#'
#' @return A dataframe with taxonomy columns added (Kingdom, Phylum, Class, AlgalGroup, Genus, Species),
#' and a 'log' attribute containing unmatched taxon records.
#'
#' @importFrom dplyr mutate select left_join relocate any_of filter
#' @importFrom stringr str_replace_all str_trim str_detect str_replace
#' @importFrom readr read_csv
higher_lvl_taxa <- function(df, after_col, std_type = 'program') {
  std_type <- tolower(std_type)
  
  # Step 1: Normalize "Genus Species cf." → "Genus cf. Species"
  df <- df %>%
    mutate(
      Taxon = stringr::str_trim(Taxon),
      Taxon = dplyr::case_when(
        stringr::str_detect(Taxon, '^\\w+\\s+\\w+\\s+cf\\.$') ~ 
          stringr::str_replace(Taxon, '^(\\w+)\\s+(\\w+)\\s+cf\\.$', '\\1 cf. \\2'),
        TRUE ~ Taxon
      )
    )
  
  # Step 2: Apply PESP rewrite ("Genus cf. Species" → "Genus sp.") to both Taxon and OrigTaxon
  if (std_type == 'pesp') {
    df <- df %>%
      mutate(
        Taxon = dplyr::case_when(
          stringr::str_detect(Taxon, '^\\w+\\s+cf\\.\\s+\\w+$') ~
            stringr::str_replace(Taxon, '^(\\w+)\\s+cf\\.\\s+\\w+$', '\\1 sp.'),
          TRUE ~ Taxon
        ),
        OrigTaxon = dplyr::case_when(
          stringr::str_detect(OrigTaxon, '^\\w+\\s+cf\\.\\s+\\w+$') ~
            stringr::str_replace(OrigTaxon, '^(\\w+)\\s+cf\\.\\s+\\w+$', '\\1 sp.'),
          TRUE ~ OrigTaxon
        )
      )
  }

  # Step 3: Save OrigTaxon (only if it doesn't already exist)
  if (!'OrigTaxon' %in% names(df)) {
    df <- df %>%
      mutate(OrigTaxon = Taxon)
  }
  
  # Step 4: Create PureTaxon by removing 'cf.' and normalizing
  df <- df %>%
    mutate(
      PureTaxon = stringr::str_replace_all(Taxon, regex('cf\\.', ignore_case = TRUE), ''),
      PureTaxon = stringr::str_trim(PureTaxon),
      PureTaxon = stringr::str_replace_all(PureTaxon, '\\s+', ' '),
      PureTaxon = tolower(PureTaxon)
    )
  # Step 5: Prepare df_syn and join based on PureTaxon
  df_syn_clean <- df_syn %>%
    mutate(
      PureTaxon = stringr::str_trim(Taxon),
      PureTaxon = stringr::str_replace_all(PureTaxon, '\\s+', ' '),
      PureTaxon = tolower(PureTaxon)
    ) %>%
    select(-Taxon)
  
  unmatched_log <- df %>%
    filter(!PureTaxon %in% df_syn_clean$PureTaxon) %>%
    select(Date, Station, OrigTaxon, Taxon, PureTaxon)
  
  message('Taxa with no match in reference list: ', nrow(unmatched_log))
  
  # Step 6?
  df_joined <- df %>%
    left_join(df_syn_clean, by = 'PureTaxon') %>%
    select(-PureTaxon) %>%
    relocate(c(OrigTaxon, Taxon, Kingdom, Phylum, Class, AlgalGroup), .after = all_of(after_col)) %>%
    relocate(c(Genus, Species), .after = AlgalGroup)
  
  existing_log <- attr(df, 'log')
  attr(df_joined, 'log') <- c(existing_log, list(unmatched_taxa = unmatched_log))
  
  return(df_joined)
}

#' @title Combine Taxon Records That Differ by Size or Label Variation
#'
#' @description
#' Aggregates multiple taxon records per sampling event that share the same Date, Station,
#' and Taxon but may differ in size or labeling. Sums key numeric columns and logs combinations.
#'
#' @param df A dataframe containing taxonomic data with columns for Date, Station, Taxon, and numeric summary fields
#'
#' @return A dataframe where taxon rows are aggregated and a log of merged entries is attached
#'
#' @importFrom dplyr group_by ungroup mutate across distinct all_of
#' @title Combine Taxon Records That Differ by Size or Label Variation
#'
#' @description
#' Aggregates multiple taxon records per sampling event that share the same Date, Station,
#' and Taxon but may differ in size or original labeling. Key numeric columns (e.g., Biovolume_per_mL)
#' are summed within each group. If multiple values of `OrigTaxon` exist, they are collapsed into
#' a single semicolon-separated string. If `OrigTaxon` is `NA`, the corresponding `Taxon` value is used instead.
#' Only one row per unique group is retained after aggregation. A log of combined entries is attached as an attribute.
#'
#' @param df A dataframe containing taxonomic data with columns for Date, Station, Taxon, OrigTaxon, and numeric fields
#'
#' @return A dataframe where taxon rows are aggregated and a 'combined_taxa' log is attached as an attribute
#'
#' @importFrom dplyr group_by ungroup mutate across slice all_of if_else summarise filter
combine_taxons <- function(df) {
  sum_cols <- c('Biovolume_per_mL', 'Units_per_mL', 'Cells_per_mL')
  sum_cols <- intersect(sum_cols, names(df))
  
  # Identify combinations
  combine_log <- df %>%
    group_by(Date, Station, Taxon) %>%
    summarise(n_combined = n(), .groups = 'drop') %>%
    filter(n_combined > 1)
  
  df <- df %>%
    group_by(Date, Station, Taxon) %>%
    mutate(
      .combine_group = n() > 1,
      .group_taxon = Taxon[1],
      across(all_of(sum_cols), ~ sum(.x, na.rm = TRUE), .names = '{.col}')
    ) %>%
    mutate(
      OrigTaxon = if (.combine_group[1]) {
        paste(
          sort(unique(
            ifelse(is.na(OrigTaxon), .group_taxon, OrigTaxon)
          )),
          collapse = '; '
        )
      } else {
        ifelse(is.na(OrigTaxon[1]), Taxon[1], OrigTaxon[1])
      }
    ) %>%
    slice(1) %>%  # keep only one row per group
    ungroup() %>%
    select(-.combine_group, -.group_taxon)
  
  message('Taxon rows combined: ', nrow(combine_log))
  attr(df, 'log') <- list(combined_taxa = combine_log)
  return(df)
}
