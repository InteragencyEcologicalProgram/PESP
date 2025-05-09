
# Check Variables ---------------------------------------------------------

# # check distinct rows
check_distinct <- function(df, return_df = FALSE, coerce = TRUE,
                           type = c('full', 'key_measure_cols', 'key_cols'),
                           key_cols = c('Station','Date'),
                           taxa_cols = c('Taxon'),
                           measurement_cols = c('Biovolume_per_mL','Cells_per_mL','Units_per_mL')) {
  type <- match.arg(type)
  df_check <- df
  
  if (coerce) {
    df_check <- df_check %>%
      mutate(
        across(where(is.factor), as.character),
        across(where(is.numeric), ~ round(.x, 6))
      )
  }
  
  if (type == 'full') {
    n_total <- nrow(df_check)
    n_unique <- nrow(distinct(df_check))
    n_non_distinct <- n_total - n_unique
    
    if (n_non_distinct > 0) {
      df_dupes <- df[duplicated(df_check) | duplicated(df_check, fromLast = TRUE), ]
      message('Number of non-distinct rows: ', nrow(df_dupes))
      
      if (isTRUE(return_df)) {
        return(df_dupes)
      } else {
        attr(df, 'log') <- list(nondistinct_allrows = df_dupes)
        return(df)
      }
    } else {
      message('All rows are unique.')
      attr(df, 'log') <- list(nondistinct_allrows = NULL)
      return(invisible(df))
    }
    
  } else if (type == 'key_measure_cols') {
    if (is.null(key_cols) || is.null(measurement_cols)) {
      stop('For type = "key_measure_cols", both key_cols and measurement_cols must be provided.')
    }
    
    # find non-distinct keys
    nondistinct_keys <- df_check %>%
      group_by(across(all_of(c(key_cols, taxa_cols, measurement_cols)))) %>%
      summarise(n = n(), .groups = 'drop') %>%
      filter(n > 1) %>%
      select(all_of(c(key_cols, taxa_cols, measurement_cols)))
    
    # extract all original rows that match these non-distinct keys
    df_dupes <- df %>%
      semi_join(nondistinct_keys, by = c(key_cols, taxa_cols, measurement_cols))
    
    n_inconsistent <- nrow(df_dupes)
    
    if (n_inconsistent > 0) {
      message('Number of inconsistent key+taxa+measurement rows: ', n_inconsistent)
      attr(df, 'log') <- list(nondistinct_keymeasurerows = df_dupes)
      if (isTRUE(return_df)) {
        return(df_dupes)
      } else {
        return(df)
      }
    } else {
      message('All key+taxa+measurement column combinations are unique.')
      attr(df, 'log') <- list(nondistinct_keymeasurerows = NULL)
      return(invisible(df))
    }
    
  } else if (type == 'key_cols') {
    # find non-distinct keys
    nondistinct_keys <- df_check %>%
      group_by(across(all_of(c(key_cols, taxa_cols)))) %>%
      summarise(n = n(), .groups = 'drop') %>%
      filter(n > 1) %>%
      select(all_of(c(key_cols, taxa_cols)))
    
    # extract all original rows that match these non-distinct keys
    df_dupes <- df %>%
      semi_join(nondistinct_keys, by = c(key_cols, taxa_cols))
    
    n_non_distinct <- nrow(df_dupes)
    
    if (n_non_distinct > 0) {
      message('Number of non-distinct key+taxa rows: ', n_non_distinct)
      attr(df, 'log') <- list(nondistinct_keyrows = df_dupes)
      if (isTRUE(return_df)) {
        return(df_dupes)
      } else {
        return(df)
      }
    } else {
      message('All key+taxa combinations are unique.')
      attr(df, 'log') <- list(nondistinct_keyrows = NULL)
      return(invisible(df))
    }
  }
}

extract_unstandardized_comments <- function(df, comment_col, delimiter = ' ') {
  comment_col <- rlang::ensym(comment_col)
  
  # normalize common delimiters to regex equivalents
  if (!is.null(delimiter) && delimiter != '') {
    delimiter <- dplyr::case_when(
      delimiter == '. '  ~ '\\.\\s*',
      delimiter == '.'   ~ '\\.',
      delimiter == ', '  ~ ',\\s*',
      delimiter == ','   ~ ',',
      delimiter == '; '  ~ ';\\s*',
      delimiter == ';'   ~ ';',
      delimiter == ' - ' ~ '\\s+-\\s*',
      TRUE               ~ delimiter
    )
  }
  
  # phrases captured by QualityCheck or Debris
  known_phrases <- c(
    'delete',
    'cross contamination',
    'did not reach',
    'cannot meet tally',
    'cannot meet natural unit',
    'CNMT>5', 'CNMT >5', 'CNMT > 5',
    'CMT>5',  'CMT >5',  'CMT > 5',
    'CMNT<5', 'CMNT <5', 'CMNT < 5',
    'CMT<5',  'CMT <5',  'CMT < 5',
    'CMT', 'CMNT','LessThan400cells',
    'degraded',
    'poor preservation', 'poorly preserved', 'PoorlyPreserved',
    'weak preservation', 'weakly preserved',
    'fungus',
    'fungal growth',
    'mycelial growth',
    'obscured',
    'many broken diatoms', 'broken diatoms', 'BrokenDiatoms',
    'high detritus', 'high sediment',
    'moderate detritus', 'moderate sediment','moderat sediment',
    'low detritus', 'low sediment',
    'light detritus', 'light sediment',
    'heavy detritus', 'heavy sediment',
    'Good'
  )
  
  # compile regex pattern
  known_pattern <- paste0(
    '(', 
    paste0(stringr::str_replace_all(known_phrases, '(\\W)', '\\\\\\1'), collapse = '|'), 
    ')'
  )
  
  raw_comments <- df %>%
    dplyr::pull(!!comment_col) %>%
    unique() %>%
    discard(is.na)
  
  cleaned_comments <- stringr::str_remove_all(raw_comments, regex(known_pattern, ignore_case = TRUE))
  
  # isolate unmatched fragments
  if (!is.null(delimiter) && delimiter != '') {
    unmatched <- cleaned_comments %>%
      map(~ stringr::str_split(.x, delimiter)[[1]]) %>%
      unlist() %>%
      stringr::str_trim() %>%
      stringr::str_remove('\\.$') %>%
      discard(~ .x == '' || is.na(.x))
  } else {
    unmatched <- cleaned_comments %>%
      stringr::str_trim() %>%
      discard(~ .x == '' || is.na(.x))
  }
  
  unmatched_df <- unique(unmatched) %>%
    tibble::tibble(Unmatched = .)
  
  if (nrow(unmatched_df) > 0) {
    message('Unique unstandardized comments: ', nrow(unmatched_df))
    attr(df, 'log')$unmatched_comments <- unmatched_df
  }
  
  return(df)
}

# Check Taxa --------------------------------------------------------------

# # check higher lvl taxa

check_higher_taxa <- function(df){
  df_error_check <- df %>% select(c('Taxon':'Species'))
  
  check <- df_error_check %>% subset(is.na(AlgalGroup) | is.na(Class) | is.na(Phylum) | is.na(Kingdom) | is.na(Genus) | is.na(Species))
  check <- check %>% mutate(Taxon = gsub('cf\\. ', '', Taxon),
                            Taxon = gsub(' var\\..*', '', Taxon))
  check <- check[!duplicated(check),]
  check <- check %>% arrange(Taxon)  
  
  if (nrow(check) > 0){
    warning(glue('Taxon missing higher level classifications:\n{toString(unique(check$Taxon)\n)}\nEither update official list or fix name(s)'))
  } else {
    message('All higher level classifications added.')
  }
  
  return(check)
}

# # check synonyms

check_synonyms <- function(df){
  df_syn <- read_quiet_csv('admin/global_data/phyto_classifications.csv') %>%
    select(c('Kingdom':'AlgalGroup','Taxon','CurrentTaxon'))
  
  changed_taxon <- unique(df_syn$Taxon[df_syn$CurrentTaxon != 'None'])
  
  multigen <- changed_taxon[changed_taxon %in% unique(df_syn$CurrentTaxon)]
  
  df_output <- data.frame(multigen) %>%
    mutate(Type = 'multigen')
  
  if (length(multigen) > 0){
    warning_one <- glue('Warning: multi-generational taxon names in synonym dataframe: {toString(multigen)}.')
  } else {
    warning_one <- ''
  }
  
  df_error_check <- df %>% select(c('Taxon':'Species'))
  
  syn_check <- df_error_check %>% subset(is.na(Taxon) | Taxon == 'Unknown')
  syn_check <- syn_check[!duplicated(syn_check),]
  syn_check <- syn_check %>% arrange(Taxon)
  
  if (nrow(syn_check) > 0){
    message(glue('{warning_one} \n Warning: Taxon missing synonym data: {toString(unique(syn_check$Taxon)\n)}\n Either update official list or fix name(s)'))
  } else {
    message(glue('{warning_one} \n All synonyms added.'))
  }
  
  syn_check <- syn_check %>%
    mutate(Type = 'missing syn')
  
  df_output <- full_join(df_output, syn_check, by = 'Type')
  
  return(df_output)
}


# Check No Organisms Observed ---------------------------------------------

check_nodata <- function(df) {
  df_nodat <- df %>% filter(Taxon == 'No organisms observed')
  if (nrow(df_nodat) == 0) {
    print('No data observed for: None')
  } else {
    message <- paste0(
      'No data observed for: ',
      paste(df_nodat$Station, df_nodat$Date, sep = ' ', collapse = '; ')
    )
    print(message)
  }
}

# Check Plots -------------------------------------------------------------

# # Plot NMDS
create_nmds <- function(df, group_var, nmds_var, taxa_var = 'Taxon', factor_var = NULL, show_legend = TRUE, color_palette = NULL) {
  set.seed(42)

  if (!is.null(factor_var)) {
    group_var <- rlang::sym(group_var)
    nmds_var <- rlang::sym(nmds_var)
    taxa_var <- rlang::sym(taxa_var)
    factor_var <- rlang::sym(factor_var)
    
    df_cells <- df %>%
      select(!!group_var, !!factor_var, !!taxa_var, !!nmds_var) %>%
      group_by(!!group_var, !!factor_var, !!taxa_var) %>%
      reframe(!!nmds_var := mean(!!nmds_var, na.rm = TRUE)) %>%
      pivot_wider(names_from = !!taxa_var, values_from = !!nmds_var)
    
    com <- df_cells %>%
      select(-!!group_var, -!!factor_var)
    com[is.na(com)] <- 0
    m_com <- as.matrix(com)
    
  } else {
    factor_var <- rlang::sym(group_var)
    nmds_var <- rlang::sym(nmds_var)
    taxa_var <- rlang::sym(taxa_var)
    
    df_cells <- df %>%
      select(!!factor_var, !!taxa_var, !!nmds_var) %>%
      group_by(!!factor_var, !!taxa_var) %>%
      reframe(!!nmds_var := mean(!!nmds_var, na.rm = TRUE)) %>%
      pivot_wider(names_from = !!taxa_var, values_from = !!nmds_var)
    
    com <- df_cells %>%
      select(-!!factor_var)
    com[is.na(com)] <- 0
    m_com <- as.matrix(com)
  }
  
  nmds <- NULL
  invisible(capture.output({
    nmds <- metaMDS(m_com, distance = 'bray')
  }))
  df_nmds <- as.data.frame(scores(nmds)$sites)
  
  df_nmds <- df_nmds %>%
    mutate(!!factor_var := df_cells %>% pull(!!factor_var)) %>%
    mutate(!!factor_var := as.factor(!!factor_var))
  
  plt_nmds <- ggplot(df_nmds, aes(x = NMDS1, y = NMDS2, group = !!factor_var)) + 
    geom_point(size = 4, shape = 21, color = '#000000', aes(fill = !!factor_var)) +
    labs(title = paste('NMDS of', nmds_var, 'by', group_var)) +
    theme_bw()
  
  if (!is.null(color_palette)) {
    plt_nmds <- plt_nmds + scale_fill_manual(values = color_palette)
  }
  
  if (!show_legend) {
    plt_nmds <- plt_nmds + theme(legend.position = 'none')
  }
    
  return(list(
    df_nmds = df_nmds,
    plot = plt_nmds,
    raw_nmds = nmds
  ))
}

check_units_cells <- function(df) {
  if ('Units_per_mL' %in% colnames(df) && 'Cells_per_mL' %in% colnames(df)) {
    
    # Check that Cells_per_mL >= Units_per_mL for every row pair
    count_issues <- df %>%
      filter(Cells_per_mL < Units_per_mL)
    
    if (nrow(count_issues) == 0) {
      message('(Correct) Cells_per_mL is greater than or equal to Units_per_mL for all rows.')
    } else {
      log_df <- count_issues
      message('(Warning) Found ', nrow(count_issues), ' row where Cells_per_mL is less than Units_per_mL.')
      
      # Add the QC code 'CountIssues' to the QualityCheck column
      df <- df %>%
        mutate(QualityCheck = case_when(
          row_number() %in% rownames(count_issues) & QualityCheck != 'NoCode' ~ paste(QualityCheck, 'CountIssues'),
          row_number() %in% rownames(count_issues) & QualityCheck == 'NoCode' ~ 'CountIssues',
          TRUE ~ QualityCheck
        ))
      
      attr(df, 'log') <- list(cell_calc_issue = log_df)
    }
  } else {
    message('One or both of the required columns ("Units_per_mL", "Cells_per_mL") are missing.')
  }
  
  return(df)
}



# Plot BSA Check ----------------------------------------------------------

plot_bsa_check <- function(df_data,
                           taxon = 'Microcystis aeruginosa',
                           cell_col = 'Total Number of Cells',
                           unit_col = 'Unit Abundance (# of Natural Units)',
                           x_range = NULL,
                           y_range = NULL) {
  
  # convert string column names to symbols
  cell_sym <- sym(cell_col)
  unit_sym <- sym(unit_col)
  
  # filter and compute difference
  df_micro <- df_data %>%
    mutate(Diff = !!cell_sym - !!unit_sym)
  
  # define shaded regions
  df_shade <- data.frame(
    xmin = as.Date(c('2013-09-01', '2013-11-01', '2014-01-01', '2021-03-01')),
    xmax = as.Date(c('2013-09-30', '2013-11-30', '2020-12-31', '2021-10-31'))
  )
  
  # define vertical lines
  vlines <- as.Date(c('2013-09-01','2013-09-30','2013-11-01','2013-11-30','2014-01-01','2020-12-31','2021-03-01','2021-10-31'))
  
  # build plot
  plt <- ggplot(df_micro, aes(x = Date, y = Diff)) +
    geom_rect(data = df_shade, aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = Inf),
              inherit.aes = FALSE, fill = 'gray85', alpha = 0.5) +
    geom_point(aes(color = Diff < 0)) +
    scale_color_manual(values = c('FALSE' = 'steelblue', 'TRUE' = 'red'), guide = 'none') +
    geom_hline(yintercept = 0, linetype = 'solid', color = 'black') +
    geom_vline(xintercept = vlines, linetype = 'dashed', color = 'gray40') +
    labs(title = paste('Differences for',taxon), x = 'Date', y = 'TotalCells - UnitAbundance') +
    theme_minimal()
  
  # apply zoom
  if (!is.null(x_range) || !is.null(y_range)) {
    plt <- plt + coord_cartesian(xlim = as.Date(x_range), ylim = y_range)
  }
  
  return(plt)
}


check_bsa_issue <- function(df,
                            cell_col = 'Total Number of Cells',
                            unit_col = 'Unit Abundance (# of Natural Units)') {
  # convert string column names to symbols
  cell_sym <- sym(cell_col)
  unit_sym <- sym(unit_col)
  
  # filter and compute difference
  df_out <- df %>%
    mutate(Diff = !!cell_sym - !!unit_sym) %>%
    filter(!is.na(Diff) & Diff < 0)
  
  # determine if any rows were < 0
  n <- nrow(df_out)
  
  if (n > 0) {
    message('Total cells < unit abundance in ', n, 
            ifelse(n == 1, ' case.', ' cases.'))
    
    # create log of issues (Taxon + Diff details)
    log_df <- df_out %>%
      distinct()
    
    attr(df_out, 'log') <- list(bsa_issue = log_df)
    return(df_out)
  } else {
    message('No instances found where total cells < than unit abundance.')
    return(invisible(NULL))
  }
}

check_nas <- function(df, exclude_cols = 'OrigTaxon') {
  # Identify columns to check for partial NAs
  cols_to_check_partial <- if (!is.null(exclude_cols)) {
    setdiff(names(df), exclude_cols)
  } else {
    names(df)
  }
  
  # Check for partial NAs
  df_check_partial <- df[, cols_to_check_partial, drop = FALSE]
  na_cols_partial <- names(which(colSums(is.na(df_check_partial)) > 0))
  
  # Check for fully missing columns (including excluded columns)
  na_cols_full <- names(which(colSums(is.na(df)) == nrow(df)))
  
  # Create log as a tibble
  log_df <- tibble::tibble(MissingInColumn = na_cols_partial) %>% distinct()
  
  # Print messages
  if (length(na_cols_partial) > 0) {
    message('Missing values found in ', length(na_cols_partial), ' column(s): ', paste(na_cols_partial, collapse = ', '))
  } else {
    message('No missing values found (excluding ', paste(exclude_cols, collapse = ', '), ').')
  }
  
  if (length(na_cols_full) > 0) {
    message('All values are missing in ', length(na_cols_full), ' column(s): ', paste(na_cols_full, collapse = ', '))
  } else {
    message('No column has all missing values.')
  }
  
  # Attach log to df
  attr(df, 'log') <- list(na_check = log_df)
  
  return(df)
}

unique_check <- function(df, col) {
  col_sym <- rlang::ensym(col)
  unique_vals <- unique(df %>% dplyr::pull(!!col_sym))
  message('Unique ', rlang::as_string(col_sym), 's: ', paste(unique_vals, collapse = ', '))
}
