
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

#' @title Extract unstandardized comments from a dataframe
#'
#' @description
#' Identifies and extracts comment text fragments that do not match a predefined
#' set of known quality or debris phrases. Normalizes delimiters, removes known
#' phrases, and appends unmatched comment fragments to the dataframe's log
#' attribute under `unmatched_comments`.
#'
#' @param df a dataframe containing comment text
#' @param comment_col unquoted column name containing comments to parse
#' @param delimiter character string giving the delimiter used to separate comment fragments (default `' '`)
#'
#' @return the input dataframe with an updated `'log'` attribute containing a tibble of unmatched comment fragments (if any)
#'
#' @importFrom rlang ensym
#' @importFrom dplyr pull case_when
#' @importFrom stringr str_remove_all str_replace_all str_split str_trim
#' @importFrom purrr map discard
#' @importFrom tibble tibble
#'
#' @export
extract_unstandardized_comments <- function(df, comment_col, delimiter = ' ') {
  comment_col <- ensym(comment_col)
  
  # normalize common delimiters to regex equivalents
  if (!is.null(delimiter) && delimiter != '') {
    delimiter <- case_when(
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
    'girdle','girdle view',
    'cyst',
    'mucilaginous detritus',
    'ciliates','ciliates observed',
    'many broken diatoms', 'broken diatoms', 'BrokenDiatoms',
    'high detritus', 'high sediment',
    'moderate detritus', 'moderate sediment','moderat sediment',
    'low detritus', 'low sediment',
    'light detritus', 'light sediment',
    'heavy detritus', 'heavy sediment', 'high amount of debris',
    'high amounts of debris', 'lots of debris', 'High sedimnet', 'High sedimen',
    'fragment.', 'fragmented diatoms', 'diatom fragments', 'diatom fragment',
    'Good',
    'and'
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
  
  cleaned_comments <- str_remove_all(raw_comments, regex(known_pattern, ignore_case = TRUE))
  
  # isolate unmatched fragments
  if (!is.null(delimiter) && delimiter != '') {
    unmatched <- cleaned_comments %>%
      map(~ stringr::str_split(.x, delimiter)[[1]]) %>%
      unlist() %>%
      stringr::str_trim() %>%
      stringr::str_remove('\\.$') %>%
      stringr::str_remove('\\;$') %>%
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
create_mcom <- function(df, group_var, measure_var, taxa_var = 'Taxon', 
                      factor_var = NULL, avg_var = NULL, compute_dist = FALSE, distance = NULL) {
  set.seed(42)
  
  message(
    '\n--- Params ---\n',
    'Measurement Variable: ', measure_var, '\n',
    'Multivariate Variable: ', taxa_var, '\n',
    'Event Grouping Variable: ', paste(group_var, collapse = ', '), ' (defines each point)\n',
    'Event Averaging Variable(s): ', 
    if (!is.null(avg_var)) paste(avg_var, collapse = ', ') else 'none (grouping variable is at the sampling event level)', '\n',
    '\n---------------\n'
  )
  
  # Pre-compute grouping columns once
  group_cols <- c(group_var, factor_var, taxa_var)
  meta_vars <- c(group_var, factor_var)
  
  # Compute effort once if needed
  effort_divisor <- NULL
  if (!is.null(avg_var)) {
    effort_divisor <- df %>%
      distinct(across(all_of(c(group_var, avg_var)))) %>%
      count(across(all_of(group_var)), name = 'n_effort')
  }
  
  # aggregate
  df_sum <- df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(total = sum(.data[[measure_var]], na.rm = TRUE), .groups = 'drop')
  
  # Apply effort correction if needed
  if (!is.null(effort_divisor)) {
    df_sum <- df_sum %>%
      left_join(effort_divisor, by = group_var) %>%
      mutate(value = total / n_effort) %>%
      select(-total, -n_effort)
  } else {
    df_sum <- df_sum %>%
      rename(value = total)
  }
  
  # Pivot and prepare matrix (combined operation)
  df_cells <- df_sum %>%
    pivot_wider(
      names_from = all_of(taxa_var), 
      values_from = value,
      values_fill = 0
    )
  
  # Extract metadata and community matrix in one step
  meta <- df_cells %>% select(all_of(meta_vars))
  m_com <- df_cells %>% 
    select(-all_of(meta_vars)) %>%
    as.matrix()
  
  result <- bind_cols(
    meta,
    m_com
  )
  
  if (compute_dist) {
    dist_obj <- vegdist(m_com, method = distance)
    print('here')
  }
  

  return_list <- list(
    m_all = result,
    m_com = m_com,
    m_meta = meta,
    meta_vars = list(
      measure_var = measure_var,
      taxa_var = taxa_var,
      group_var = group_var,
      factor_var = factor_var,
      avg_var = avg_var
    )
  )
  
  
  if (compute_dist) {
    dist_obj <- vegdist(m_com, method = distance)
    result_list$m_dist <- dist_obj
  }
  
  return(return_list)
}

calc_nmds <- function(df, group_var, measure_var, taxa_var = 'Taxon', 
                      factor_var = NULL, avg_var = NULL,
                      distance = 'bray',
                      show_legend = TRUE, color_palette = NULL,
                      try = 20, trymax = 20) {
  set.seed(42)
  
  message(
    '\n--- NMDS Setup ---\n',
    'Measurement Variable: ', measure_var, '\n',
    'Multivariate Variable: ', taxa_var, '\n',
    'Event Grouping Variable: ', group_var, ' (defines each point)\n',
    'Event Averaging Variable(s): ', 
    if (!is.null(avg_var)) paste(avg_var, collapse = ', ') else 'none (grouping variable is at the sampling event level)', '\n',
    'Display/Coloring Variable(s): ', 
    if (!is.null(factor_var)) paste(factor_var, collapse = ', ') else 'none',
    '\nDistance Metric: ', distance,
    '\n------------------\n'
  )
  
  # Pre-compute grouping columns once
  group_cols <- c(group_var, factor_var, taxa_var)
  meta_vars <- c(group_var, factor_var)
  
  # Compute effort once if needed
  effort_divisor <- NULL
  if (!is.null(avg_var)) {
    effort_divisor <- df %>%
      distinct(across(all_of(c(group_var, avg_var)))) %>%
      count(across(all_of(group_var)), name = 'n_effort')
  }
  
  # Single aggregation step
  df_sum <- df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(total = sum(.data[[measure_var]], na.rm = TRUE), .groups = 'drop')
  
  # Apply effort correction if needed
  if (!is.null(effort_divisor)) {
    df_sum <- df_sum %>%
      left_join(effort_divisor, by = group_var) %>%
      mutate(value = total / n_effort) %>%
      select(-total, -n_effort)
  } else {
    df_sum <- df_sum %>%
      rename(value = total)
  }
  
  # Pivot and prepare matrix (combined operation)
  df_cells <- df_sum %>%
    pivot_wider(
      names_from = all_of(taxa_var), 
      values_from = value,
      values_fill = 0
    )
  
  # Extract metadata and community matrix in one step
  meta <- df_cells %>% select(all_of(meta_vars))
  m_com <- df_cells %>% 
    select(-all_of(meta_vars)) %>%
    as.matrix()
  
  # Run NMDS
  nmds <- suppressMessages(
    metaMDS(m_com, distance = distance, try = try, trymax = trymax)
  )
  
  # Combine results
  result <- bind_cols(
    as.data.frame(scores(nmds)$sites),
    meta
  )
  
  return(list(
    df_nmds = result,
    # raw_nmds = nmds,
    m_com = m_com,
    m_meta = meta,
    meta_vars = list(
      measure_var = measure_var,
      taxa_var = taxa_var,
      group_var = group_var,
      factor_var = factor_var,
      avg_var = avg_var
    )
  ))
}

calc_nmds <- function(df, group_var, measure_var, taxa_var = 'Taxon', 
                      factor_var = NULL, avg_var = NULL,
                      distance = 'bray',
                      show_legend = TRUE, color_palette = NULL,
                      try = 20, trymax = 20) {
  set.seed(42)
  
  message(
    '\n--- NMDS Setup ---\n',
    'Measurement Variable: ', measure_var, '\n',
    'Multivariate Variable: ', taxa_var, '\n',
    'Event Grouping Variable: ', group_var, ' (defines each point)\n',
    'Event Averaging Variable(s): ', 
    if (!is.null(avg_var)) paste(avg_var, collapse = ', ') else 'none (grouping variable is at the sampling event level)', '\n',
    'Display/Coloring Variable(s): ', 
    if (!is.null(factor_var)) paste(factor_var, collapse = ', ') else 'none',
    '\nDistance Metric: ', distance,
    '\n------------------\n'
  )
  
  # Pre-compute grouping columns once
  group_cols <- c(group_var, factor_var, taxa_var)
  meta_vars <- c(group_var, factor_var)
  
  # Compute effort once if needed
  effort_divisor <- NULL
  if (!is.null(avg_var)) {
    effort_divisor <- df %>%
      distinct(across(all_of(c(group_var, avg_var)))) %>%
      count(across(all_of(group_var)), name = 'n_effort')
  }
  
  # Single aggregation step
  df_sum <- df %>%
    group_by(across(all_of(group_cols))) %>%
    summarise(total = sum(.data[[measure_var]], na.rm = TRUE), .groups = 'drop')
  
  # Apply effort correction if needed
  if (!is.null(effort_divisor)) {
    df_sum <- df_sum %>%
      left_join(effort_divisor, by = group_var) %>%
      mutate(value = total / n_effort) %>%
      select(-total, -n_effort)
  } else {
    df_sum <- df_sum %>%
      rename(value = total)
  }
  
  # Pivot and prepare matrix (combined operation)
  df_cells <- df_sum %>%
    pivot_wider(
      names_from = all_of(taxa_var), 
      values_from = value,
      values_fill = 0
    )
  
  # Extract metadata and community matrix in one step
  meta <- df_cells %>% select(all_of(meta_vars))
  m_com <- df_cells %>% 
    select(-all_of(meta_vars)) %>%
    as.matrix()
  
  # Run NMDS
  nmds <- suppressMessages(
    metaMDS(m_com, distance = distance, try = try, trymax = trymax)
  )
  
  # Combine results
  result <- bind_cols(
    as.data.frame(scores(nmds)$sites),
    meta
  )
  
  return(list(
    df_nmds = result,
    # raw_nmds = nmds,
    m_com = m_com,
    m_meta = meta,
    meta_vars = list(
      measure_var = measure_var,
      taxa_var = taxa_var,
      group_var = group_var,
      factor_var = factor_var,
      avg_var = avg_var
    )
  ))
}

plot_nmds <- function(df_nmds, meta_vars, fill_var, show_legend = TRUE, color_palette = NULL, title = NULL, alpha = 0.7, flip_order = FALSE) {
  fill_sym <- rlang::sym(fill_var)
  
  if (is.null(title)) {
    title <- paste0(meta_vars$measure_var, 
                   ' (per ', meta_vars$taxa_var, 
                   ') per ', meta_vars$group_var)
  }
  
  if (flip_order) {
    df_nmds <- df_nmds %>%
      arrange(desc(!!fill_sym))
  } else {
    df_nmds <- df_nmds %>%
      arrange(!!fill_sym)
  }
  
  plt <- ggplot(df_nmds, aes(x = NMDS1, y = NMDS2, fill = !!fill_sym)) +
    geom_point(size = 4, shape = 21, alpha = alpha, color = '#000000') +
    theme_bw() +
    labs(title = title)
  
  if (!is.null(color_palette)) {
    plt <- plt + scale_fill_manual(values = color_palette)
  }
  
  if (!show_legend) {
    plt <- plt + theme(legend.position = 'none')
  }
  
  return(plt)
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
      
      # # Add the QC code 'UnitsExceedCells' to the QualityCheck column
      # df <- df %>%
      #   mutate(QualityCheck = case_when(
      #     row_number() %in% rownames(count_issues) & QualityCheck != 'NoCode' ~ paste(QualityCheck, 'UnitsExceedCells'),
      #     row_number() %in% rownames(count_issues) & QualityCheck == 'NoCode' ~ 'UnitsExceedCells',
      #     TRUE ~ QualityCheck
      #   ))
      
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

# Check Station Count -----------------------------------------------------
check_station_count <- function(df) {
  df <- df %>%
    mutate(
      MonthYear = as.Date(floor_date(Date, 'month')),
      Station = as.character(Station)
    )
  df_station_count <- df %>%
    distinct(Station, MonthYear) %>%
    count(MonthYear, name = 'NumStations') %>%
    mutate(
      Year = year(MonthYear),
      Month = month(MonthYear, label = TRUE, abbr = TRUE)
    )
  
  # Create complete grid for all year-month combinations
  all_years <- min(df_station_count$Year):max(df_station_count$Year)
  all_months <- month(1:12, label = TRUE, abbr = TRUE)  # This creates ordered factor like df_station_count$Month
  complete_grid <- expand_grid(Year = all_years, Month = all_months)
  
  df_complete <- complete_grid %>%
    left_join(df_station_count, by = c('Year', 'Month'))
  
  ggplot(df_complete, aes(x = Month, y = Year, fill = NumStations)) +
    geom_tile(color = 'black', linewidth = 0.5) +
    scale_fill_distiller(palette = 'Blues', direction = 1, name = '# Stations', na.value = 'white') +
    scale_y_continuous(breaks = all_years) +
    theme_minimal() +
    labs(
      x = 'Month',
      y = 'Year',
      title = 'Number of Stations Sampled Per Month'
    ) +
    theme(
      axis.text.x = element_text(angle = 45),
      panel.grid = element_blank()
    )
}

# Check Sampling Frequency ------------------------------------------------
check_sampling_freq <- function(df) {
  df <- df %>%
    mutate(
      MonthYear = as.Date(floor_date(Date, 'month')),
      Station = as.character(Station)
    )
  
  df_station_frequency <- df %>%
    mutate(Year = year(MonthYear)) %>%
    distinct(Station, MonthYear, Year) %>%
    count(Station, Year, name = 'TimesPerYear')
  
  # Create complete grid for all station-year combinations
  all_years <- min(df_station_frequency$Year):max(df_station_frequency$Year)
  all_stations <- unique(df$Station)
  complete_grid <- expand_grid(Year = all_years, Station = all_stations)
  
  df_complete <- complete_grid %>%
    left_join(df_station_frequency, by = c('Year', 'Station'))
  
  ggplot(df_complete, aes(x = Year, y = Station, fill = TimesPerYear)) +
    geom_tile(color = 'black', linewidth = 0.2) +
    scale_fill_distiller(palette = 'Blues', direction = 1, name = 'Frequency', na.value = 'white') +
    scale_x_continuous(breaks = all_years) +
    theme_minimal() +
    labs(
      x = 'Year',
      y = 'Station',
      title = 'Station Sampling Frequency Per Year'
    ) +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}

# Check Introduced NAs ----------------------------------------------------

check_non_numeric <- function(df, col_name) {
  if (col_name %in% names(df)) {
    original <- df[[col_name]]
    converted <- as.numeric(as.character(original))
    
    # Find which values became NA
    na_indices <- which(is.na(converted) & !is.na(original))
    
    if (length(na_indices) > 0) {
      cat('Column:', col_name, '\n')
      cat('Problematic values:\n')
      print(unique(original[na_indices]))
      cat('At rows:', na_indices[1:min(10, length(na_indices))], '\n\n')
    }
  }
}


# Plot SIMPER Taxon -------------------------------------------------------

plot_simper_bp <- function(df, sim, taxon_col = 'Taxon', x_axis = 'Year', y_axis, p = 0.05, taxa_num = 6) {
  taxon_sym <- sym(taxon_col)
  x_sym <- sym(x_axis)
  y_sym <- sym(y_axis)
  
  # extract top taxa from SIMPER results
  top_taxa <- sim[[1]] %>%
    as.data.frame() %>%
    rownames_to_column(var = taxon_col) %>%
    arrange(desc(average)) %>%
    filter(p <= !!p) %>%
    slice_head(n = taxa_num) %>%
    pull(!!taxon_sym)
  
  df %>%
    filter((!!taxon_sym) %in% top_taxa) %>%
    ggplot(aes(x = !!x_sym, y = !!y_sym)) +
    geom_boxplot(color = 'black') +
    facet_wrap(vars(!!taxon_sym), scales = 'free_y') +
    labs(
      title = paste('Top SIMPER', y_axis),
      x = x_axis,
      y = y_axis
    ) +
    theme_minimal() +
    theme(strip.text = element_text(size = 8))
}

plot_simper_summary <- function(df, sim,
                                x_axis = 'Year',
                                taxon_col = 'Taxon',
                                y_axis = 'Biovolume_per_mL',
                                agg_fun = sum,
                                p = 0.05,
                                taxa_num = 6) {
  taxon_sym <- sym(taxon_col)
  x_sym <- sym(x_axis)
  y_sym <- sym(y_axis)
  agg_name <- as.character(substitute(agg_fun))[[1]]
  
  top_taxa <- sim[[1]] %>%
    as.data.frame() %>%
    rownames_to_column(var = taxon_col) %>%
    arrange(desc(average)) %>%
    filter(p <= !!p) %>%
    slice_head(n = taxa_num) %>%
    pull(!!taxon_sym)
  
  df %>%
    filter((!!taxon_sym) %in% top_taxa) %>%
    group_by(!!taxon_sym, !!x_sym) %>%
    summarise(
      Value = agg_fun(!!y_sym, na.rm = TRUE),
      .groups = 'drop'
    ) %>%
    ggplot(aes(x = !!x_sym, y = Value, color = !!taxon_sym, group = !!taxon_sym)) +
    geom_line() +
    geom_point(size = 2) +
    labs(
      title = paste('Top SIMPER', y_axis),
      x = x_axis,
      y = paste(stringr::str_to_title(agg_name), 'of', as_label(y_sym)),
      color = 'Taxon'
    ) +
    theme_minimal()
}

# map regions
convert_to_sf <- function(df, sf_delta = NULL) {
  
  if (is.null(sf_delta)) {
    sf_delta <- deltamapr::R_EDSM_Subregions_Mahardja
  }
  
  # convert df to SpatialPointsDataFrame
  coords <- df[, c('Longitude', 'Latitude')]
  data   <- subset(df, select = -c(Latitude, Longitude))
  crs    <- CRS(SRS_string = 'EPSG:4326')
  
  spdf_wq <- SpatialPointsDataFrame(
    coords = coords,
    data   = data,
    proj4string = crs
  )
  
  # convert delta sf to Spatial
  spdf_delta <- as(sf_delta, 'Spatial')
  spdf_delta <- spTransform(spdf_delta, CRS('+init=epsg:4326 +proj=longlat'))
  
  # assign Region/SubRegion
  col_sr <- sp::over(spdf_wq, spdf_delta[, c('Region', 'SubRegion')])
  spdf_wq$Region <- col_sr$Region
  spdf_wq$SubRegion <- col_sr$SubRegion
  
  # convert to sf and tibble
  sf_wq <- st_as_sf(spdf_wq)
  sf_wq <- st_transform(sf_wq, st_crs(sf_delta))
  sf_wq <- sf_wq %>% filter(!is.na(Region))
  
  df_wq <- as_tibble(sf_wq)
  
  # return
  return(sf_wq)
}

# export primer
export_primer <- function(df_meta, df_com,
                          title = 'Environmental variables',
                          meta_as_factors = TRUE) {
  
  # check
  if (nrow(df_meta) != nrow(df_com))
    stop('df_meta and df_com must have same number of rows (samples).')
  
  # sample IDs
  sample_ids <- paste0('R', seq_len(nrow(df_meta)))
  
  # community matrix (variables as rows)
  df_com <- as_tibble(df_com)
  df_mat <- as.data.frame(t(df_com))
  colnames(df_mat) <- sample_ids
  df_mat <- rownames_to_column(df_mat, 'Variable')
  
  # row for sample IDs (R1, R2, …)
  row_sample_ids <- c('Variable', sample_ids)
  
  # optionally include metadata below
  df_factors <- NULL
  if (meta_as_factors) {
    df_factors <- df_meta %>%
      mutate(across(everything(), as.character)) %>%
      t() %>%
      as.data.frame(stringsAsFactors = FALSE)
    colnames(df_factors) <- sample_ids
    df_factors <- rownames_to_column(df_factors, 'Variable')
  }
  
  # prepare title + blank rows
  n_cols <- ncol(df_mat)
  row_title <- rep('', n_cols)
  row_title[1] <- title
  row_blank <- rep('', n_cols)
  
  # assemble in correct PRIMER order:
  # title, sample IDs, community data, blank, metadata
  df_primer <- rbind(
    row_title,
    row_sample_ids,
    df_mat,
    row_blank,
    df_factors
  )
  
  # replace header cell with NA as PRIMER expects
  df_primer[2, 1] <- NA
  
  # create workbook
  wb <- createWorkbook()
  addWorksheet(wb, 'Sheet1')
  
  # write to Excel
  writeData(wb, 'Sheet1', df_primer, colNames = FALSE)
  
  # merge title row
  mergeCells(wb, 'Sheet1', cols = 1:n_cols, rows = 1)
  
  return(wb)
}