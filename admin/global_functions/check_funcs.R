
# Check Variables ---------------------------------------------------------

# # check station names

check_stations <- function(col_data, col_check){
  
  ls_ex_stations <- unique(col_data)[!(unique(col_data) %in% c(col_check))]
  
  df_ex_stations <- data.frame('extra stations' = ls_ex_stations)
  
  if (length(ls_ex_stations) > 0){
    warning(glue('Station(s) found that are not in the official list: {toString(ls_ex_stations)}.\nEither update official list or fix station name(s).'))
  } else {
    message('All station names in given station list.')
  }
  
  return(df_ex_stations)
}

# # check methods

check_methods <- function(df){
  ls_methods <- unique(df$SamplingMethod)
  
  message(message(glue::glue('Currect collection type methods: {toString(ls_methods)}')))
}

# # check distinct rows
check_distinct <- function(df, return_df = FALSE, coerce = TRUE,
                           type = c('full', 'key_cols'),
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
    
  } else if (type == 'key_cols') {
    if (is.null(key_cols) || is.null(measurement_cols)) {
      stop('For type = "key_cols", both key_cols and measurement_cols must be provided.')
    }
    
    df_grouped <- df_check %>%
      group_by(across(all_of(c(key_cols, taxa_cols, measurement_cols)))) %>%
      summarise(n = n(), .groups = 'drop') %>%
      filter(n > 1)
    
    nondistinct_keyrows <- df_grouped %>%
      filter(if_any(all_of(measurement_cols), ~ .x > 1))
    
    n_inconsistent <- nrow(nondistinct_keyrows)
    
    if (n_inconsistent > 0) {
      message('Number of inconsistent key combinations: ', n_inconsistent)
      attr(df, 'log') <- list(nondistinct_keyrows = nondistinct_keyrows)
      if (isTRUE(return_df)) {
        return(nondistinct_keyrows)
      } else {
        return(df)
      }
    } else {
      message('All key+taxa+measurement column combinations are unique.')
      attr(df, 'log') <- list(nondistinct_keyrows = NULL)
      return(invisible(df))
    }
  }
}

extract_unstandardized_comments <- function(df, comment_col, delimiter = ' ') {
  comment_col <- rlang::ensym(comment_col)
  
  # normalize common delimiters to regex equivalents
  if (!is.null(delimiter) && delimiter != "") {
    delimiter <- dplyr::case_when(
      delimiter == ". "  ~ "\\.\\s*",
      delimiter == "."   ~ "\\.",
      delimiter == ", "  ~ ",\\s*",
      delimiter == ","   ~ ",",
      delimiter == "; "  ~ ";\\s*",
      delimiter == ";"   ~ ";",
      delimiter == " - " ~ "\\s+-\\s*",
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
    "(", 
    paste0(stringr::str_replace_all(known_phrases, "(\\W)", "\\\\\\1"), collapse = "|"), 
    ")"
  )
  
  raw_comments <- df %>%
    dplyr::pull(!!comment_col) %>%
    unique() %>%
    discard(is.na)
  
  cleaned_comments <- stringr::str_remove_all(raw_comments, regex(known_pattern, ignore_case = TRUE))
  
  # isolate unmatched fragments
  if (!is.null(delimiter) && delimiter != "") {
    unmatched <- cleaned_comments %>%
      map(~ stringr::str_split(.x, delimiter)[[1]]) %>%
      unlist() %>%
      stringr::str_trim() %>%
      stringr::str_remove('\\.$') %>%
      discard(~ .x == "" || is.na(.x))
  } else {
    unmatched <- cleaned_comments %>%
      stringr::str_trim() %>%
      discard(~ .x == "" || is.na(.x))
  }
  
  unmatched_df <- unique(unmatched) %>%
    tibble::tibble(Unmatched = .)
  
  if (nrow(unmatched_df) > 0) {
    message("Unique unstandardized comments: ", nrow(unmatched_df))
    attr(df, "log")$unmatched_comments <- unmatched_df
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
create_nmds <- function(df, group_var, nmds_var, factor_var = NULL, show_legend = TRUE, color_palette = NULL) {
  set.seed(42)

  if (!is.null(factor_var)) {
    group_var <- rlang::sym(group_var)
    nmds_var <- rlang::sym(nmds_var)
    factor_var <- rlang::sym(factor_var)
    
    df_cells <- df %>%
      select(!!group_var, !!factor_var, Taxon, !!nmds_var) %>%
      group_by(!!group_var, !!factor_var, Taxon) %>%
      reframe(!!nmds_var := mean(!!nmds_var, na.rm = TRUE)) %>%
      pivot_wider(names_from = Taxon, values_from = !!nmds_var)
    
    com <- df_cells %>%
      select(-!!group_var, -!!factor_var)
    com[is.na(com)] <- 0
    m_com <- as.matrix(com)
    
  } else {
    factor_var <- rlang::sym(group_var)
    nmds_var <- rlang::sym(nmds_var)
    
    df_cells <- df %>%
      select(!!factor_var, Taxon, !!nmds_var) %>%
      group_by(!!factor_var, Taxon) %>%
      reframe(!!nmds_var := mean(!!nmds_var, na.rm = TRUE)) %>%
      pivot_wider(names_from = Taxon, values_from = !!nmds_var)
    
    com <- df_cells %>%
      select(-!!factor_var)
    com[is.na(com)] <- 0
    m_com <- as.matrix(com)
  }
  
  nmds <- metaMDS(m_com, distance = 'bray')
  df_nmds <- as.data.frame(scores(nmds)$sites)
  
  df_nmds <- df_nmds %>%
    mutate(!!factor_var := df_cells %>% pull(!!factor_var)) %>%
    mutate(!!factor_var := as.factor(!!factor_var))
  
  plt_nmds <- ggplot(df_nmds, aes(x = NMDS1, y = NMDS2, group = !!factor_var)) + 
    geom_point(size = 4, shape = 21, color = '#000000', aes(fill = !!factor_var)) +
    theme_bw()
  
  if (!is.null(color_palette)) {
    plt_nmds <- plt_nmds + scale_fill_manual(values = color_palette)
  }
  
  if (!show_legend) {
    plt_nmds <- plt_nmds + theme(legend.position = 'none')
  }
    
  return(list(
    nmds_df = df_nmds,
    plot = plt_nmds
  ))
}








