# --- Load all group schemas from /schemas folder ---
load_schemas <- function() {
  files   <- list.files('schemas', pattern = '\\.json$', full.names = TRUE)
  schemas <- lapply(files, fromJSON)
  names(schemas) <- sapply(schemas, `[[`, 'survey')
  
  presets <- c('PESP', 'Survey')
  
  for (s in names(schemas)) {
    for (step in names(default_steps)) {
      if (is.null(schemas[[s]]$steps[[step]])) {
        schemas[[s]]$steps[[step]] <- setNames(
          replicate(length(presets), default_steps[[step]], simplify = FALSE),
          presets
        )
      }
    }
  }
  
  schemas
}

# --- Validate a data frame against a schema ---
validate_file <- function(df, schema) {
  errors <- c()
  expected_cols <- names(schema$columns)
  
  # only flag missing columns that are required
  required_cols <- Filter(function(col) {
    isTRUE(schema$columns[[col]]$required)
  }, expected_cols)
  
  missing <- setdiff(required_cols, names(df))
  if (length(missing) > 0)
    errors <- c(errors, paste('Missing columns:', paste(missing, collapse = ', ')))
  
  for (col in intersect(expected_cols, names(df))) {
    expected_type <- schema$columns[[col]]$type
    
    if (expected_type == 'integer' && !is.numeric(df[[col]]))
      errors <- c(errors, paste(col, 'should be numeric'))
    if (expected_type == 'character' && !is.character(df[[col]]))
      errors <- c(errors, paste(col, 'should be character'))
    
    if (!is.null(schema$columns[[col]]$min) && is.numeric(df[[col]])) {
      if (any(df[[col]] < schema$columns[[col]]$min, na.rm = TRUE))
        errors <- c(errors, paste(col, 'has values below min:', schema$columns[[col]]$min))
    }
    if (!is.null(schema$columns[[col]]$max) && is.numeric(df[[col]])) {
      if (any(df[[col]] > schema$columns[[col]]$max, na.rm = TRUE))
        errors <- c(errors, paste(col, 'has values above max:', schema$columns[[col]]$max))
    }
  }
  
  errors
}

# --- Standardize column names using schema mapping ---
standardize_columns <- function(df, schema) {
  if (is.null(schema$mapping)) return(df)
  for (std in names(schema$mapping)) {
    raw <- schema$mapping[[std]]
    if (raw %in% names(df)) names(df)[names(df) == raw] <- std
  }
  df
}

# --- Add not applicable notice ---
not_applicable_alert <- function() {
  div(class = 'alert alert-warning', '⚠️ This function is not applicable for this survey.')
}

# --- Default steps ---
default_steps <- list(
  duplicate_checks        = list(applies = TRUE),
  convert_timezone        = list(applies = TRUE),
  missing_datetime_checks = list(applies = TRUE),
  extreme_time_checks     = list(applies = TRUE),
  station_checks          = list(applies = TRUE),
  na_checks               = list(applies = TRUE),
  add_metadata            = list(applies = TRUE),
  add_comment_cols        = list(applies = TRUE),
  calc_densities          = list(applies = TRUE),
  correct_taxon_typos     = list(applies = TRUE),
  standardize_unknowns    = list(applies = TRUE, std_sp = TRUE, std_suffix = TRUE),
  update_synonyms         = list(applies = TRUE),
  higher_lvl_taxa         = list(applies = TRUE, std_type = 'pesp'),
  combine_taxa            = list(applies = TRUE),
  subset_cols             = list(applies = TRUE)
)