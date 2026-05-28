# --- Load all group schemas from /schemas folder ---
load_schemas <- function() {
  files <- list.files('schemas', pattern = '\\.json$', full.names = TRUE)
  schemas <- lapply(files, fromJSON)
  names(schemas) <- sapply(schemas, `[[`, 'survey')
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