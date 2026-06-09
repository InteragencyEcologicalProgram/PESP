server <- function(input, output, session) {
  load('data/survey_metadata.rda')
  schemas <- load_schemas()
  
  # ---- Reactive state ----
  file_manifest           <- reactiveVal(list())
  merged_df               <- reactiveVal(NULL)
  full_dupes              <- reactiveVal(NULL)
  key_dupes               <- reactiveVal(NULL)
  na_data                 <- reactiveVal(NULL)
  datetime_problems       <- reactiveVal(NULL)
  extreme_problems        <- reactiveVal(NULL)
  unstandardized_comments <- reactiveVal(NULL)
  
  current_groups <- reactive({ names(file_manifest()) })
  
  reset_all <- function() {
    file_manifest(list())
    merged_df(NULL)
    full_dupes(NULL)
    key_dupes(NULL)
    na_data(NULL)
    datetime_problems(NULL)
    extreme_problems(NULL)
    unstandardized_comments(NULL)
    updateSelectInput(session, 'selected_preset', choices = c('PESP', 'Survey'))
  }
  
  source('R/server_upload.R',  local = TRUE)
  source('R/server_inichecks.R',  local = TRUE)
  source('R/server_format.R',  local = TRUE)
  source('R/server_finchecks.R',  local = TRUE)
  source('R/server_export.R',  local = TRUE)
}