# ---- Upload tab server logic ----

observe({
  updateSelectInput(session, 'selected_survey', choices = names(schemas))
})

observeEvent(input$selected_survey, {
  req(input$selected_survey)
  updateSelectInput(session, 'selected_preset', choices = c('PESP', 'Survey'))
  reset_all()
}, ignoreInit = TRUE)

observeEvent(input$clear_btn, {
  reset_all()
})

observeEvent(input$file_upload, {
  req(input$selected_survey)
  group    <- input$selected_survey
  schema   <- schemas[[group]]
  manifest <- file_manifest()
  
  existing_names <- unlist(lapply(manifest, function(files) {
    sapply(files, `[[`, 'name')
  }))
  
  for (i in seq_len(nrow(input$file_upload))) {
    path <- input$file_upload$datapath[i]
    name <- input$file_upload$name[i]
    
    if (name %in% existing_names) next
    
    df <- tryCatch(
      if (grepl('\\.xlsx$', name)) read_excel(path) else read.csv(path),
      error = function(e) NULL
    )
    
    errors <- if (is.null(df)) 'Could not read file' else validate_file(df, schema)
    status <- if (length(errors) == 0) 'valid' else 'invalid'
    
    manifest[[group]] <- c(manifest[[group]], list(list(
      name   = name,
      status = status,
      errors = errors,
      data   = df
    )))
    
    existing_names <- c(existing_names, name)
  }
  
  file_manifest(manifest)
})

output$group_cards <- renderUI({
  manifest <- file_manifest()
  if (length(manifest) == 0) return(p('No files uploaded yet.'))
  
  lapply(names(manifest), function(group) {
    files <- manifest[[group]]
    
    file_rows <- lapply(files, function(f) {
      icon <- if (f$status == 'valid') '✅' else '❌'
      tip  <- if (f$status == 'valid') '' else paste(f$errors, collapse = '; ')
      fluidRow(
        column(1, icon),
        column(9, strong(f$name), if (tip != '') div(style = 'color:red; font-size:0.85em', tip)),
        column(2, '')
      )
    })
    
    wellPanel(
      h4(group),
      do.call(tagList, file_rows)
    )
  })
})

output$combine_ui <- renderUI({
  manifest <- file_manifest()
  valid_files <- unlist(lapply(manifest, function(files) {
    Filter(function(f) f$status == 'valid', files)
  }), recursive = FALSE)
  
  if (length(valid_files) == 0) {
    p('No valid files uploaded yet.')
  } else {
    tagList(
      p(paste(length(valid_files), 'valid file(s) ready to process and/or combine.')),
      actionButton('merge_btn', 'Process Files', icon = icon('object-group'),
                   class = 'btn-primary'),
      br(), br(),
      DTOutput('merged_preview')
    )
  }
})

observeEvent(input$merge_btn, {
  manifest <- file_manifest()
  
  all_dfs <- unlist(lapply(names(manifest), function(group) {
    schema <- schemas[[group]]
    files  <- Filter(function(f) f$status == 'valid', manifest[[group]])
    lapply(files, function(f) {
      df <- standardize_columns(f$data, schema)
      format_data(df, schema$survey)
    })
  }), recursive = FALSE)
  
  merged_df(bind_rows(all_dfs))
})

output$merged_preview <- renderDT({
  req(merged_df())
  datatable(merged_df(), options = list(pageLength = 5, scrollX = TRUE))
})