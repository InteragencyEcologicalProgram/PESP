upload_tab <- nav_panel(
  'Upload',
  br(),
  
  tags$div(
    class = 'tab-sections',
    
    accordion(
      open = 'upload_panel',
      multiple = TRUE,
      
      accordion_panel(
        'Upload Data',
        value = 'upload_panel',
        icon = icon('upload'),
        layout_sidebar(
          sidebar = sidebar(
            width = 350,
            selectInput('selected_survey', 'Select Survey',
                        choices = c('Select a survey...' = ''),
                        selected = ''),
            selectInput('selected_preset', 'Select Preset', choices = NULL),
            fileInput('file_upload', 'Upload Files', multiple = TRUE,
                      accept = c('.xlsx', '.csv')),
            actionButton('clear_btn', 'Clear All', icon = icon('trash'),
                         class = 'btn-danger')
          ),
          uiOutput('group_cards')
        )
      ),
      
      accordion_panel(
        'Process Data',
        value = 'combine_panel',
        icon = icon('table'),
        uiOutput('combine_ui')
      )
    )
  )
)