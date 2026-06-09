export_tab <- nav_panel(
  'Export',
  br(),
  
  tags$div(
    class = 'tab-sections',
    
    accordion(
      open = 'export_panel',
      multiple = TRUE,
      
      accordion_panel(
        'Export Data',
        value = 'export_panel',
        icon = icon('download'),
        uiOutput('export_ui')
      )
    )
  )
)