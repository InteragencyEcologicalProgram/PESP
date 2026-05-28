home_tab <- nav_panel(
  'Home',
  br(),
  h4('Overview'),
  p('This app formats phytoplankton enumeration data for review and export.'),
  p('Use the tabs from left to right: upload files, run initial checks, format the data, run final checks, and export the result.'),
  tags$hr(),
  h4('Workflow'),
  tags$ol(
    tags$li('Upload one or more source files.'),
    tags$li('Process uploaded files and combine if needed.'),
    tags$li('Perform initial data checks.'),
    tags$li('Apply formatting and data transformations.'),
    tags$li('Review final checks and export the completed dataset.')
  ),
  tags$hr(),
  h4('Metadata'),
  p('More metadata info')
)