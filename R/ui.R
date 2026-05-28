source('R/ui_helpers.R')
source('R/ui_home.R')
source('R/ui_upload.R')
source('R/ui_inichecks.R')
source('R/ui_format.R')
source('R/ui_finchecks.R')
source('R/ui_export.R')

ui <- page_navbar(
  title = 'Phytoplankton Enumeration Formatting',
  theme = bs_theme(bootswatch = 'flatly'),
  app_css,
  home_tab,
  upload_tab,
  initial_checks_tab,
  format_tab,
  final_checks_tab,
  export_tab
)