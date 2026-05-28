library(readxl)
library(tidyr)
library(dplyr)

parse_date_col <- function(x) {
  if (inherits(x, 'POSIXct') || inherits(x, 'Date')) {
    as.Date(x)
  } else if (is.numeric(x)) {
    as.Date(as.numeric(x), origin = '1899-12-30')
  } else {
    as.Date(x, format = '%m/%d/%Y')
  }
}

survey_metadata <- read_excel(
  'data-raw/SurveyMetadata.xlsx',
  sheet     = 'Sheet1',
  skip      = 1,
  col_names = TRUE
) %>%
  fill(Survey, Contact, Timezone, SampleScheme, SampleDepth,
       DepthType, SampleMethod, Lab, CountMethodSample,
       Magnification, .direction = 'down') %>%
  mutate(
    `Starting Date` = parse_date_col(`Starting Date`),
    `Ending Date`   = if_else(
      is.na(`Ending Date`),
      Sys.Date(),
      parse_date_col(`Ending Date`)
    )
  )

save(survey_metadata, file = 'data/survey_metadata.rda')