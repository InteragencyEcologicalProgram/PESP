#Delta Smelt Resiliency Strategy
#Aquatic Weed Control Action
#phytoplankton data

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

#script purpose
#download data set published on EDI and
#add some columns with metadata for this project
#so this dataset can be combined with other phyto data sets

#to do list
#probably need to convert time from PDT to PST

#load packages---------
library(tidyverse)
library(lubridate)

#read in data----------

#phytoplankton abundance data
phyto <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1079.2&entityid=03b1ffd483203ff8d46e6572307a1a63") %>% 
  glimpse()

#station metadata
stations <-read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1079.2&entityid=ce6f9ee9c02c9a3cdf23a5e1fb5983bf") %>% 
  glimpse()

#format data for PESP---------

#subset station data frame to just needed columns
stn <- stations %>% 
  select(station,treatment,latitude,longitude) %>% 
  glimpse()

#add station info to abundance dataframe
phyto_stn <- left_join(phyto,stn)

#did these join correctly?
#check for NAs in treatment column 
phyto_stn_na <- phyto_stn %>% 
  filter(is.na(treatment))
#no NAs; probabably worked fine then

#final formatting
phyto_format <- phyto_stn %>% 
  #drop some columns not needed by PESP
  select(-c(survey_year,survey_month)) %>% 
  #change date time to PST
  
  #separate date and time columns
  #mutate(date = ymd(date_time_pdt) 
         #,time = hms(date_time_pdt)
         #) %>% 
  #add some columns with metadata about AWCA survey
  add_column(survey = "AWCA"
             #,collection_type = #what is this?
             ,depth = 0
             ,lab = "BSA"
             ) %>% 
  select(survey
         ,station
         ,latitude
         ,longitude
         #,collection_type
         ,date_time_pdt
         #,date
         #,time
         ,depth
         ,lab
         ,taxon_original
         ,taxon = taxon_current
         ,kingdom:algal_group
         ,genus
         ,species
         ,organisms_per_ml
         ,cells_per_ml
         ,biovolume_cubic_micron_per_ml
         ,gald:debris
         ) %>% 
  glimpse()










