#Delta Smelt Resiliency Strategy
#Aquatic Weed Control Action
#phytoplankton data

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

#script purpose
#download AWCA phytoplankton data set published on EDI and
#add some columns with metadata for the PESP project
#so this dataset can be combined with other phyto data sets

#notes
#consider dropping samples from sites treated with herbicides

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
  mutate(
    #specify time zone as PDT for date-time
    date_time_pdt = force_tz(date_time_pdt, tzone = "America/Los_Angeles")
    #change date-time from PDT to PST
    ,date_time_pst = with_tz(date_time_pdt,tzone="Etc/GMT+8")
    #separate date and time columns
    ,date = date(date_time_pst) 
    ,time = format(as.POSIXct(date_time_pst),format = "%H:%M:%S")
    #create taxon column with standardize name
    ,taxon = case_when(taxon_current!="None"~taxon_current,TRUE~taxon_original)
    ) %>% 
  relocate(date_time_pst:time,.after = date_time_pdt) %>% 
  relocate(taxon,.after = taxon_current) %>% 
  #add some columns with metadata about AWCA survey
  add_column(survey = "AWCA"
             ,collection_type = "grab_surface"
             ,tidal_stage = "high_slack"
             ,depth = 0
             ,lab = "BSA"
             ) %>% 
  select(survey
         ,station
         ,latitude
         ,longitude
         ,collection_type
         ,date
         ,time_pst = time
         ,tidal_stage
         ,depth_m = depth
         ,lab
         ,taxon_original
         ,taxon
         ,kingdom:algal_group
         ,genus
         ,species
         ,units_per_ml = organisms_per_ml
         ,cells_per_ml
         ,biovolume_cubic_um_per_ml = biovolume_cubic_micron_per_ml
         ,gald_um = gald
         ,quality_check
         ,debris
         ) %>% 
  glimpse()

#look at QA codes
#unique(phyto_format$quality_check)
#"good"       "fragmented" "degraded" 

#write formatted data file
#write_csv(phyto_format, "./programs/AWCA/AWCA_phyto.csv")








