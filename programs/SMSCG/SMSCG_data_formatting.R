#Suisun Marsh Salinity Control Gate Project
#phytoplankton data

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

#script purpose
#download SMSCG phytoplankton data set published on EDI and
#add some columns with metadata for the PESP project
#so this dataset can be combined with other phyto data sets

#To do list
#add GALD to SMSCG phyto dataset

#load packages---------
library(tidyverse)
library(lubridate)

#read in data----------

#phytoplankton abundance data
phyto <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.6&entityid=8c283ea7d1823824ccfa2f05d8056027") %>% 
  glimpse()

#station metadata
stations <-read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.6&entityid=08de2a97cf2a3743af06e3ff6e0e9b39") %>% 
  glimpse()

#format data for PESP---------

#subset station data frame to just the needed columns
stn <- stations %>% 
  select(station
         ,longitude
         ,latitude)

#add station metadata to phyto abundance data frame
phyto_stn <- left_join(phyto,stn)

#did these join correctly?
#check for NAs in treatment column 
phyto_stn_na <- phyto_stn %>% 
  filter(is.na(latitude)) %>% 
  distinct(station)
#three stations
#MONT vs STN_MONT; need to fix that
#the two EZ stations which are floating stations

#final formatting
phyto_format <- phyto_stn %>% 
  #drop samples collected by EMP because these data will be provided by EMP separately
  filter(collected_by!="EMP") %>% 
  #add some columns with metadata about AWCA survey
  add_column(survey = "SMSCG"
             ,collection_type = "grab_surface"
             ,tidal_stage = "variable"
             ,depth = 0
             ,lab = "BSA"
  ) %>% 
  select(survey
         ,station
         ,latitude
         ,longitude
         ,collection_type
         ,date
         ,time_pst
         ,tidal_stage
         ,depth_m = depth
         ,lab
         ,taxon_original = taxon
         #,taxon
         ,kingdom:class
         #,algal_group
         ,genus
         #,species
         ,organisms_per_ml
         ,cells_per_ml
         ,biovolume_cubic_um_per_ml = biovolume_per_ml
         #,gald_um = gald
         #,quality_check
         #,debris
  ) %>% 
  glimpse()

#write formatted data file
#write_csv(phyto_format, "./programs/SMSCG/SMSCG_phyto.csv")


