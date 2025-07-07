#Suisun Marsh Salinity Control Gate Project
#phytoplankton data

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

#script purpose
#download SMSCG phytoplankton data set published on EDI and
#add some columns with metadata for the PESP project
#so this dataset can be combined with other phyto data sets

#load packages---------
library(tidyverse)
library(lubridate)

#read in data----------

#EDI links aren't working 5/23/25
#phytoplankton abundance data
# phyto <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=8c283ea7d1823824ccfa2f05d8056027") %>% 
#   glimpse()

#station metadata
# stations <-read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=08de2a97cf2a3743af06e3ff6e0e9b39") %>% 
#   glimpse()

#phytoplankton taxonomy
# taxonomy <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.876.7&entityid=4c514654c71a7b8b930bdd031701c359") %>% 
#   glimpse()

#read in data from files downloaded to PESP repo from latest EDI version of phyto files

#phytoplankton abundance data
phyto <-read_csv("./programs/DWR-SMSCG/data_input/smscg_phytoplankton_samples_2020-2023.csv")

#station metadata
stations <- read_csv("./programs/DWR-SMSCG/data_input/smscg_stations_phyto.csv")

#SMSCG phytoplankton taxonomy
#NOTE: need to use the one Tiffany made, but I might still need some data from this if I have taxa on my list
#that she is missing from hers
taxonomy_smscg <- read_csv("./programs/DWR-SMSCG/data_input/smscg_phytoplankton_taxonomy.csv")


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
#looks like it 

#add taxonomy info 
#phyto_stn_tax <- left_join(phyto_stn,taxonomy) 
#matching doesn't work that well; should have retained taxon and taxon_original in abundance data set
#don't stress about it for now because Tiffany is reworking the master taxonomy anyway

#final formatting
phyto_format <- phyto_stn %>% 
  #drop samples collected by EMP because these data will be provided by EMP separately
  filter(collected_by!="EMP") %>% 
  #add some columns with metadata; I think these will be added later via a separate metadata table
  # add_column(Survey = "DWR-SMSCG"
  #            ,collection_type = "grab_surface"
  #            #,tidal_stage = "variable" #not in final PESP table
  #            ,depth = 0
  #            ,lab = "BSA"
  # ) %>% 
  select(Date = date
         ,Time = time_pst #but I think PESP wants in PDT for some reason,station
         ,Station = station #compare my formatting of names to PESP
         ,Latitude = latitude
         ,Longitude = longitude
         ,OrigTaxon = taxon_original 
         ,Taxon = taxon #but need to check this against PESP taxonomy
         #,kingdom:class
         #,algal_group
         #,genus
         #,species
         ,Cells_per_mL = cells_per_ml
         ,Units_per_mL = units_per_ml 
         ,Biovolume_per_mL = biovolume_per_ml
         ,GALD = gald_um 
         ,PhytoForm = phyto_form
         ,QualityCheck = quality_check
         ,Debris = debris
  ) %>% 
  glimpse()
#need to add taxonomy info to this at some point

#unique(phyto_format$Debris)
#NA         "high"     "moderate" "low"  
#PESP uses high, moderate, low, none, unknown

#unique(phyto_format$QualityCheck)
#"Good"  "TallyNotMet"      "PoorlyPreserved"        "BrokenDiatoms"  "TallyNotMet BrokenDiatoms"
#PESP uses 

#write formatted data file
#write_csv(phyto_format, "./programs/SMSCG/SMSCG_phyto.csv")


