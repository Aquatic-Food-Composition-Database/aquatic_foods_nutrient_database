# =========================================
# Clean Fiorella & Thilsted database
# exports to CSV 
#
#
# Name: J. Zachary (Zach) Koehn
# Email: zkoehn@gmail.com
# For: Blue Food Assessment, Stanford University
# Date started: 04/24/2020
# Revised: 04/27/2020
# =========================================


#__________________________________________
# read data and load libraries directory defaults
# _________________________________________
library(plyr);library(dplyr);library(here);library(readxl)
Sys.setlocale('LC_ALL','C') #sets language to eliminate multibyte error if it arrises

# load raw data file
file <- "Data_Abstraction_Sept_2019_Additions.xlsx"

raw_dat <- read_excel(
  here("data_2",file),
  sheet="Nutrient Composition")

# __________________________________________
# extract nutrient data for mass transformation

# this data maintains the unit of measure that was included in the original study
# so we need to convert to FAO's standard units that are used in the master file
# __________________________________________

# first extract only nutrient estimates and the nutrient unit of measure

nutrient_dat <- raw_dat %>%
  select("B1":"TAA unit measure") #selects from first nutrient to last
# and extract all the metadata that will be added bound back onto the converted nutrient info later
obs_dat <- raw_dat %>% 
  select("Citation (Abbreviation)":"Type")

# first figure out how many different kinds of measurement units there are
# all units of measure are even numbers, so...

nutrient_dat %>%
  select(seq(2,dim(nutrient_dat)[2],by=2)) %>%
  unlist(use.names = FALSE) %>%
  unique()
  
names(nutrient_dat)

mass_transform <- function(dat,row_num,col_num) {
  if( dat[row_num,col_num]=="mg/gram" )
  
}




