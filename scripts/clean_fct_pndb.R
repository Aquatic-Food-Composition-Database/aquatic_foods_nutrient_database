#============================================================
# Cleaning the Pacific Nutritional Database for merge with 
# note, this data was shared in confidence, do not share until
# it is published

#Name: J. Zachary (Zach) Koehn
#Email: zkoehn@stanford.edu
#Date started: 05/11/2020
#Revised: 05/11/2020
#============================================================

#__________________________________________
# read data and load libraries directory defaults
# _________________________________________
library(tidyverse);library(dplyr);library(here);library(readxl)

here()
# source script of functions
source(here("scripts","functions","func_cleaning_fct.R"))

# load raw data files
pndb_details_file <- "PNDB(2020)_v02_Database.xlsx"

# this has some greek symbols (alpha tocopherol)... so need to set locale to be able to read outside of un
Sys.setlocale("LC_ALL", 'en_US.UTF-8')

# now use here package to navigate from working directory to specific food file
dat_pac <- read_excel(
  here("data_do_not_share",pndb_details_file), 
  sheet="3.Final foods PNDB_2019",col_names = TRUE)

#__________________________________________
# clean column names (some repeats due to duplicated variable names)
# _________________________________________

# A few of the variable names were duplicated, but the difference is explained in the 
# second row of the spreadsheet... usually due to some standardization. 
# R flagged them with ...{row number}
# so, we need to change the variable names

# the energy values, the second set of values were standardized measures, ones we will use
colnames(dat_pac)[22:25] <- c("ENERC_kcal","ENERC_kJ","ENERC_kcal_standardized","ENERC_kJ_standardized")

# carbohydrate values, first version is standardized, second method was unavailable
colnames(dat_pac)[32:33] <- c("CHOAVL or CHOAVLDF (g)_standardized","CHOAVL or CHOAVLDF (g)_unknown")

# and vitamin A, retinol, RAE
colnames(dat_pac)[52:55] <- c("VITA_RAE (µg)", "VITA (µg)","VITA_RAE (µg)_standardized", "VITA (µg)_standardized")

# and vitamin e alpha tocopherols
colnames(dat_pac)[64:65] <- c("VITE or TOCPHA (µg)_standardized","VITE or TOCPHA (µg)")


# now clean up dat_paca frame
dat_pac <- dat_pac[-c(1),-c(69:71)] # last 3 columns that don't have any variables or values... as well as the subheader row (row 1)

dat_pac_blue <- dat_pac %>%
  filter(HDDS_Group == "Fish and seafood")

dat_pac_blue[,15:68] <- apply(dat_pac_blue[,15:68],2,function(x) as.numeric(x))


