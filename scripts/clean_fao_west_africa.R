#============================================================
# Cleaning FAO's West Africa food composition table dataset
# for merge with AFCT

#Name: J. Zachary (Zach) Koehn
#Email: zkoehn@stanford.edu
#Date started: 05/05/2020
#Revised: 05/11/2020
#============================================================



#__________________________________________
# read data and load libraries directory defaults
# _________________________________________
library(dplyr);library(here);library(readxl)
Sys.setlocale('LC_ALL','C') #sets language to eliminate multibyte error if it arrises

# source script of functions
source(here("functions","func_cleaning_fct.R"))

# load raw data files
fao_west_africa_file <- "WAFCT_2019.xlsx"

food_details_dat <- read_excel(
  here("data","australia_database",fao_west_africa_file), 
  sheet="Release 1 - Food File")
