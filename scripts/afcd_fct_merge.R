#============================================================
# Merge cleaned FCT information with AFCD

#Name: J. Zachary (Zach) Koehn
#Email: zkoehn@stanford.edu
#Date started: 05/13/2020
#Revised: 05/14/2020
#============================================================

afcd_file <- "AFCD.final.csv"
aus_file <- "clean_fct_aus.csv"
pndb_file <- "clean_fct_pndb.csv"

#__________________________________________
# read data and load libraries directory defaults
# _________________________________________
library(tidyverse);library(dplyr);library(here)


# now use here package to navigate from working directory to specific food file
afcd_dat <- read.csv(
  here("data","OutputsFromR",afcd_file)
  )
aus_dat <- read.csv(
  here("data","OutputsFromR","cleaned_fcts",aus_file)
  )
pndb_dat <- read.csv(
  here("data","OutputsFromR","cleaned_fcts",pndb_file)
  )


dim(afcd_dat)
dim(aus_dat)

c(names(afcd_dat),names(aus_dat))[duplicated(c(names(afcd_dat),names(aus_dat)))==FALSE]

# binds together all variables
afcd_bind <- bind_rows(afcd_dat,aus_dat,pndb_dat)

dim(afcd_bind[afcd_bind$Country.ISO3=="AUS",])