#============================================================
# Cleaning the Pacific Nutritional Database for merge with 
# note, this data was shared in confidence, do not share data 
# until it is published

#Name: J. Zachary (Zach) Koehn
#Email: zkoehn@stanford.edu
#Date started: 05/11/2020
#Revised: 05/14/2020
#============================================================

#__________________________________________
# read data and load libraries directory defaults
# _________________________________________
library(tidyverse);library(dplyr);library(here);library(readxl);library(data.table)

# source script of functions
source(here("scripts","functions","func_cleaning_fct.R"))

# load raw data files
pndb_details_file <- "PNDB(2020)_v02_Database.xlsx"
merge_key_file <- "database_merge_key.xlsx"


# this has some greek symbols (alpha tocopherol)... so need to set locale to be able to read outside of un
Sys.setlocale("LC_ALL", 'en_US.UTF-8')

# now use here package to navigate from working directory to specific food file
dat_pac <- read_excel(
  here("data_do_not_share",pndb_details_file), 
  sheet="3.Final foods PNDB_2019",col_names = TRUE)

merge_key <- read_excel(
  here("data",merge_key_file),
  sheet="key"
)


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
# remove greek micro symbol (doens't always play nice)
colnames(dat_pac) <- gsub("µ","u",colnames(dat_pac)) 

# subset just aquatic foods we want to add
dat_pac_blue <- dat_pac %>%
  rename("source_nutrient_profile"='Source of nutrient profile',
         "Water (g)" = "WATER") %>%
  filter(HDDS_Group == "Fish and seafood")

dat_pac_blue[,15:68] <- apply(dat_pac_blue[,15:68],2,function(x) as.numeric(x)) #now convert to numeric


# because we are adding the New Zealand and new Australian FCT, remove that data here
dat_pac_blue_no_overlap <- dat_pac_blue %>%
  filter(
    grepl("U1",source_nutrient_profile)
  )
dat_pac_blue_no_overlap$source_nutrient_profile

aus_rows <- which(grepl("A4",dat_pac_blue$source_nutrient_profile)&!(grepl("\\),",dat_pac_blue$source_nutrient_profile)))
nz_rows <- which(grepl("N1",dat_pac_blue$source_nutrient_profile)&!(grepl("\\),",dat_pac_blue$source_nutrient_profile)))
usa_rows <- which(grepl("U1",dat_pac_blue$source_nutrient_profile)&!(grepl("\\),",dat_pac_blue$source_nutrient_profile)))
other_databases_to_remove <- c(aus_rows,nz_rows,usa_rows)

dat_pac_blue_no_overlap <- dat_pac_blue[-other_databases_to_remove,]

# run user-written function that converts nutrient measurement units if needed
# this is all coming from the merge_key file loaded at the top
PNDB_conversion_coefs <- coefs_convert_unit_fct(key=merge_key,
                                               original_unit="PNDB_unit",
                                               convert_to_unit="AFCD_unit",
                                               variables_to_convert="PNDB_variable_name") %>%
  select(PNDB_variable_name,coefs) %>% #just select the variable name and conversion coefficient
  slice(-1:-5)  # and remove a few rows wher we have non-numeric values (matters for merge... but not for unit conversions)


# now take all that information and create a named vector, with JUST the conversion coefficients named by the nutrient to be converted
coefs <- as.numeric(PNDB_conversion_coefs$coefs)
PNDB_names <- PNDB_conversion_coefs$PNDB_variable_name
names(coefs) <- PNDB_names #names it

# now figure out which variables match from the conversion vector to 
name_match <- intersect(names(dat_pac_blue_no_overlap), names(coefs))
dat_pac_blue_no_overlap[name_match] <- sweep(dat_pac_blue_no_overlap[name_match], 2, unlist(coefs[name_match]), `*`)



# finally, change names so that it can be readily merged with AFCD

# use function in "functions/func_cleaning_fct.r" create dataframe that 
# includes variable names to change from Aus to AFCD
PNDB_names_to_convert_to_afcd <- convert_nutrient_names("PNDB_variable_name") 


dat_pac_blue_clean <- dat_pac_blue_no_overlap %>%
  rename('Source of nutrient profile'='source_nutrient_profile') %>% #change this one back (needed to remove spaces)
  data.table::setnames(
    old=PNDB_names_to_convert_to_afcd$original_dataset,
    new=PNDB_names_to_convert_to_afcd$AFCD_variable_name)

dat_pac_blue_clean$Country.ISO3 <- "PNDB" #adds classification for PNDB


write.csv(dat_pac_blue_clean,file.path(directory,"data","OutputsFromR","cleaned_fcts","clean_fct_pndb.csv"),row.names = FALSE)

