#============================================================
# Cleaning the SMILING EU datasets
# Cambodia

#Name: J. Zachary (Zach) Koehn
#Email: zkoehn@stanford.edu
#Date started: 05/22/2020
#Revised: 05/22/2020
#============================================================

#__________________________________________
# read data and load libraries directory defaults
# _________________________________________
library(tidyverse);library(dplyr);library(here);library(readxl);library(data.table)

# source script of functions
source(here("scripts","functions","func_cleaning_fct.R"))


cambodia_file <- "D3_5a_SMILING_FCT__Cambodia_010813_protected.xlsx"
merge_key_file <- "database_merge_key.xlsx"

cambodia_dat <- read_excel(
  here("data","EU_SMILING_project",cambodia_file), 
  sheet="User FCT",col_names = FALSE)

# and read the excel merge key used to modify names and units to merge with AFCD
merge_key <- read_excel(
  here("data",merge_key_file),
  sheet="key"
)

# set colnames and clean up the formatting a bit
colnames(cambodia_dat) <- as.character(cambodia_dat[2,]) 
cambodia_dat <- cambodia_dat[-1:-2,]


# ID_Code for fish starts with J

smiling_cambodia_aquatic_foods_dat <- cambodia_dat[grep("^J", cambodia_dat$'ID Code'),]

# now format nutrient columsn (to numeric)
smiling_cambodia_aquatic_foods_dat[,14:dim(smiling_cambodia_aquatic_foods_dat)[2]] <- sapply(
  smiling_cambodia_aquatic_foods_dat[,14:dim(smiling_cambodia_aquatic_foods_dat)[2]], 
  as.numeric
)
# run user-written function that converts nutrient measurement units if needed
	
smiling_cambodia_conversion_coefs <- coefs_convert_unit_fct(key=merge_key,
                                                  original_unit="smiling_cambodia_unit",
                                                  convert_to_unit="AFCD_unit",
                                                  variables_to_convert="smiling_cambodia_variable_name") %>%
  slice(-1:-5) %>%
  filter(smiling_cambodia_variable_name != "XN") %>%
  select(smiling_cambodia_variable_name,coefs) #just select the variable name and conversion coefficient


# now take all that information and create a named vector, with JUST the conversion coefficients named by the nutrient to be converted
coefs <- as.numeric(smiling_cambodia_conversion_coefs$coefs)
smiling_cambodia_names <- smiling_cambodia_conversion_coefs$smiling_cambodia_variable_name
names(coefs) <- smiling_cambodia_names #names it

# now figure out which variables match from the conversion vector to 
name_match <- intersect(names(smiling_cambodia_aquatic_foods_dat), names(coefs))
smiling_cambodia_aquatic_foods_dat[name_match] <- sweep(smiling_cambodia_aquatic_foods_dat[name_match], 2, unlist(coefs[name_match]), `*`)




# finally, change names so that it can be readily merged with AFCD

# use function in "functions/func_cleaning_fct.r" create dataframe that 
# includes variable names to change from Aus to AFCD
smiling_cambodia_names_to_convert_to_afcd <- convert_nutrient_names("smiling_cambodia_variable_name") 


smiling_cambodia_aquatic_foods_dat_clean <- smiling_cambodia_aquatic_foods_dat %>%
  data.table::setnames(
    old=smiling_cambodia_names_to_convert_to_afcd$original_dataset,
    new=smiling_cambodia_names_to_convert_to_afcd$AFCD_variable_name)
# if this throws an error related to teh old not being int he new... it's usually because the formatting of those 
# variables is wrong. 

smiling_cambodia_aquatic_foods_dat_clean$Country.ISO3 <- "smiling_cambodia" #adds classification for PNDB




# change names of all the values to align with names in AFCD





# save the modified data frames to the folder
write.csv(smiling_cambodia_aquatic_foods_dat_clean,here("data","OutputsFromR","cleaned_fcts","clean_fct_smiling_cambodia.csv"),row.names = FALSE)


