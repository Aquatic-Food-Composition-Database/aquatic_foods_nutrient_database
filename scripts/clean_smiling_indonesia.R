#============================================================
# Cleaning the SMILING EU datasets
# Indonesia

#Name: J. Zachary (Zach) Koehn
#Email: zkoehn@stanford.edu
#Date started: 05/22/2020
#Revised: 09/13/2021
#============================================================

#__________________________________________
# read data and load libraries directory defaults
# _________________________________________
library(tidyverse);library(dplyr);library(here);library(readxl);library(data.table)

# source script of functions
source(here("scripts","functions","func_cleaning_fct.R"))

indonesia_file <- "D3_5a_SMILING_FCT_Indonesia_070813_protected.xlsx"
merge_key_file <- "database_merge_key.xlsx"

indonesia_dat <- read_excel(
  here("data","EU_SMILING_project",indonesia_file), 
  sheet="FCT_INA_05082013_FINAL",col_names = TRUE)

# and read the excel merge key used to modify names and units to merge with AFCD
merge_key <- read_excel(
  here("data",merge_key_file),
  sheet="key"
)



#__________________________________________
# extract aquatic foods information and 
# cleaning variable names
# _________________________________________
# set colnames and clean up the formatting a bit
indonesia_dat <- indonesia_dat[-1,]

# subset only aquatic food values using the document's subgroups
aquatic_food_groups <- c(
  "Fish without bones",
  "Seafood",
  "Small,whole fish,with bones"
)

smiling_indonesia_aquatic_foods_dat <- indonesia_dat %>%
  filter(FOOD_SUBGRP_Optifood %in% aquatic_food_groups) %>%
  select(-FOOD_GRP_Optifood,-FOOD_SUBGRP_Optifood,-'Brand or description',-Recipe)



#__________________________________________
# Convert nutrient units to those used in AFCD (using merge_key file)
# _________________________________________
# now format nutrient columns (to numeric)
smiling_indonesia_aquatic_foods_dat[,5:dim(smiling_indonesia_aquatic_foods_dat)[2]] <- sapply(
  smiling_indonesia_aquatic_foods_dat[,5:dim(smiling_indonesia_aquatic_foods_dat)[2]], 
  as.numeric
)
# run user-written function that converts nutrient measurement units if needed

smiling_indonesia_conversion_coefs <- coefs_convert_unit_fct(key=merge_key,
                                                            original_unit="smiling_indonesia_unit",
                                                            convert_to_unit="AFCD_unit",
                                                            variables_to_convert="smiling_indonesia_variable_name") %>%
  slice(-1:-5) %>%
  select(smiling_indonesia_variable_name,coefs) #just select the variable name and conversion coefficient


# now take all that information and create a named vector, with JUST the conversion coefficients named by the nutrient to be converted
coefs <- as.numeric(smiling_indonesia_conversion_coefs$coefs)
smiling_indonesia_names <- smiling_indonesia_conversion_coefs$smiling_indonesia_variable_name
names(coefs) <- smiling_indonesia_names #names it

# now figure out which variables match from the conversion vector to 
name_match <- intersect(names(smiling_indonesia_aquatic_foods_dat), names(coefs))
smiling_indonesia_aquatic_foods_dat[name_match] <- sweep(smiling_indonesia_aquatic_foods_dat[name_match], 2, unlist(coefs[name_match]), `*`)



#__________________________________________
# Convert nutrient names to those used in AFCD (using merge_key file)
# _________________________________________
# use function in "functions/func_cleaning_fct.r" create dataframe that 
# includes variable names to change from Aus to AFCD
smiling_indonesia_names_to_convert_to_afcd <- convert_nutrient_names("smiling_indonesia_variable_name") 

smiling_indonesia_aquatic_foods_dat_clean <- smiling_indonesia_aquatic_foods_dat %>%
  data.table::setnames(
    old=smiling_indonesia_names_to_convert_to_afcd$original_dataset,
    new=smiling_indonesia_names_to_convert_to_afcd$AFCD_variable_name)
# if this throws an error related to teh old not being int he new... it's usually because the formatting of those 
# variables is wrong. 

smiling_indonesia_aquatic_foods_dat_clean$Country.ISO3 <- "smiling_indonesia" #adds classification for PNDB



#__________________________________________
# save the modified data frames to the folder
# _________________________________________
write.csv(smiling_indonesia_aquatic_foods_dat_clean,here("data","OutputsFromR","cleaned_fcts","clean_fct_smiling_indonesia.csv"),row.names = FALSE)


