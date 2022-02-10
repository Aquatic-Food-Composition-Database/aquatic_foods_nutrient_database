#============================================================
# Cleaning Norway national FCT

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


norway_file <- "The Norwegian Food Composition Table 2019 (xlsx).xlsx"
merge_key_file <- "database_merge_key.xlsx"

norway_dat <- read_excel(
  here("data","norway",norway_file), 
  sheet="Foods",col_names = FALSE)

# and read the excel merge key used to modify names and units to merge with AFCD
merge_key <- read_excel(
  here("data",merge_key_file),
  sheet="key"
)


#__________________________________________
# extract aquatic foods information and 
# clean data set
# _________________________________________
# set colnames and clean up the formatting a bit
colnames(norway_dat) <- as.character(norway_dat[3,]) 
norway_dat <- norway_dat[-1:-5,]


# ID_Code for fish starts with 04.
norway_aquatic_foods_dat <- norway_dat[grep("^04.", norway_dat$FoodID),] 
norway_aquatic_foods_dat <- norway_aquatic_foods_dat[, !duplicated(colnames(norway_aquatic_foods_dat))] %>%
  select(-Ref)



#__________________________________________
# Convert nutrient units to those used in AFCD (using merge_key file)
# _________________________________________
# now format nutrient columns (to numeric)
norway_aquatic_foods_dat[,3:dim(norway_aquatic_foods_dat)[2]] <- sapply(
  norway_aquatic_foods_dat[,3:dim(norway_aquatic_foods_dat)[2]], 
  as.numeric
)

# run user-written function that converts nutrient measurement units if needed
norway_conversion_coefs <- coefs_convert_unit_fct(key=merge_key,
                                                            original_unit="norway_unit",
                                                            convert_to_unit="AFCD_unit",
                                                            variables_to_convert="norway_variable_name") %>%
  slice(-1:-3) %>%
  select(norway_variable_name,coefs) #just select the variable name and conversion coefficient

# now take all that information and create a named vector, with JUST the conversion coefficients named by the nutrient to be converted
coefs <- as.numeric(norway_conversion_coefs$coefs)
norway_names <- norway_conversion_coefs$norway_variable_name
names(coefs) <- norway_names #names it

# now figure out which variables match from the conversion vector to 
name_match <- intersect(names(norway_aquatic_foods_dat), names(coefs))
norway_aquatic_foods_dat[name_match] <- sweep(norway_aquatic_foods_dat[name_match], 2, unlist(coefs[name_match]), `*`)



#__________________________________________
# Convert nutrient names to those used in AFCD (using merge_key file)
# _________________________________________
# use function in "functions/func_cleaning_fct.r" create dataframe that 
# includes variable names to change from Aus to AFCD
norway_names_to_convert_to_afcd <- convert_nutrient_names("norway_variable_name") 


norway_aquatic_foods_dat_clean <- norway_aquatic_foods_dat %>%
  data.table::setnames(
    old=norway_names_to_convert_to_afcd$original_dataset,
    new=norway_names_to_convert_to_afcd$AFCD_variable_name)
# if this throws an error related to teh old not being int he new... it's usually because the formatting of those 
# variables is wrong. 

norway_aquatic_foods_dat_clean$Country.ISO3 <- "NOR" #adds classification for PNDB


# save the modified data frames to the folder
write.csv(norway_aquatic_foods_dat_clean,here("data","OutputsFromR","cleaned_fcts","clean_fct_norway_2019.csv"),row.names = FALSE)

