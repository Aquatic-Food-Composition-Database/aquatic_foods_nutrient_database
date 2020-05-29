#============================================================
# Cleaning the SMILING EU datasets
# Thailand

#Name: J. Zachary (Zach) Koehn
#Email: zkoehn@stanford.edu
#Date started: 05/22/2020
#Revised: 05/28/2020
#============================================================

#__________________________________________
# read data and load libraries directory defaults
# _________________________________________
library(tidyverse);library(dplyr);library(here);library(readxl);library(data.table)

# source script of functions
source(here("scripts","functions","func_cleaning_fct.R"))


vietnam_file <- "D3_5a_SMILING_FCT_Vietnam_180713_protected.xlsx"
merge_key_file <- "database_merge_key.xlsx"

vietnam_dat <- read_excel(
  here("data","EU_SMILING_project",vietnam_file), 
  sheet="WP3 FCT",col_names = FALSE)

# and read the excel merge key used to modify names and units to merge with AFCD
merge_key <- read_excel(
  here("data",merge_key_file),
  sheet="key"
)

# clean up rows
colnames(vietnam_dat) <- vietnam_dat[2,] #extract and add column names
vietnam_dat <- vietnam_dat[-c(1:2),] #delete organizational rows
# subset only aquatic food values using the document's subgroups
aquatic_food_groups <- c(
  "MyFoods_Special Meats", #gobies in here
  "Fish without bones",
  "Seafood",
  "Other animal parts" #gastropods and toad meat were lumped in here
  
)

smiling_vietnam_aquatic_foods_dat <- vietnam_dat %>%
  filter(FOOD_SUB_GROUP %in% aquatic_food_groups) %>%
  select(-FOOD_GROUP,-FOOD_SUB_GROUP)

# now format nutrient columsn (to numeric)
smiling_vietnam_aquatic_foods_dat[,4:dim(smiling_vietnam_aquatic_foods_dat)[2]] <- sapply(
  smiling_vietnam_aquatic_foods_dat[,4:dim(smiling_vietnam_aquatic_foods_dat)[2]], 
  as.numeric
)
# run user-written function that converts nutrient measurement units if needed

smiling_vietnam_conversion_coefs <- coefs_convert_unit_fct(key=merge_key,
                                                            original_unit="smiling_vietnam_unit",
                                                            convert_to_unit="AFCD_unit",
                                                            variables_to_convert="smiling_vietnam_variable_name") %>%
  select(smiling_vietnam_variable_name,coefs) #just select the variable name and conversion coefficient


# now take all that information and create a named vector, with JUST the conversion coefficients named by the nutrient to be converted
coefs <- as.numeric(smiling_vietnam_conversion_coefs$coefs)
smiling_vietnam_names <- smiling_vietnam_conversion_coefs$smiling_vietnam_variable_name
names(coefs) <- smiling_vietnam_names #names it

# now figure out which variables match from the conversion vector to 
name_match <- intersect(names(smiling_vietnam_aquatic_foods_dat), names(coefs))
smiling_vietnam_aquatic_foods_dat[name_match] <- sweep(smiling_vietnam_aquatic_foods_dat[name_match], 2, unlist(coefs[name_match]), `*`)




# finally, change names so that it can be readily merged with AFCD

# use function in "functions/func_cleaning_fct.r" create dataframe that 
# includes variable names to change from Aus to AFCD
smiling_vietnam_names_to_convert_to_afcd <- convert_nutrient_names("smiling_vietnam_variable_name") 


smiling_vietnam_aquatic_foods_dat_clean <- smiling_vietnam_aquatic_foods_dat %>%
  data.table::setnames(
    old=smiling_vietnam_names_to_convert_to_afcd$original_dataset,
    new=smiling_vietnam_names_to_convert_to_afcd$AFCD_variable_name)
# if this throws an error related to teh old not being int he new... it's usually because the formatting of those 
# variables is wrong. 

smiling_vietnam_aquatic_foods_dat_clean$Country.ISO3 <- "smiling_vietnam" #adds classification for PNDB
smiling_vietnam_aquatic_foods_dat_clean$Edible.portion.coefficient <- 1 #all nutrients included were in 100 gram EDIBLE portions, so assume 1






# save the modified data frames to the folder
write.csv(smiling_vietnam_aquatic_foods_dat_clean,here("data","OutputsFromR","cleaned_fcts","clean_fct_smiling_vietnam.csv"),row.names = FALSE)
