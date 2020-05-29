#============================================================
# Cleaning the SMILING EU datasets
# Thailand

#Name: J. Zachary (Zach) Koehn
#Email: zkoehn@stanford.edu
#Date started: 05/28/2020
#Revised: 05/28/2020
#============================================================

#__________________________________________
# read data and load libraries directory defaults
# _________________________________________
library(tidyverse);library(dplyr);library(here);library(readxl);library(data.table)

# source script of functions
source(here("scripts","functions","func_cleaning_fct.R"))


thailand_file <- "D3_5a_SMILING_FCT_Thailand_150513_protected.xlsx"
merge_key_file <- "database_merge_key.xlsx"

thailand_dat <- read_excel(
  here("data","EU_SMILING_project",thailand_file), 
  sheet="user FCT",col_names = FALSE)

# and read the excel merge key used to modify names and units to merge with AFCD
merge_key <- read_excel(
  here("data",merge_key_file),
  sheet="key"
)

# clean up rows
colnames(thailand_dat) <- thailand_dat[2,] #extract and add column names
thailand_dat <- thailand_dat[-c(1:4),] #delete organizational rows
thailand_dat <- thailand_dat[,-c(31:84)] # delete a bunch of empty columns
# subset only aquatic food values using the document's subgroups
aquatic_food_groups <- c(
  "Finfish, shellfish, aquatic animals and products"
)

smiling_thailand_aquatic_foods_dat <- thailand_dat %>%
  filter(Group %in% aquatic_food_groups) 

# now format nutrient columsn (to numeric)
smiling_thailand_aquatic_foods_dat[,6:dim(smiling_thailand_aquatic_foods_dat)[2]] <- sapply(
  smiling_thailand_aquatic_foods_dat[,6:dim(smiling_thailand_aquatic_foods_dat)[2]], 
  as.numeric
)
# run user-written function that converts nutrient measurement units if needed

smiling_thailand_conversion_coefs <- coefs_convert_unit_fct(key=merge_key,
                                                        original_unit="smiling_thailand_unit",
                                                        convert_to_unit="AFCD_unit",
                                                        variables_to_convert="smiling_thailand_variable_name") %>%
  slice(-1:-4) %>%
  select(smiling_thailand_variable_name,coefs) #just select the variable name and conversion coefficient


# now take all that information and create a named vector, with JUST the conversion coefficients named by the nutrient to be converted
coefs <- as.numeric(smiling_thailand_conversion_coefs$coefs)
smiling_thailand_names <- smiling_thailand_conversion_coefs$smiling_thailand_variable_name
names(coefs) <- smiling_thailand_names #names it

# now figure out which variables match from the conversion vector to 
name_match <- intersect(names(smiling_thailand_aquatic_foods_dat), names(coefs))
smiling_thailand_aquatic_foods_dat[name_match] <- sweep(smiling_thailand_aquatic_foods_dat[name_match], 2, unlist(coefs[name_match]), `*`)




# finally, change names so that it can be readily merged with AFCD

# use function in "functions/func_cleaning_fct.r" create dataframe that 
# includes variable names to change from Aus to AFCD
smiling_thailand_names_to_convert_to_afcd <- convert_nutrient_names("smiling_thailand_variable_name") 


smiling_thailand_aquatic_foods_dat_clean <- smiling_thailand_aquatic_foods_dat %>%
  data.table::setnames(
    old=smiling_thailand_names_to_convert_to_afcd$original_dataset,
    new=smiling_thailand_names_to_convert_to_afcd$AFCD_variable_name)
# if this throws an error related to teh old not being int he new... it's usually because the formatting of those 
# variables is wrong. 

smiling_thailand_aquatic_foods_dat_clean$Country.ISO3 <- "smiling_thailand" #adds classification for PNDB
smiling_thailand_aquatic_foods_dat_clean$Edible.portion.coefficient <- 1 #all nutrients included were in 100 gram EDIBLE portions, so assume 1






# save the modified data frames to the folder
write.csv(smiling_thailand_aquatic_foods_dat_clean,here("data","OutputsFromR","cleaned_fcts","clean_fct_smiling_thailand.csv"),row.names = FALSE)

