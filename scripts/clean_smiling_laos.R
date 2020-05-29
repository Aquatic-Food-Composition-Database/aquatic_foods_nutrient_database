#============================================================
# Cleaning the SMILING EU datasets
# Laos

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


laos_file <- "D3_5a_SMILING_FCT_Laos_010813_protected.xlsx"
merge_key_file <- "database_merge_key.xlsx"

laos_dat <- read_excel(
  here("data","EU_SMILING_project",laos_file), 
  sheet="FCTB",col_names = TRUE)

# and read the excel merge key used to modify names and units to merge with AFCD
merge_key <- read_excel(
  here("data",merge_key_file),
  sheet="key"
)


# subset only aquatic food values using the document's subgroups
aquatic_food_groups <- c(
  "Fish without bones",
  "Seafood",
  "Small,whole fish,with bones"
)

smiling_laos_aquatic_foods_dat <- laos_dat %>%
  filter(FOOD_SUB_GROUP %in% aquatic_food_groups) %>%
  select(-FOOD_GROUP,-FOOD_SUB_GROUP,-COUNTRY,-'SECONDARY SOURCE AND FOOD CODE'	,-'RETENTION FACTOR, if applicable'	,-'Match 1=exact 2=similar')

# now format nutrient columsn (to numeric)
smiling_laos_aquatic_foods_dat[,5:dim(smiling_laos_aquatic_foods_dat)[2]] <- sapply(
  smiling_laos_aquatic_foods_dat[,5:dim(smiling_laos_aquatic_foods_dat)[2]], 
  as.numeric
)
# run user-written function that converts nutrient measurement units if needed

smiling_laos_conversion_coefs <- coefs_convert_unit_fct(key=merge_key,
                                                             original_unit="smiling_laos_unit",
                                                             convert_to_unit="AFCD_unit",
                                                             variables_to_convert="smiling_laos_variable_name") %>%
  slice(-1:-4) %>%
  select(smiling_laos_variable_name,coefs) #just select the variable name and conversion coefficient


# now take all that information and create a named vector, with JUST the conversion coefficients named by the nutrient to be converted
coefs <- as.numeric(smiling_laos_conversion_coefs$coefs)
smiling_laos_names <- smiling_laos_conversion_coefs$smiling_laos_variable_name
names(coefs) <- smiling_laos_names #names it

# now figure out which variables match from the conversion vector to 
name_match <- intersect(names(smiling_laos_aquatic_foods_dat), names(coefs))
smiling_laos_aquatic_foods_dat[name_match] <- sweep(smiling_laos_aquatic_foods_dat[name_match], 2, unlist(coefs[name_match]), `*`)




# finally, change names so that it can be readily merged with AFCD

# use function in "functions/func_cleaning_fct.r" create dataframe that 
# includes variable names to change from Aus to AFCD
smiling_laos_names_to_convert_to_afcd <- convert_nutrient_names("smiling_laos_variable_name") 


smiling_laos_aquatic_foods_dat_clean <- smiling_laos_aquatic_foods_dat %>%
  data.table::setnames(
    old=smiling_laos_names_to_convert_to_afcd$original_dataset,
    new=smiling_laos_names_to_convert_to_afcd$AFCD_variable_name)
# if this throws an error related to teh old not being int he new... it's usually because the formatting of those 
# variables is wrong. 

smiling_laos_aquatic_foods_dat_clean$Country.ISO3 <- "smiling_laos" #adds classification for PNDB







# save the modified data frames to the folder
write.csv(smiling_laos_aquatic_foods_dat_clean,here("data","OutputsFromR","cleaned_fcts","clean_fct_smiling_laos.csv"),row.names = FALSE)


