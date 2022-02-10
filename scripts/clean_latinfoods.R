#============================================================
# Cleaning FAO LATINFOODS

#Name: J. Zachary (Zach) Koehn
#Email: zkoehn@stanford.edu
#Date started: 06/26/2020
#Revised: 05/22/2020
#============================================================

#__________________________________________
# read data and load libraries directory defaults
# _________________________________________
library(tidyverse);library(dplyr);library(here);library(readxl);library(data.table)

# source script of functions
source(here("scripts","functions","func_cleaning_fct.R"))


latinfoods_file <- "Tabla de Composición Alimentos América Latina al 2010-EXCEL Editada para pag WEB.xlsx - TCA LA.csv"
merge_key_file <- "database_merge_key.xlsx"

latinfoods_dat <- read.csv(
  here("data","LATINFOODS",latinfoods_file), 
  header = TRUE)

# and read the excel merge key used to modify names and units to merge with AFCD
merge_key <- read_excel(
  here("data",merge_key_file),
  sheet="key"
)


# subset for aquatic foods
# Food Codes for aquatic fish starts with E

variables_to_remove <- c("ALIMENTO",
  "TIPO",
  "CEPA",
  "GRADO",
  "MENSAJE",
  "VARIEDAD",
  "NOMBRE_ALT",
  "NOMBRE_COMERCIAL",
  "GENERO",
  "ESPECIE"
)

latinfoods_aquatic_foods_dat <- latinfoods_dat[grep("^E", latinfoods_dat$CODIGO),] %>%
  mutate(scientific_name = paste(GENERO,ESPECIE)) %>% # add scientific names and drop teh genus and species columns
  select(-all_of(variables_to_remove)) # remove columns that are not needed with AFCD

# now (tidyverse is VERY annoying in that you can't pipeline this at this time (without indecipherable errors at least))
latinfoods_aquatic_foods_dat[,8:33] <- apply(latinfoods_aquatic_foods_dat[,8:33],2,function(x) 
  gsub(x,pattern = ",",replacement = ".") # change comma to decimal
)

latinfoods_aquatic_foods_dat[,8:33] <- apply(latinfoods_aquatic_foods_dat[,8:33],2,function(x) 
  as.numeric(x) #convert to numeric
)

# run user-written function that converts nutrient measurement units if needed

latinfoods_conversion_coefs <- coefs_convert_unit_fct(key=merge_key,
                                                 original_unit="latinfoods_unit",
                                                 convert_to_unit="AFCD_unit",
                                                 variables_to_convert="latinfoods_variable_name") %>%
  slice(-1:-6,-32,-33) %>%
  select(latinfoods_variable_name,coefs) #just select the variable name and conversion coefficient

# now take all that information and create a named vector, with JUST the conversion coefficients named by the nutrient to be converted
coefs <- as.numeric(latinfoods_conversion_coefs$coefs)
latinfoods_names <- latinfoods_conversion_coefs$latinfoods_variable_name
names(coefs) <- latinfoods_names #names it

# now figure out which variables match from the conversion vector to 
name_match <- intersect(names(latinfoods_aquatic_foods_dat), names(coefs))
latinfoods_aquatic_foods_dat[name_match] <- sweep(latinfoods_aquatic_foods_dat[name_match], 2, unlist(coefs[name_match]), `*`)



# finally, change names so that it can be readily merged with AFCD

# use function in "functions/func_cleaning_fct.r" create dataframe that 
# includes variable names to change from LATINFOODS to AFCD
# note: LATINFOODS Sodium variable was called "NA", which doesn't play nice in programming so it was modified in
# the CSV to "SODIUM"

latinfoods_names_to_convert_to_afcd <- convert_nutrient_names("latinfoods_variable_name") 


latinfoods_aquatic_foods_dat_clean <- latinfoods_aquatic_foods_dat %>%
  data.table::setnames(
    old=latinfoods_names_to_convert_to_afcd$original_dataset,
    new=latinfoods_names_to_convert_to_afcd$AFCD_variable_name)
# if this throws an error related to teh old not being int he new... it's usually because the formatting of those 
# variables is wrong. 

latinfoods_aquatic_foods_dat_clean$Country.ISO3 <- "FAO.latinfoods" #adds classification for FAO Latinfoods




# change names of all the values to align with names in AFCD





# save the modified data frames to the folder
write.csv(latinfoods_aquatic_foods_dat_clean,here("data","OutputsFromR","cleaned_fcts","clean_latinfoods.csv"),row.names = FALSE)


