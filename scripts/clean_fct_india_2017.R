#============================================================
# Cleaning India national FCT from 2017

#Name: J. Zachary (Zach) Koehn
#Email: zkoehn@stanford.edu
#Date started: 05/22/2020
#Revised: 04/09/2024
#============================================================

#__________________________________________
# read data and load libraries directory defaults
# _________________________________________
library(tidyverse);library(dplyr);library(here);library(readxl);library(data.table)

# source script of functions
source(here("scripts","functions","func_cleaning_fct.R"))


# load data
list_files <- list.files(here("data","india_fct_2017","ifct2017 compositions master assets"),pattern="\\.csv$")
india_dat_list <- lapply(list_files,function(x) read_csv(here("data","india_fct_2017","ifct2017 compositions master assets",x)))

  
# and read the excel merge key used to modify names and units to merge with AFCD
merge_key <- read_excel(
  here("data","database_merge_key.xlsx"),
  sheet="key"
)


#__________________________________________
# extract aquatic foods information and 
# cleaning variable names
# _________________________________________

# binds together CSV files from the India Food Composition Table data on GitHub
# completes cleaning to extract/separate estimates from standard deviations,
# excludes non-aquatic food information
# re-formats data to align with other FCT data structures for cleaning
india_dat_df <- rbindlist(india_dat_list,fill=TRUE) %>%
  mutate(
    across(water:tfapu,as.character)
  ) %>%
  pivot_longer(water:tfapu,names_to="nutrients",values_to="value") %>%
  drop_na(value) %>%
  filter(
    str_detect(code,"P") #code filters out any food that is not aquatic
  ) %>%
  separate(name,into=c("name","scientific_name"),sep=" \\(") %>%
  mutate(scientific_name=str_replace_all(scientific_name,"\\)","")) %>%
  separate(value,into=c("estimate","sd"),sep="±") %>%
  # for now, remove the standard deviations, in future, could include these standard deviations
  select(-sd) %>%
  mutate(
    estimate=as.numeric(estimate)
  ) %>%
  select(code:estimate) %>%
  drop_na(estimate) %>%
  pivot_wider(names_from=nutrients,values_from=estimate)



#__________________________________________
# Convert nutrient units to those used in AFCD (using merge_key file)
# _________________________________________
# run user-written function that converts nutrient measurement units if needed

india_conversion_coefs <- coefs_convert_unit_fct(key=merge_key,
                                                  original_unit="india_unit",
                                                  convert_to_unit="AFCD_unit",
                                                  variables_to_convert="india_variable_name") %>%
  slice(-1:-3) %>%
  select(india_variable_name,coefs) #just select the variable name and conversion coefficient


# now take all that information and create a named vector, with JUST the conversion coefficients named by the nutrient to be converted
coefs <- as.numeric(india_conversion_coefs$coefs)
india_names <- india_conversion_coefs$india_variable_name
names(coefs) <- india_names #names it

# now figure out which variables match from the conversion vector to 
name_match <- intersect(names(india_dat_df), names(coefs))
india_dat_df[name_match] <- sweep(india_dat_df[name_match], 2, unlist(coefs[name_match]), `*`)



#__________________________________________
# Convert nutrient names to those used in AFCD (using merge_key file)
# _________________________________________
# use function in "functions/func_cleaning_fct.r" create dataframe that 
# includes variable names to change from Aus to AFCD
india_names_to_convert_to_afcd <- convert_nutrient_names("india_variable_name") 


india_aquatic_foods_dat_clean <- india_dat_df %>%
  data.table::setnames(
    old=india_names_to_convert_to_afcd$original_dataset,
    new=india_names_to_convert_to_afcd$AFCD_variable_name,
    skip_absent = TRUE
    )
# if this throws an error related to the old not being int he new... it's usually because the formatting of those 
# variables is wrong. 

india_aquatic_foods_dat_clean$Country.ISO3 <- "IND" #adds classification for PNDB
india_aquatic_foods_dat_clean$Preparation <- "raw" #FCT readme "xcept for eggs, all other food component data are for foods in the raw form.'
india_aquatic_foods_dat_clean$Parts <- "muscle tissue" 

# change names of all the values to align with names in AFCD





#__________________________________________
# save the modified data frames to the folder
# _________________________________________
write.csv(india_aquatic_foods_dat_clean,here("data","OutputsFromR","cleaned_fcts","clean_fct_india_2017.csv"),row.names = FALSE)
india_aquatic_foods_dat_clean %>%
filter(
  Scientific.Name=="Aprion virescens"
) %>%
  select(Calcium)
