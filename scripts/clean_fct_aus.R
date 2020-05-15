#============================================================
# Cleaning AUS national 2019 food compositiont tables 
# for merge with AFCT

#Name: J. Zachary (Zach) Koehn
#Email: zkoehn@stanford.edu
#Date started: 05/01/2020
#Revised: 05/14/2020
#============================================================



#__________________________________________
# read data and load libraries directory defaults
# _________________________________________
library(tidyverse);library(dplyr);library(here);library(readxl)
Sys.setlocale('LC_ALL','C') #sets language to eliminate multibyte error if it arrises

# source script of functions
source(here("scripts","functions","func_cleaning_fct.R"))

# load raw data files
food_details_file <- "Release1_Food_details_file.xlsx"
food_database_file <- "Release1_Food_nutrient_database.xlsx"
merge_key_file <- "database_merge_key.xlsx"
conversion_table_file <- "unit_conversion_tables.csv"

# now use here package to navigate from working directory to specific food file
food_details_dat <- read_excel(
  here("data","australia_database",food_details_file), 
  sheet="Release 1 - Food File")

food_database_dat <- read_excel(
  here("data","australia_database",food_database_file),
  sheet="Solid per 100g & liq per 100mL",
  col_names=FALSE,skip=2)

# and read the excel merge key used to modify names and units to merge with AFCD
merge_key <- read_excel(
  here("data",merge_key_file),
  sheet="key"
  )


# __________________________________________
# First, subset a data frame the aquatic food product codes we care about
# using the food details dataset
# __________________________________________

aquatic_foods_classifications <- c( #vector of aquatic food types
  "Seaweeds",
  "Fin fish, fresh, frozen",
  "Fin fish, battered or crumbed",
  "Eel",
  "Molluscs, fresh, frozen",
  "Crustacea, fresh, frozen",
  "Packed fin fish",
  "Fish and seafood products",
  "Smoked fish"
  )

# subset details dataset for aquatic foods
aquatic_food_details_dat <- food_details_dat %>%
  rename(food_category='Classification Name') %>%
  filter(food_category %in% aquatic_foods_classifications)
  
names(aquatic_food_details_dat)
names(food_database_dat)

# __________________________________________
# Clean up nutrient database
# __________________________________________

# first, whenever a nutrient has multiple units of measurement the excel file has a merged value
# across the nutrient name, when this is read into R, the second of the merged cells is NA, so need to 
# fill in nutrient names for all second merged cells

food_database_dat[1,2:dim(food_database_dat)[2]] <- sapply(2:dim(food_database_dat)[2],
       function(x)
    if(is.na(food_database_dat[1,x])==TRUE) {food_database_dat[1,x] <- food_database_dat[1,x-1]} else {
      food_database_dat[1,x] <- food_database_dat[1,x]
    }
  )

food_database_dat[1,173:180]

# second we deal with the nutrients where there are multiple measurements. In all cases, the 
# first nutrient measurement is some ratio value (e.g., %) whereas the second is the gram/mg mass 
# measurement we want. So, for any nutrient with two measurements, 
# we can take the second measurement and drop the first. 
nutr_names <- as.character(food_database_dat[1,])

duplicated(nutr_names,incomparables=TRUE)[108:115]
#bit of weird logic, but this allows me to select ONLY columns with a) unique values and b) the second nutrient unit measurement (i.e. *not* the ratio measurement)
food_database_dat <- food_database_dat[,duplicated(nutr_names,incomparables=TRUE,fromLast = TRUE)==FALSE]



# create new names that include nutrients and units
new_colnames <-  sapply(1:dim(food_database_dat)[2], function(x) clean_colnames_aus_fct(dat=food_database_dat,x=x))
colnames(food_database_dat) <- new_colnames #set these new names as the column names
food_database_dat <- food_database_dat[-c(1,2),] #and remove the old units


# __________________________________________
# Merge food details with nutrient database
# __________________________________________

# use "Public Food Key", food name and classification ID variables as keys to merge, merge onto aquatic_food_details_dat

aquatic_foods_database_dat <- merge(aquatic_food_details_dat,food_database_dat,all.x = TRUE, 
                                    by.x=c("Public Food Key","Classification ID","Name"),
                                    by.y=c("Public Food Key","Classification","Food Name")
)


# dim(aquatic_food_details_dat);dim(food_database_dat);dim(aquatic_foods_database_dat) # check data dimensions to make sure it merged correctly





# run user-written function that converts nutrient measurement units if needed

aus_conversion_coefs <- coefs_convert_unit_fct(key=merge_key,
                                               original_unit="Aus_unit",
                                               convert_to_unit="AFCD_unit",
                                               variables_to_convert="Aus_variable_name") %>%
  select(Aus_variable_name,coefs) %>% #just select the variable name and conversion coefficient
  filter( Aus_variable_name != c("Scientific Name","Name"))  # and remove a few rows wher we have non-numeric values (matters for merge... but not for unit conversions)


# now take all that information and create a named vector, with JUST the conversion coefficients named by the nutrient to be converted
coefs <- as.numeric(aus_conversion_coefs$coefs)
aus_names <- aus_conversion_coefs$Aus_variable_name
names(coefs) <- aus_names #names it

# make sure that database and merge key variables are numeric so can be multiplied. 
aquatic_foods_database_dat[,15:dim(aquatic_foods_database_dat)[2]] <- sapply(
  aquatic_foods_database_dat[,15:dim(aquatic_foods_database_dat)[2]], function(x) as.numeric(x)
  )

# now figure out which variables match from the conversion vector to 
name_match <- intersect(names(aquatic_foods_database_dat), names(coefs))
aquatic_foods_database_dat[name_match] <- sweep(aquatic_foods_database_dat[name_match], 2, unlist(coefs[name_match]), `*`)




# finally, change names so that it can be readily merged with AFCD

# use function in "functions/func_cleaning_fct.r" create dataframe that 
# includes variable names to change from Aus to AFCD
aus_names_to_convert_to_afcd <- convert_nutrient_names("Aus_variable_name") 


aquatic_foods_database_dat <- aquatic_foods_database_dat %>%
  # select(-food_category,-Description) %>% #drops a couple of variables we don't need... Description is a really specific version of 'Name' variable
  data.table::setnames(
    old=aus_names_to_convert_to_afcd$original_dataset,
    new=aus_names_to_convert_to_afcd$AFCD_variable_name) %>%
  rename( # now rename the remaining variables 
    "aus_food_key" = "Public Food Key",
    "aus_classification_id"="Classification ID",
    "aus_info_origin"="Derivation",
    "aus_sampling_details"="Sampling details",
    "aus_nitrogen_factor"="Nitrogen Factor",
    "aus_specific_gravity"="Specific Gravity",
    "aus_analysed_portion"="Analysed portion",
    "aus_unanalyse_portion"="Unanalysed portion"
  )

aquatic_foods_database_dat$Country.ISO3 <- "AUS" #adds ISO country code for Australia

write.csv(aquatic_foods_database_dat,here("data","OutputsFromR","cleaned_fcts","clean_fct_aus.csv"),row.names = FALSE)
