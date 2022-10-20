# ===============================================================================
# Cleans  USDA data from Food Data Central (extracted by API)
#
#
# Name: J. Zachary (Zach) Koehn
# Email: zkoehn@gmail.com
# Date started: 09/06/2022

# ===============================================================================
library(tidyverse)
library(here)
library(readxl)

raw_usda <- read_csv(file=here("data","usa","usda_api_extract.csv"))
source(here("scripts","functions","func_cleaning_fct.R"))
# and read the excel merge key used to modify names and units to merge with AFCD
merge_key <- read_excel(
  here("data","database_merge_key.xlsx"),
  sheet="key"
)
# first export variables names and add to the excel file with the key to merge/transform names
# "database_merge_key.xlsx"
# usda_nutrient_info <- rbind(
#   data.frame(
#     variable_name=c("fdc_id","food_description","scientific_name"),
#     nutrient_unit="none"
#     ),
#   data.frame(
#     variable_name=raw_usda$nutrient_name,
#     nutrient_unit=raw_usda$nutrient_unit
#     )
#   ) %>%
#   distinct() %>%
#   drop_na(variable_name) %>%
#   write_csv(here("data","outputsFromR","usda_variable_names.csv"))

usda_clean <- raw_usda %>%
  # clean scientific names, remove names of taxonomists from scientific name for merge with AFCD
  mutate(
    scientific_name = str_replace_all(scientific_name,"\\(.*","\\"), #removes taxonimist identifier specified by "(Taxonomist surname)"
    scientific_name = str_replace_all(scientific_name," L.",""),# remove taxonomist identifer
    scientific_name = str_replace_all(scientific_name," Tilesius",""),# remove taxonomist identifier Tilesius
    scientific_name = str_replace_all(scientific_name," Milne-Edwards",""),# remove taxonomist identifier Milne-Edwards
    scientific_name = str_replace_all(scientific_name," Valenciennes",""),# remove taxonomist identifer
    scientific_name = str_replace_all(scientific_name," L.",""),# remove taxonomist identifer
    scientific_name = str_replace_all(scientific_name," Girard",""),# remove taxonomist identifer
    scientific_name = str_replace_all(scientific_name," Richardson",""),# remove taxonomist identifer
    scientific_name = str_replace_all(scientific_name," Goode and Bean",""),# remove taxonomist identifer
    scientific_name = str_replace_all(scientific_name," Lamarck",""),# remove taxonomist identifer
    scientific_name = str_replace_all(scientific_name," Rathbun",""),# remove taxonomist identifer
    scientific_name = str_replace_all(scientific_name," Dana",""),# remove taxonomist identifer
    scientific_name= str_trim(scientific_name,side=c("both")), # trim food names
    nutrient_name=ifelse((nutrient_name=="Energy" & nutrient_unit=="kcal"),"Energy kcal",nutrient_name),
    nutrient_name=ifelse((nutrient_name=="Energy" & nutrient_unit=="kJ"),"Energy kj",nutrient_name)
    ) %>%
  drop_na(nutrient_value) %>%
  select(-nutrient_unit) %>%
  distinct() %>%
  pivot_wider(names_from=nutrient_name,values_from=nutrient_value)

# modify nutrient names for merge with AFCD
  
# run user-written function that converts nutrient measurement units if needed

usda_conversion_coefs <- coefs_convert_unit_fct(
  key=merge_key,
  original_unit="usda_unit",
  convert_to_unit="AFCD_unit",
  variables_to_convert="usda_variable_name"
  ) %>%
  slice(-1:-3,) %>%
  select(usda_variable_name,coefs) #just select the variable name and conversion coefficient

# now take all that information and create a named vector, with JUST the conversion coefficients named by the nutrient to be converted
coefs <- as.numeric(usda_conversion_coefs$coefs)
usda_names <- usda_conversion_coefs$usda_variable_name
names(coefs) <- usda_names #names it

# now figure out which variables match from the conversion vector to 
name_match <- intersect(names(usda_clean), names(coefs))
usda_clean[name_match] <- sweep(usda_clean[name_match], 2, unlist(coefs[name_match]), `*`)



# finally, change names so that it can be readily merged with AFCD

# use function in "functions/func_cleaning_fct.r" create dataframe that 
# includes variable names to change from usda to AFCD
# note: usda Sodium variable was called "NA", which doesn't play nice in programming so it was modified in
# the CSV to "SODIUM"

usda_names_to_convert_to_afcd <- convert_nutrient_names("usda_variable_name") 


usda_aquatic_foods_dat_clean <- usda_clean %>%
  data.table::setnames(
    old=usda_names_to_convert_to_afcd$original_dataset,
    new=usda_names_to_convert_to_afcd$AFCD_variable_name,
    skip_absent=TRUE)
# if this throws an error related to teh old not being int he new... it's usually because the formatting of those 
# variables is wrong. 

usda_aquatic_foods_dat_clean$Country.ISO3 <- "USA_USDA_2022" #adds classification for  usda

usda_aquatic_foods_dat_clean <- usda_aquatic_foods_dat_clean %>%
  mutate(
    Scientific.Name=ifelse(Scientific.Name=="none",NA,Scientific.Name)
    )



prep_to_remove <- "\\(|\\)|, raw.*|, dried.*|, dry*|, crab cakes.*|, fresh|, cooked.*|, canned.*|, pickled.*|, rehydrated*|, salted.*, batter.*|, kippered.*|, smoked.*|, steamed or poached*|, farmed.*|, wild.*|, breaded and fried*|bay and sea*"
# funciton to fill as many 
filled_sciname <- sapply(
  1:dim(usda_aquatic_foods_dat_clean)[1], function(x) {
    if(is.na(usda_aquatic_foods_dat_clean[x,]$Scientific.Name)==TRUE) {
        food_name <- usda_aquatic_foods_dat_clean[x,]$Food.name.in.English
        
        food_name <- str_replace_all(food_name,prep_to_remove,"")
        food_name_match_vec <- str_replace_all(usda_aquatic_foods_dat_clean$Food.name.in.English,prep_to_remove,"")
        matched_vec <- str_detect(str_to_lower(food_name_match_vec),str_to_lower(food_name))
        matched_sciname <- na.omit(as.data.frame(usda_aquatic_foods_dat_clean$Scientific.Name[matched_vec]))[1,]
        filled_sciname <- matched_sciname
      } else {
        filled_sciname <- usda_aquatic_foods_dat_clean[x,]$Scientific.Name
      }
      return(filled_sciname)
    }
  )


usda_aquatic_foods_dat_clean <- usda_aquatic_foods_dat_clean %>%
  mutate(
    Scientific.Name=filled_sciname,
    Scientific.Name=ifelse(str_detect(Food.name.in.English,"sockeye"),"Oncorhynchus nerka",Scientific.Name),
    Scientific.Name=ifelse(str_detect(Food.name.in.English,"tilapia"),"Oreochromis spp.",Scientific.Name),
    Scientific.Name=ifelse(str_detect(Food.name.in.English,"Fish, tuna") & is.na(Scientific.Name),"Scombridae",Scientific.Name)
  )



# and store
write_csv(usda_aquatic_foods_dat_clean,
          here("data","OutputsFromR","cleaned_fcts","clean_usda_food_data_central.csv")
)



