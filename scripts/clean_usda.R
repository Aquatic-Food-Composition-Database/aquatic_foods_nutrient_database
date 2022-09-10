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

raw_usda <- read_csv(file=here("data","usa","usda_api_extract.csv"))

# and read the excel merge key used to modify names and units to merge with AFCD
merge_key <- read_excel(
  here("data","database_merge_key.xlsx"),
  sheet="key"
)
# first export variables names and add to the excel file with the key to merge/transform names
# "database_merge_key.xlsx"
usda_nutrient_info <- rbind(
  data.frame(
    variable_name=c("fdc_id","food_description","scientific_name"),
    nutrient_unit="none"
    ),
  data.frame(
    variable_name=raw_usda$nutrient_name,
    nutrient_unit=raw_usda$nutrient_unit
    )
  ) %>%
  distinct() %>%
  drop_na(variable_name) %>%
  write_csv(here("data","outputsFromR","usda_variable_names.csv"))

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
    scientific_name= str_trim(scientific_name,side=c("both")) # trim food names
    ) %>%
  merge(
    
  )
# modify nutrient names for merge with AFCD
  
  

# extract specific product types
raw.usda.data <- usda.relevant.data[grep(", raw", usda.relevant.data$food.name), ]
canned.usda.data <- usda.relevant.data[grep(", canned", usda.relevant.data$food.name), ]
cooked.usda.data <- usda.relevant.data[grep(", cooked", usda.relevant.data$food.name), ]
baked.usda.data <- usda.relevant.data[grep(", baked", usda.relevant.data$food.name), ]

# and store
write.csv(all.usda.relevant.data,
          here("OutputsFromR","raw.usda.data.csv")
)



