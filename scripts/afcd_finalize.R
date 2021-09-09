# ===============================================================================
# Cleans merged AFCD database
# exports to CSV in live folder 

#
#
# Name: J. Zachary (Zach) Koehn
# Email: zkoehn@stanford.edu
# Date started: 08/17/2017
# Revised: 04/28/2021
# ===============================================================================


#_____________________________________________________________________________________________
# read data and load libraries directory defaults
# ________________________________________________________________________________________________
directory <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Nutrition/Separate/aquatic_foods_nutrient_database"


library(tidyverse)

afcd_dat <- read.csv(
  file.path(directory,"data","OutputsFromR","afcd_with_taxa.csv"),
  header = TRUE)



source(
  file.path(directory,"scripts","afcd_clean_categories.R")
)

afcd_dat_clean <- afcd_dat %>%
  mutate(
    parts_of_food = case_when(
      Parts %in%  muscle_tissue ~ "muscle_tissue",
      Parts %in%  whole ~ "whole",
      Parts %in% reproductive_tissue ~ "reproductive_tissue",
      Parts %in% body_wall ~ "body_wall",
      Parts %in% gills ~ "gills",
      Parts %in% unknown_part ~ "unknown_part",
      Parts %in% mantle ~ "mantle",
      Parts %in% blade ~ "blade",
      Parts %in% stipe ~ "stipe",
      Parts %in% holdfast ~ "holdfast",
      Parts %in% edible ~ "edible",
      Parts %in% raw ~ "raw",
      Parts %in% skin ~ "skin",
      Parts %in% liver ~ "liver",
      Parts %in% viscera ~ "viscera",
      Parts %in% roe ~ "roe",
      Parts %in% blubber ~ "blubber",
      Parts %in% frond ~ "frond",
      Parts %in% bone ~ "bone",
      Parts %in% flippers ~ "flippers",
      Parts %in% heart ~ "heart",
      Parts %in% gutted ~ "gutted",
      Parts %in% oil ~ "oil",
      Parts %in% larvae ~ "larvae",
      Parts %in% head ~ "head",
      Parts %in% kidney ~ "kidney",
      Parts %in% combination ~ "combination",
      Parts %in% gelatin ~ "gelatin"
    ),
    preparation_of_food = case_when(
      Preparation %in% frozen ~ "frozen",
      Preparation %in% raw ~ "raw",
      Preparation %in% freezedried~  "freeze_dried",
      Preparation %in% unknown_preparation ~ "unknown_preparation",
      Preparation %in% dried ~ "dried",
      Preparation %in% baked ~ "baked",
      Preparation %in% boiled_steamed ~ "boiled_steamed",
      Preparation %in% fried ~ "fried",
      Preparation %in% canned ~ "canned",
      Preparation %in% smoked ~ "smoked",
      Preparation %in% boiled_steamed ~ "steamed",
      Preparation %in% microwaved~ "microwaved",
      Preparation %in% cooked ~ "cooked",
      Preparation %in% grilled ~ "grilled",
      Preparation %in% acid_digestion ~ "acid_digestion",
      Preparation %in% salted ~ "salted",
      Preparation %in% aged ~ "aged",
      Preparation %in% curried ~ "curried",
      Preparation %in% combination ~ "combination"
    ),
    production_category = case_when(
      Wild.Farmed %in% c("w","wild","Wild","wild ") ~ "wild_capture",
      Wild.Farmed %in% c("Captive","darmed","f","farm","farmed","Farmed","farmed ") ~ "farmed",
      Wild.Farmed %in% c("","NA","unknown","Retail Bought ","micropropegated","mix") ~ "unknown",
      is.na(Wild.Farmed) ~ "unknown"
    ), 
    parts_of_food = ifelse(is.na(parts_of_food),"small sample",parts_of_food),
    preparation_of_food = ifelse(is.na(preparation_of_food),"small sample",preparation_of_food)
  ) %>%
  # select(-Wild.Farmed,-Parts,-Preparation) %>%
  mutate(
    Study.ID.number=ifelse(is.na(Study.ID.number),Country.ISO3,Study.ID.number),
    Study.ID.number=str_replace_all(Study.ID.number,"BGD","Bangladesh_2013"),
    # Korea assumption here, that this is 8th and not 9th revision (published in 2018), because
    # this was part of the older dataset that I believe was used in the 2016 Nature paper
    Study.ID.number=str_replace_all(Study.ID.number,"KOR","Korea_8thRev_2011"), 
    # NZ assumption here, that this is 12th edition, published in 2016, the 11th was published in 2013
    Study.ID.number=str_replace_all(Study.ID.number,"NZL","NewZealand_12th_2016"), 
    #MOZ assumption this comes from the database in FAO-INFOODS, link here: http://www.fao.org/infoods/infoods/tables-and-databases/africa/en/
    Study.ID.number=str_replace_all(Study.ID.number,"MOZ","Mozambique_V2_2011"), 
    # ARG assumption is this comes from ARGENFOODS, linked from FAO-INFOODS: http://www.unlu.edu.ar/~argenfoods/Tablas/Tabla.htm
    Study.ID.number=str_replace_all(Study.ID.number,"ARG","Argentina_2010"), 
    # JPN assumption is this comes from a link in FAO-INFOODS, link here: https://www.mext.go.jp/en/policy/science_technology/policy/title01/detail01/sdetail01/sdetail01/1385122.htm
    Study.ID.number=str_replace_all(Study.ID.number,"JPN","Japan_7thRev_2015"), 
    # GMB assumption is this comes from a link in FAO-INFOODS, link to pdf here:http://www.fao.org/fileadmin/templates/food_composition/documents/pdf/Gambia04082011.pdf
    Study.ID.number=str_replace_all(Study.ID.number,"GMB","Gambia_2011"), 
    # CAN assumption is this comes from a link in FAO-INFOODS, link here: https://www.canada.ca/en/health-canada/services/food-nutrition/healthy-eating/nutrient-data/canadian-nutrient-file-2015-download-files.html
    Study.ID.number=str_replace_all(Study.ID.number,"CAN","Canada_CNF_2015"),
    # UK assumption, I believe this is from 2015 adn NOT the newest 2021: https://www.gov.uk/government/publications/composition-of-foods-integrated-dataset-cofid?utm_source=MW7+List+March+2015&utm_campaign=947c9d4b28-Newsletter_2_December_2013_FINAL12_13_2013&utm_medium=email&utm_term=0_3b8ecbdaea-947c9d4b28-95444717
    Study.ID.number=str_replace_all(Study.ID.number,"GBR","UK_2015"), 
    # Unclear what this one is!! guessing cambodia
    Study.ID.number=str_replace_all(Study.ID.number,"KHM","KHM"), 
    # Unclear what this one is!! guessing Chile but not in FAO-INFOODS
    Study.ID.number=str_replace_all(Study.ID.number,"CHL","Chile_2010"), 
    # Unclear what this one is!! guessing Finland but FAO-INFOODS only has a link to a website with no excel file 
    Study.ID.number=str_replace_all(Study.ID.number,"FIN","FIN"), 
    # Unclear what this one is!! guessing Poland but FAO-INFOODS only has a link to a website with no excel file 
    Study.ID.number=str_replace_all(Study.ID.number,"POL","POL"), 
    # Unclear what this one is!! guessing Russia but FAO-INFOODS only has a link to very old, undigitized, literature
    Study.ID.number=str_replace_all(Study.ID.number,"RUS","RUS"), 
    # Unclear what this one is!! guessing Greece but FAO-INFOODS only has a broken link, as well as the compilers
    Study.ID.number=str_replace_all(Study.ID.number,"GRC","GRC"),
    # Unclear what this one is!! guessing Peru but FAO-INFOODS only has a broken link, as well as the compilers
    Study.ID.number=str_replace_all(Study.ID.number,"PER","PER"),
    # Unclear what this one is!! guessing Israel but FAO-INFOODS only has a broken link, as well as the compilers
    Study.ID.number=str_replace_all(Study.ID.number,"ISL","ISL"),
    # Unclear what this one is!! guessing Malaysia, but the food codes and common product descriptions (where they exist) do not match up
    Study.ID.number=str_replace_all(Study.ID.number,"MYS","Malaysia_1997"),
    # Unclear what this one is!! guessing Italy, but the food codes and common product descriptions (where they exist) do not match up
    Study.ID.number=str_replace_all(Study.ID.number,"ITA","Italy_FCT_2009"), 
    # Unclear what this one is!! guessing Turkey, but the food codes and common product descriptions (where they exist) do not match up
    Study.ID.number=str_replace_all(Study.ID.number,"TUR","Turkey_FCT_2014"), 
    # Unclear what these are!!
    # Study.ID.number=str_replace_all(Study.ID.number,"",""),
    Study.ID.number=str_replace_all(Study.ID.number,"XXX","XXX")
  ) %>%
  filter(!Study.ID.number %in% 
           c("KHM","ISL","PER","RUS","FIN","GRE","POL","XXX") #removing from live version because we are unsure of the citation (very old data)
  ) %>%
  # fix error where NAs were imported as 
  mutate(
    Iodine = ifelse(Iodine==0 & Study.ID.number %in% c('Japan_7thRev_2015'),NA,Iodine)
  )
 
afcd_dat_clean %>%
  # filter(Study.ID.number %in% c('Japan_7thRev_2015','Australia_2019')) %>%
  select(taxa_name,Study.ID.number,Iodine) %>%

  filter(Study.ID.number %in% c('Japan_7thRev_2015'),Iodine==0)


write.csv(afcd_dat_clean,
          file.path(directory,"data","OutputsFromR","aquatic_food_composition_database","AFCD_live.csv"),
          row.names=FALSE
)



