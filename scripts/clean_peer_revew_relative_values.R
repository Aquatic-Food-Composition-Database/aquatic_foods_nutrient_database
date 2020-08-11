##################
# Cleans data collected from peer review studies for merge with AFCD
# calculates absolute values from relative values 
# 
# creator: Zach Koehn
# email: zkoehn@stanford.edu
# date started: 07/14/2020
# last date modified: 07/16/2020
##################

##################
# load needed packages and data sets
##################
library(tidyverse);library(here)


# load nutrient information from Camille's datasets in our drive folder
# note this uses the here package, which locates the files based on whatever folder the Rproject is stored in
macro <- read.csv(
  here("data","afcd_peer_review_data","Seafood nutrients","macro nutrients.csv"),
  header=TRUE
)
amino <- read.csv(
  here("data","afcd_peer_review_data","Seafood nutrients","amino acids.csv"),
  header=TRUE
)
fats <- read.csv(
  here("data","afcd_peer_review_data","Seafood nutrients","fatty_acids.csv"),
  header=TRUE
)
minerals <- read.csv(
  here("data","afcd_peer_review_data","Seafood nutrients","minerals.csv"),
  header=TRUE
)
misc <- read.csv(
  here("data","afcd_peer_review_data","Seafood nutrients","misc..csv"),
  header=TRUE
)
vitamin <- read.csv(
  here("data","afcd_peer_review_data","Seafood nutrients","vitamins.csv"),
  header=TRUE
)


##################
# before merging, determine the metadata that we can use to intersect
##################

name_intersect <- Reduce(intersect,
                         list(
                           names(macro),
                           names(amino),
                           names(fats),
                           names(minerals),
                           names(misc),
                           names(vitamin)
                         )
)

##################
# calculate the absolute amino and fatty acid information from studies where it is relative
##################

# Directions from Camille Desisto: 
# The fatty acid columns preceded by "PFA." are values that are expressed in as the percent of total fatty acids or the percent of total lipids/ fats. 
# The amino acids column preceded by "PP." are values that are expressed in percent of protein 
# and those preceded by "PAA" are expressed in terms of the percent of total amino acids. 
# 
# Total protein and fat/lipid values are in the macronutrients sheet 


# bind to macro data and extract relative information for aminos, protein and fatty acids
# and multiply each of the relative values by the absolute (total) values
macro <- macro %>%
  mutate(across(Nitrogen.nonprotein_est:hydrogen.total_sd, as.numeric)) 


relative_aminos <- merge(macro,amino,by= name_intersect) %>%
  distinct() %>%
  select(
    all_of(name_intersect),
    str_subset(names(amino),"PAA.")
  ) 
relative_proteins <- amino %>%
  select(
    all_of(name_intersect),
    str_subset(names(amino),"PP.")
  )
relative_fats <- fats %>%
  select(
    all_of(name_intersect),
    str_subset(names(fats),"PFA.")
  )
