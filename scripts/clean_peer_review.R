##################
# Cleans data collected from peer review studies for merge with AFCD
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
# BEFORE merging the datasets together, 
# calculate the absolute amino and fatty acid information
##################

# total lipid/amino values are found in macros





##################
# merge datasets together
# of note, the first 26 rows in each of these should be the same across all datasets
##################


# now determine the metadata that we can use to intersect
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

# now bind all the nutrient data frames together
# bind_rows 'smartly' binds by shared column names without specifying name_intersect
all_nutrients <- bind_rows(
  macro,
  amino,
  vitamin,
  minerals,
  fats,
  amino,
  misc
) %>%
  distinct() # creates some duplicate rows, remove those

##################
# clean for merge with existing AFCD data
##################







