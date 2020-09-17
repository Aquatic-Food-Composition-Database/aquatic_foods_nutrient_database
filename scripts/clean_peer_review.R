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
library(tidyverse);library(here);library(data.table)


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
) %>%
  dplyr::select(-X)
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
# before merging, determine the metadata that we can use that to merge the data
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
# merge datasets together
# of note, the first 26 rows in each of these should be the same across all datasets
##################



# now bind all the nutrient data frames together 
# note this also ensures that all names used in the merge are of the same type
# fats <- fats %>% mutate(across(all_of(name_intersect),as.character))

all_nutrients <- full_join(
  macro %>% mutate(across(all_of(name_intersect),as.character)),
  amino %>% mutate(across(all_of(name_intersect),as.character)),
  by=name_intersect) %>% distinct()
all_nutrients_1 <- full_join(
  all_nutrients,
  vitamin %>% mutate(across(all_of(name_intersect),as.character)),
  by=name_intersect) %>% distinct()
all_nutrients_2 <- full_join(
  all_nutrients_1,
  minerals %>% mutate(across(all_of(name_intersect),as.character)),
  by=name_intersect) %>% distinct()
all_nutrients_3 <- full_join(
  all_nutrients_2,
  fats %>% mutate(
    across(all_of(name_intersect),as.character),
    Edible.portion.coefficient_est = as.character(Edible.portion.coefficient_est)
    ),
  by=name_intersect) %>% distinct()
all_nutrients_4 <- full_join(
  all_nutrients_3,
  misc %>% mutate(across(all_of(name_intersect),as.character)),
  by=name_intersect) %>% distinct()

##################
# for now REMOVE relative values, those are cleaned & calculated in "clean_pere_review_relative_values.R"
##################
all_nutrients_no_relatives <- all_nutrients_4 %>%
  select(
    -str_subset(names(all_nutrients_4),"PAA."), #for now removes 
    -str_subset(names(all_nutrients_4),"PP."),
    -str_subset(names(all_nutrients_4),"PFA."),
    -str_subset(names(all_nutrients_4),"X."), #old artifact of merging (false rownames)
    -str_subset(names(all_nutrients_4),"_sd"), #removes the standard deviation 
    -Fatty.acid.22.1.n9.fatty.acid.22.1.n11 #duplicated
  ) %>%
  rename_all(funs(
    stringr::str_replace_all( ., "_est", "" )
  ))



##################
# clean for merge with existing AFCD data
##################

write.csv(
  all_nutrients_no_relatives,
  here("data","OutputsFromR","cleaned_fcts",
       "clean_peer_review.csv"
       ),
  row.names = FALSE
)






