#============================================================
# Merge cleaned FCT & peer review information with AFCD

#Name: J. Zachary (Zach) Koehn
#Email: zkoehn@stanford.edu
#Date started: 05/13/2020
#Revised: 08/10/2020
#============================================================

afcd_file <- "AFCD.w.FAO.USDA.csv"
aus_file <- "clean_fct_aus.csv"
pndb_file <- "clean_fct_pndb.csv"
fao_wa_file <- "clean_fct_fao_west_africa.csv"
smiling_cambodia_file <- "clean_fct_smiling_cambodia.csv"
smiling_indonesia_file <- "clean_fct_smiling_indonesia.csv"
smiling_laos_file <- "clean_fct_smiling_laos.csv"
smiling_thailand_file <- "clean_fct_smiling_thailand.csv"
smiling_vietnam_file <- "clean_fct_smiling_vietnam.csv"
norway_file <-  "clean_fct_norway_2019.csv"
india_file <- "clean_fct_india_2017.csv"
latinfoods_file <- "clean_latinfoods.csv"
usda_file <- "clean_usda_food_data_central.csv"
jpn_algae_file <- "clean_jpn_algae_fct_2015.csv"
peer_review_file <- "clean_peer_review_master.csv"
#__________________________________________
# read data and load libraries directory defaults
# _________________________________________
library(tidyverse);library(here)

directory <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Nutrition/Separate/aquatic_foods_nutrient_database"
# now use here package to navigate from working directory to specific food file

afcd_dat <- read.csv(
  here::here("data","OutputsFromR",afcd_file)
  ) %>%
  mutate(
    Study.ID.number=case_when(
      Country.ISO3=="USA"~"USA_USDA_2019",
      Country.ISO3=="FAO.biodiv3"~"FAO_Biodiversity",
      Country.ISO3=="FAO.infoods.ufish1"~"FAO_Infoods_Ufish"
    )
  )
aus_dat <- read.csv(
  here::here("data","OutputsFromR","cleaned_fcts",aus_file)
  ) %>%
  mutate(
    Study.ID.number="Australia_2019"
  )
pndb_dat <- read.csv(
  here::here("data","OutputsFromR","cleaned_fcts",pndb_file)
  ) %>%
  mutate(
    Study.ID.number="PNDB_2020"
  )
fao_wa_dat <- read.csv(
  here::here("data","OutputsFromR","cleaned_fcts",fao_wa_file)
  ) %>%
  mutate(
    Study.ID.number="FAO_WestAfrica_2019"
  )
smiling_cambodia_dat <- read.csv(
  here::here("data","OutputsFromR","cleaned_fcts",smiling_cambodia_file)
  ) %>%
  mutate(
    Study.ID.number="SMILING_Cambodia"
  )
smiling_indonesia_dat <- read.csv(
  here::here("data","OutputsFromR","cleaned_fcts",smiling_indonesia_file)
  ) %>%
  mutate(
    Study.ID.number="SMILING_Indonesia"
  )
smiling_laos_dat <- read.csv(
  here::here("data","OutputsFromR","cleaned_fcts",smiling_laos_file)
) %>%
  mutate(
    Study.ID.number="SMILING_Laos"
  )
smiling_thailand_dat <- read.csv(
  here::here("data","OutputsFromR","cleaned_fcts",smiling_thailand_file)
) %>%
  mutate(
    Study.ID.number="SMILING_Thailand"
  )
smiling_vietnam_dat <- read.csv(
  here::here("data","OutputsFromR","cleaned_fcts",smiling_vietnam_file)
) %>%
  mutate(
    Study.ID.number="SMILING_Vietnam"
  )
norway_dat <- read.csv(
  here::here("data","OutputsFromR","cleaned_fcts",norway_file)
) %>%
  mutate(
    Study.ID.number="Norway_2019"
  )
india_dat <- read.csv(
  here::here("data","OutputsFromR","cleaned_fcts",india_file)
  ) %>%
  mutate(
    Study.ID.number="India_2017"
  )
latinfoods_dat <- read.csv(
  here::here("data","OutputsFromR","cleaned_fcts",latinfoods_file)
  ) %>%
  mutate(
    Study.ID.number="LATINFOODS"
  )
usda_foods_dat <- read.csv(
  here::here("data","OutputsFromR","cleaned_fcts",usda_file)
) %>%
  mutate(
    Study.ID.number="USA_USDA_2022"
  )
jpn_algae_dat <- read.csv(
  here::here("data","OutputsFromR","cleaned_fcts",jpn_algae_file)
) %>%
  mutate(
    Study.ID.number="Japan_7thRev_2015",
    across(acetic.acid:Zinc,as.numeric)
  )

peer_review_dat <- read.csv(
  here::here("data","OutputsFromR","cleaned_fcts",peer_review_file)
) %>%
  select(
    Study.ID.number:Notes.on.Laboratory.Analysis.methods.to.calculate.the.nutrient.composition.values,
    contains("_est")) %>%
  mutate(
    PAA.Aspartic.acid_est=ifelse(is.na(PAA.Aspartic.acid_est),PAA.Aspartic.acid.1_est.1,PAA.Aspartic.acid_est),
    PAA.Glutamine.gluamic.acid_est=ifelse(is.na(PAA.Glutamine.gluamic.acid_est),PAA.Glutamine.gluamic.acid_est.1,PAA.Glutamine.gluamic.acid_est)
  ) %>%
  select(-c(PAA.Aspartic.acid.1_est.1,PAA.Glutamine.gluamic.acid_est.1)) %>%
  mutate(
    across(-c(Study.ID.number:Notes.on.Laboratory.Analysis.methods.to.calculate.the.nutrient.composition.values),as.numeric)
  )
names(peer_review_dat) <- str_replace_all(names(peer_review_dat),"_est","")
  

#__________________________________________
# clean up the 
# _________________________________________
# a few of the FCT food codes do not have preceding character so read in as integers, need to convert to character
smiling_laos_dat$Original.FCT.Food.Code <- as.character(smiling_laos_dat$Original.FCT.Food.Code)
smiling_thailand_dat$Original.FCT.Food.Code <- as.character(smiling_thailand_dat$Original.FCT.Food.Code)
smiling_vietnam_dat$Original.FCT.Food.Code <- as.character(smiling_vietnam_dat$Original.FCT.Food.Code)
norway_dat$Original.FCT.Food.Code <- as.character(norway_dat$Original.FCT.Food.Code)
usda_foods_dat$Original.FCT.Food.Code <- as.character(usda_foods_dat$Original.FCT.Food.Code)
jpn_algae_dat$Original.FCT.Food.Code <- as.character(jpn_algae_dat$Original.FCT.Food.Code)
# peer_review_dat$ISSCAAP <- as.integer(peer_review_dat$ISSCAAP)
# peer_review_dat$FishBase.SAU.Code <- as.integer(peer_review_dat$FishBase.SAU.Code)
peer_review_dat<- peer_review_dat %>%
  mutate(
    Study.ID.number=as.character(Study.ID.number),
    Scientific.Name=as.character(Scientific.Name),
    peer_review="yes"
    )


#__________________________________________
# bind together all the variables
# _________________________________________
afcd_bind <- bind_rows(afcd_dat,
                       aus_dat,pndb_dat,fao_wa_dat,
                       smiling_cambodia_dat,smiling_indonesia_dat,
                       smiling_laos_dat,smiling_thailand_dat,
                       smiling_vietnam_dat,norway_dat,
                       india_dat,latinfoods_dat,
                       usda_foods_dat,
                       jpn_algae_dat,
                       peer_review_dat
                       )

#__________________________________________
# remove variables for which there was no 
# overlap with the nutrients included in AFCD
# _________________________________________
afcd_names <- names(afcd_dat)

no_afcd_names <- setdiff(names(afcd_bind),afcd_names)

rando_numbers <- 
  afcd_bind %>%
  select(Study.ID.number,no_afcd_names) %>%
  select_if(is.numeric)

variable_only_na <- cbind(afcd_bind$Study.ID.number,rando_numbers) %>%
  rename(Study.ID.number='afcd_bind$Study.ID.number') %>%
  pivot_longer(cols=-Study.ID.number) %>%
  # drop_na(value) %>%
  group_by(name) %>%
  summarize(
    num_nas=sum(is.na(value)),
    num_total=length(value)
    ) %>%
  mutate(proportion=num_nas/num_total) %>%
  filter(proportion ==1)
  
  

afcd_bind_clean <- afcd_bind %>%
  select(-all_of(unique(variable_only_na$name)))

#__________________________________________
# write data frame to folder
# _________________________________________
write.csv(afcd_bind_clean,here::here("data","OutputsFromR","AFCD_merged.csv"),row.names = FALSE)
