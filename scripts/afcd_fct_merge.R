#============================================================
# Merge cleaned FCT & peer review information with AFCD

#Name: J. Zachary (Zach) Koehn
#Email: zkoehn@stanford.edu
#Date started: 05/13/2020
#Revised: 08/10/2020
#============================================================

afcd_file <- "AFCD_working.csv"
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
peer_review_file <- "clean_peer_review.csv"
#__________________________________________str
# read data and load libraries directory defaults
# _________________________________________
library(tidyverse);library(dplyr);library(here)


# now use here package to navigate from working directory to specific food file
afcd_dat <- read.csv(
  here("data","OutputsFromR",afcd_file)
  )
aus_dat <- read.csv(
  here("data","OutputsFromR","cleaned_fcts",aus_file)
  )
pndb_dat <- read.csv(
  here("data","OutputsFromR","cleaned_fcts",pndb_file)
  )
fao_wa_dat <- read.csv(
  here("data","OutputsFromR","cleaned_fcts",fao_wa_file)
  )
smiling_cambodia_dat <- read.csv(
  here("data","OutputsFromR","cleaned_fcts",smiling_cambodia_file)
)
smiling_indonesia_dat <- read.csv(
  here("data","OutputsFromR","cleaned_fcts",smiling_indonesia_file)
)
smiling_laos_dat <- read.csv(
  here("data","OutputsFromR","cleaned_fcts",smiling_laos_file)
)
smiling_thailand_dat <- read.csv(
  here("data","OutputsFromR","cleaned_fcts",smiling_thailand_file)
)
smiling_vietnam_dat <- read.csv(
  here("data","OutputsFromR","cleaned_fcts",smiling_vietnam_file)
)
norway_dat <- read.csv(
  here("data","OutputsFromR","cleaned_fcts",norway_file)
)
india_dat <- read.csv(
  here("data","OutputsFromR","cleaned_fcts",india_file)
)
latinfoods_dat <- read.csv(
  here("data","OutputsFromR","cleaned_fcts",latinfoods_file)
)


peer_review_dat <- read.csv(
  here("data","OutputsFromR","cleaned_fcts",peer_review_file)
)

# a few of the FCT food codes do not have preceding character so read in as integers, need to convert to character
smiling_laos_dat$Original.FCT.Food.Code <- as.character(smiling_laos_dat$Original.FCT.Food.Code)
smiling_thailand_dat$Original.FCT.Food.Code <- as.character(smiling_thailand_dat$Original.FCT.Food.Code)
smiling_vietnam_dat$Original.FCT.Food.Code <- as.character(smiling_vietnam_dat$Original.FCT.Food.Code)
norway_dat$Original.FCT.Food.Code <- as.character(norway_dat$Original.FCT.Food.Code)
peer_review_dat$ISSCAAP <- as.integer(peer_review_dat$ISSCAAP)
peer_review_dat$FishBase.SAU.Code <- as.integer(peer_review_dat$FishBase.SAU.Code)
peer_review_dat<- peer_review_dat %>%
  mutate(
    across(Nitrogen.nonprotein:Tannins.total,as.numeric),
    Dry.Matter=as.character(Dry.Matter),
    Study.ID.number=as.character(Study.ID.number)
    )

# binds together all variables
afcd_bind <- bind_rows(afcd_dat,
                       aus_dat,pndb_dat,fao_wa_dat,
                       smiling_cambodia_dat,smiling_indonesia_dat,
                       smiling_laos_dat,smiling_thailand_dat,
                       smiling_vietnam_dat,norway_dat,
                       india_dat,latinfoods_dat,
                       peer_review_dat
                       )


write.csv(afcd_bind,here("data","OutputsFromR","AFCD_final.csv"),row.names = FALSE)
