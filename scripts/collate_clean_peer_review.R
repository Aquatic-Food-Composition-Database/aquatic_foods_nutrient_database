
library(tidyverse);library(here);library(readxl)


peer_review_dat_nisha <-read_excel(
  here("data","afcd_peer_review_data",
       "afcd peer review updates",
       "afcd_peer_review_data.xlsx"),
  sheet="all peer review data"
  ) %>%
  select(-validation_2023)


peer_review_dat_lucie <-read_excel(
  here("data","afcd_peer_review_data",
       "afcd peer review updates",
       "Pacific additions",
       "lucie_afcd_peer_review_data_27Mar2023.xlsx"),
  sheet="all peer review data"
) %>%
  filter(Study.ID.number>=1570) %>%
  # need to moving these terms around
  mutate(
    Preparation1=Processing, #move processing data to a stored prep column
    Processing=Preparation, #store old prep data in processing (sample processing)
    Preparation=Preparation1 #now move stored prep data to the correct 
  ) %>%
  drop_na(Scientific.Name) %>%
  select(-c(sample_location_subnational,Preparation1))

peer_review_master <- rbind(peer_review_dat_nisha,peer_review_dat_lucie) %>%
  mutate(
    Preparation=ifelse(is.na(Preparation)==TRUE,Processing,Preparation)
  )



peer_review_master %>%
  filter(is.na(Preparation)) %>%
  select(1:30) %>%
  View
  
  # group_by(Preparation) %>%
  # count() %>%
  View



write_csv(peer_review_master,here("data","OutputsFromR","cleaned_fcts","clean_peer_review_master.csv"))





