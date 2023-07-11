
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
  select(-sample_location_subnational) %>%
  filter(Study.ID.number>=1570) %>%
  mutate()

peer_review_master <- rbind(peer_review_dat_nisha,peer_review_dat_lucie)


write_csv(peer_review_master,here("data","OutputsFromR","cleaned_fcts","clean_peer_review_master.csv"))





