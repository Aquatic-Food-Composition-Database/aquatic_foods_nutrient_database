
library(tidyverse);library(here);library(readxl)



peer_review_dat_nisha_old <-read_excel(
  here("data","afcd_peer_review_data",
       "afcd peer review updates",
       "[ARCHIVE] afcd_peer_review_data.xlsx"),
  sheet="all peer review data"
  ) 


clean_review_reorganized_expand <- read_csv(
  here("data","afcd_peer_review_data",
       "afcd peer review updates",
       "peer_review_all_r_output.csv")
  ) %>%
  mutate(validation_2023="")

length(sort(unique(peer_review_dat_nisha_old$Study.ID.number)))
length(sort(unique(clean_review_reorganized_expand$Study.ID.number)))

intersect(sort(unique(peer_review_dat_nisha_old$Study.ID.number)),
sort(unique(clean_review_reorganized_expand$Study.ID.number)))
excluded_study_ids <- setdiff(sort(unique(clean_review_reorganized_expand$Study.ID.number)),
        sort(unique(peer_review_dat_nisha_old$Study.ID.number))
        )
length(excluded_study_ids)
clean_review_reorganized_expand_only_excluded <- clean_review_reorganized_expand %>%
  filter(Study.ID.number %in% excluded_study_ids) %>%
  mutate('country_origin_sample-detail'="",other_ingredients="",relative_weight="")

setdiff(clean_review_reorganized_expand_only_excluded$Scientific.Name,peer_review_dat_nisha_old$Scientific.Name)

peer_review_dat_nisha_new <-rbind(peer_review_dat_nisha_old,clean_review_reorganized_expand_only_excluded)
write_csv(
  peer_review_dat_nisha_new,
  here("data","afcd_peer_review_data",
       "afcd peer review updates",
       "peer_review_with_previously_excluded_studies.csv")
)
          

peer_review_dat_lucie <- read_excel(
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
  # add columns from other peer review data
  mutate('country_origin_sample-detail'="",other_ingredients="",relative_weight="",validation_2023="") %>%
  drop_na(Scientific.Name) %>%
  select(-c(sample_location_subnational,Preparation1))

peer_review_master <- rbind(peer_review_dat_nisha_new,peer_review_dat_lucie) %>%
  mutate(
    Preparation=ifelse(is.na(Preparation)==TRUE,Processing,Preparation)
  )




write_csv(peer_review_master,here("data","OutputsFromR","cleaned_fcts","clean_peer_review_master.csv"))
