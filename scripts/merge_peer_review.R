##################
# Cleans data collected from peer review studies for merge with AFCD
# 
# creator: Zach Koehn
# email: zkoehn@stanford.edu
# date started: 07/14/2020
##################


##################
# load needed packages and data sets
##################
library(tidyverse);library(here);library(data.table)

# load nutrient information from Camille's datasets in our drive folder
macro <- read.csv(
  here("data","afcd_peer_review_data","Seafood nutrients","macro nutrients.csv"),
  header=TRUE
) %>%
  dplyr::select(-X,-X.1,-X.2) %>%
  replace(is.na(.),"")

amino <- read.csv(
  here("data","afcd_peer_review_data","Seafood nutrients","amino acids.csv"),
  header=TRUE
) %>%
  dplyr::select(-X) %>%
  replace(is.na(.),"")

fats <- read.csv(
  here("data","afcd_peer_review_data","Seafood nutrients","fatty_acids.csv"),
  header=TRUE
) %>%
  dplyr::select(-X) %>%
  replace(is.na(.),"")

minerals <- read.csv(
  here("data","afcd_peer_review_data","Seafood nutrients","minerals.csv"),
  header=TRUE
) %>%
  dplyr::select(-X,-X.1,-X.2) %>%
  replace(is.na(.), "") 

misc <- read.csv(
  here("data","afcd_peer_review_data","Seafood nutrients","misc.csv"),
  header=TRUE
) %>%
  select(-c(Isohamnetin_est:Quercetin_sd,USDA.ndbno_est:Wax.total_sd)) %>%
  replace(is.na(.),"")

vitamin <- read.csv(
  here("data","afcd_peer_review_data","Seafood nutrients","vitamins.csv"),
  header=TRUE
)  %>%
  replace(is.na(.),"")

  

##################
# before merging, determine the metadata that we can use that to merge the data
##################

name_intersect <- Reduce(
  intersect,
  tibble::lst(
     names(macro),
     names(amino),
     names(fats),
     names(minerals),
     names(misc),
     names(vitamin)
   )
)

macro_ids <- macro %>%
  select(Study.ID.number,Scientific.Name) %>%
  group_by(Study.ID.number,Scientific.Name) %>%
  tally() %>% right_join(macro,by=c("Study.ID.number","Scientific.Name")) %>%
  group_by(Study.ID.number,Scientific.Name) %>%
  mutate(
    sample_id=seq_along(1:n)
  ) %>%
  ungroup() %>%
  select(Study.ID.number,sample_id,everything())

amino_ids <- amino %>%
  select(Study.ID.number,Scientific.Name) %>%
  group_by(Study.ID.number,Scientific.Name) %>%
  tally() %>% right_join(amino,by=c("Study.ID.number","Scientific.Name")) %>%
  group_by(Study.ID.number,Scientific.Name) %>%
  mutate(
    sample_id=seq_along(1:n)
  ) %>%
  ungroup() %>%
  select(Study.ID.number,sample_id,everything())

fats_ids <- fats %>%
  select(Study.ID.number,Scientific.Name) %>%
  group_by(Study.ID.number,Scientific.Name) %>%
  tally() %>% right_join(fats,by=c("Study.ID.number","Scientific.Name")) %>%
  group_by(Study.ID.number,Scientific.Name) %>%
  mutate(
    sample_id=seq_along(1:n)
  ) %>%
  ungroup() %>%
  select(Study.ID.number,sample_id,everything())


minerals_ids <- minerals %>%
  select(Study.ID.number,Scientific.Name) %>%
  group_by(Study.ID.number,Scientific.Name) %>%
  tally() %>% right_join(minerals,by=c("Study.ID.number","Scientific.Name")) %>%
  group_by(Study.ID.number,Scientific.Name) %>%
  mutate(
    sample_id=seq_along(1:n)
  ) %>%
  ungroup() %>%
  select(Study.ID.number,sample_id,everything())

misc_ids <- misc %>%
  select(Study.ID.number,Scientific.Name) %>%
  group_by(Study.ID.number,Scientific.Name) %>%
  tally() %>% right_join(misc,by=c("Study.ID.number","Scientific.Name")) %>%
  group_by(Study.ID.number,Scientific.Name) %>%
  mutate(
    sample_id=seq_along(1:n)
  ) %>%
  ungroup() %>%
  select(Study.ID.number,sample_id,everything())

vitamin_ids <- vitamin %>%
  select(Study.ID.number,Scientific.Name) %>%
  group_by(Study.ID.number,Scientific.Name) %>%
  tally() %>% right_join(vitamin,by=c("Study.ID.number","Scientific.Name")) %>%
  group_by(Study.ID.number,Scientific.Name) %>%
  mutate(
    sample_id=seq_along(1:n)
  ) %>%
  ungroup() %>%
  select(Study.ID.number,sample_id,everything())


master_ids <- data.frame(
  Study.ID.number=c(
    macro_ids$Study.ID.number,amino_ids$Study.ID.number,fats_ids$Study.ID.number,
    minerals_ids$Study.ID.number,misc_ids$Study.ID.number,vitamin_ids$Study.ID.number),
  sample_id=c(
    macro_ids$sample_id,amino_ids$sample_id,fats_ids$sample_id,
    minerals_ids$sample_id,misc_ids$sample_id,vitamin_ids$sample_id)
) %>%
  distinct()



all_list <- list(master_ids,macro_ids,vitamin_ids,minerals_ids,fats_ids,amino_ids,misc_ids)
all_merge <- Reduce(function(x, y) merge(x, y, all.x=TRUE), all_list) %>%
  distinct() %>%
  filter(
    is.na(Scientific.Name)==FALSE,
    Scientific.Name!=""
    )
# extract unit data 
macro_meta <- data.frame(
  variable=names(macro),
  nutrient_units=as.character(macro[1,]),
  category="macro"
) %>%
  filter(!variable  %in% name_intersect)

amino_meta <- data.frame(
  variable=names(amino),
  nutrient_units=as.character(amino[1,]),
  category="amino"
) %>% filter(!variable  %in% name_intersect)

fats_meta <- data.frame(
  variable=names(fats),
  nutrient_units=as.character(fats[1,]),
  category="fats"
) %>%
  filter(!variable  %in% name_intersect)

minerals_meta <- data.frame(
  variable=names(minerals),
  nutrient_units=as.character(minerals[1,]),
  category="minerals"
) %>% filter(!variable  %in% name_intersect)

misc_meta <- data.frame(
  variable=names(misc),
  nutrient_units=as.character(misc[1,]),
  category="misc"
  ) %>% filter(!variable  %in% name_intersect)

vitamin_meta <- data.frame(
  variable=names(vitamin),
  nutrient_units=as.character(vitamin[1,]),
  category="vitamin"
) %>%
  filter(!variable  %in% name_intersect)

obs_meta <- data.frame(
  variable=name_intersect,
  nutrient_units=NA,
  category="study_info"
)

research_overview <- rbind(
  obs_meta,macro_meta,minerals_meta,vitamin_meta, amino_meta,fats_meta,misc_meta
) %>%
  replace(is.na(.), "") %>%
  mutate(
    nutrient_units = ifelse(nutrient_units=="NA","",nutrient_units),
    nutrient_units = str_replace_all(nutrient_units,"\\(",""),
    nutrient_units = str_replace_all(nutrient_units,"\\)",""),
    nutrient_units = str_replace_all(nutrient_units," ",""),
    nutrient_units = str_replace_all(nutrient_units,"Kcal","kcal"),
    nutrient_units = str_replace_all(nutrient_units,"KJ","kj"),
    nutrient_units = str_replace_all(nutrient_units,"mg.100g","mg/100g"),
    variable=str_replace_all(variable,"PAA","PAA."),
    variable=str_replace_all(variable,"PFA","PFA."),
    variable=str_replace_all(variable,"\\.\\.","\\."),
    variable=str_replace_all(variable,"fatty.","Fatty."),
    variable=str_replace_all(variable,"Fatty.acids.","Fatty.acid.")
  ) %>%
  filter(
  !variable %in% c("X.1","X.2","alt.scinames","GBD.Macro","GBD.Sub","FishBase.SAU.Code","ISSCAAP","FAO.Taxon.Code","FAO.3A_CODE","Original.FCT.Food.Code","Season","
         Fatty.acids.total.n3.polyunsaturated_est.1","Fatty.acids.total.n3.polyunsaturated_sd.1","Fatty.acid.15.0.anteiso.1","
         PAAAspartic.acid.1_sd.1","PAAAspartic.acid.1_sd","PFA.Fatty.acid.15.0.iso.1","
         PAA.Glutamine.gluamic.acid_est.1","PAA.Glutamine.gluamic.acid_sd.1","
         PAA.Aspartic.acid.1_est","PAA.Aspartic.acid.1_est.1","PAA.Aspartic.acid.1_sd","
         PFA.Fatty.acids.total.n3.polyunsaturated_est.1","PFA.Fatty.acids.total.n3.polyunsaturated_sd.1")
  ) %>%
  select(category,variable,nutrient_units)
  

##################
# merge datasets together
# of note, the first 26 rows in each of these should be the same across all datasets
##################


clean_review <- all_merge %>%
  rename_with(stringr::str_replace, 
              pattern = "PAA", replacement = "PAA.") %>%
  rename_with(stringr::str_replace, 
              pattern = "PFA", replacement = "PFA.") %>%
  rename_with(stringr::str_replace, 
              pattern = "\\.\\.", replacement = "\\.") %>%
  rename_with(stringr::str_replace, 
              pattern = "fatty.", replacement = "Fatty.") %>%
  rename_with(stringr::str_replace, 
              pattern = "Fatty.acids.", replacement = "Fatty.acid.") %>%
  rename(
    country_origin_study=Country.ISO3,
    sample_month=Season
  ) %>%
  mutate(
    country_origin_sample="",
    sample_year=""
  ) %>%
  ungroup() %>%
  select(
    Study.ID.number:country_origin_study,country_origin_sample,sample_year,sample_month,
    everything()
    # remove duplicates
  )
  
  

# remove from memory
rm(
  amino,fats,macro,minerals,misc,vitamin,
  amino_long,fats_long,macro_long,minerals_long,misc_long,vitamin_long,
  amino_meta,fats_meta,macro_meta,minerals_meta,misc_meta,vitamin_meta,obs_meta,
  all_nutrients,all_nutrients_1,all_nutrients_2,all_nutrients_3,all_nutrients_4
)



relative_value_columns <- clean_review %>%
  select(
    starts_with("PFA."),
    starts_with("PP."),
    starts_with("PAA.")
    )

clean_review_no_relatives <- clean_review %>%
  select(-names(relative_value_columns))



clean_review_reorganized <- 
  cbind(clean_review_no_relatives,relative_value_columns) %>%
  mutate(
    sample_month=ifelse(is.na(sample_month),Season,sample_month),
    Fatty.acid.total.n3.polyunsaturated_est=ifelse(is.na(Fatty.acid.total.n3.polyunsaturated_est)==TRUE,Fatty.acid.total.n3.polyunsaturated_est.1,Fatty.acid.total.n3.polyunsaturated_est),
    Fatty.acid.total.n3.polyunsaturated_sd=ifelse(is.na(Fatty.acid.total.n3.polyunsaturated_sd)==TRUE,Fatty.acid.total.n3.polyunsaturated_sd.1,Fatty.acid.total.n3.polyunsaturated_sd),
    Fatty.acid.15.0.anteiso=Fatty.acid.15.0.anteiso.1,
    PAA.Aspartic.acid_est=ifelse(is.na(PAA.Aspartic.acid_est)==TRUE,PAA.Aspartic.acid.1_est,PAA.Aspartic.acid_est),
    PFA.Fatty.acid.15.0.iso=ifelse(is.na(PFA.Fatty.acid.15.0.iso)==TRUE,PFA.Fatty.acid.15.0.iso.1,PFA.Fatty.acid.15.0.iso),
    PFA.Fatty.acid.total.n3.polyunsaturated_est=ifelse(is.na(PFA.Fatty.acid.total.n3.polyunsaturated_est)==TRUE,PFA.Fatty.acid.total.n3.polyunsaturated_est.1,PFA.Fatty.acid.total.n3.polyunsaturated_est),
    PFA.Fatty.acid.total.n3.polyunsaturated_sd=ifelse(is.na(PFA.Fatty.acid.total.n3.polyunsaturated_sd)==TRUE,PFA.Fatty.acid.total.n3.polyunsaturated_sd.1,PFA.Fatty.acid.total.n3.polyunsaturated_sd),
    Study.ID.number=as.integer(Study.ID.number),
    sample_id=as.integer(sample_id)
    ) %>%
  select(-c(
    n,alt.scinames,GBD.Macro,GBD.Sub,FishBase.SAU.Code,ISSCAAP,FAO.Taxon.Code,FAO.3A_CODE,Original.FCT.Food.Code,
    Fatty.acid.15.0.anteiso.1,
    PFA.Fatty.acid.15.0.iso.1,
    PAA.Aspartic.acid.1_est,PAA.Aspartic.acid.1_sd,
    PFA.Fatty.acid.total.n3.polyunsaturated_est.1,PFA.Fatty.acid.total.n3.polyunsaturated_sd.1)
    ) %>%
  replace(is.na(.), "") %>%
  distinct()

write_csv(
  research_overview,
  here("data","afcd_peer_review_data","afcd peer review updates","dataset_overview.csv"),
)


write_csv(
  clean_review_reorganized,
  here("data","afcd_peer_review_data","afcd peer review updates","peer_review_all_r_output.csv"),
  quote="needed"
  )




