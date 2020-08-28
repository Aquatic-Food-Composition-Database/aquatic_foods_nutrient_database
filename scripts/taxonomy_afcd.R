# ===============================================================================
# add taxonomic data new AFCD database
# exports to CSV 
# Check for trends in outliers among major nutrient groups
#
#
# Name: J. Zachary (Zach) Koehn
# Email: zkoehn@gmail.com
# For: Chris Golden - Harvard University, synthesizing aquatic foods database
# Date started: 08/17/2017
# Revised: 04/05/2020
# ===============================================================================


#_____________________________________________________________________________________________
# read data and load libraries directory defaults
# ________________________________________________________________________________________________
library(tidyverse)
library(taxize)
library(taxizedb)
library(data.table)

# filenames
afcd_merged_file <- "AFCD_merged.csv"


# work directory, set yours using wk_dir here!
wk_dir <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Nutrition/Separate/aquatic_foods_nutrient_database"

afcd_dat <- read.csv(
  file.path(wk_dir,"data","OutputsFromR",afcd_merged_file),
  header = TRUE)



Sys.setlocale('LC_ALL','C') #sets language to eliminate multibyte error







#_____________________________________________________________________________________________
# Adds taxonomic information to dataset
# using the package "taxize"
# Tutorial: 
# ________________________________________________________________________________________________
afcd_dat_all_names <- afcd_dat %>%
  select(Scientific.Name) %>%
  distinct() %>%
  mutate(Scientific.Name=trimws(Scientific.Name)) %>%
  filter(
    Scientific.Name!=""
    )

  afcd_scinames <- unique(afcd_dat_all_names$Scientific.Name)
id_tx_ncbi <- taxizedb::name2taxid(afcd_scinames,db="ncbi", out_type="summary")
id_tx_itis <- taxizedb::name2taxid(afcd_scinames,db="itis", out_type="summary")
id_tx_gbif <- taxizedb::name2taxid(afcd_scinames,db="gbif", out_type="summary")
id_tx_wfo <- taxizedb::name2taxid(afcd_scinames,db="wfo", out_type="summary")
id_tx_tpl <- taxizedb::name2taxid(afcd_scinames,db="tpl", out_type="summary")


setdiff(id_tx_tpl$name,id_tx_gbif$name) #gbif has everything in tpl
setdiff(id_tx_wfo$name,id_tx_gbif$name) #gbif has everything in wfo
setdiff(id_tx_ncbi$name,id_tx_gbif$name) #gbif has everything in wfo

class_ncbi <- taxizedb::classification(id_tx_ncbi$id,db="ncbi")
class_itis <- taxizedb::classification(id_tx_itis$id,db="itis")
class_gbif <- taxizedb::classification(id_tx_gbif$id,db="gbif")


widen_taxa <- function(class_list,i) {
  # i <- 1827
  # class_list <- class_ncbi
  id_df <- as.data.frame(class_list[[i]])
  id_wide <- id_df %>% 
    select(name,rank) %>%
    distinct() %>%
    filter(rank %in% c(
      "kingdom","phylum","class","order","family","genus","species"
      )
    ) %>%
    pivot_wider(names_from=rank,values_from=name) %>%
    select(contains("kingdom"),contains("phylum"),contains("class"),contains("order"),contains("family"),contains("genus"),contains("species")) %>%
    mutate(
      taxa_id=names(class_list)[i])  

  return(as.data.frame(id_wide))

}

# the itis database did not have 
## create a function that returns a logical value
isEmpty <- function(x) {
    is.data.frame(x) && sum(dim(x)) == 0L
}
## apply it over the list
empty <- unlist(lapply(class_itis, isEmpty))
empty_itis <- names(empty)[empty==TRUE]
class_itis_remove_bad_ids <- class_itis[!(names(class_itis) %in% empty_itis)]
## remove the empties
list_gbif <- lapply(1:length(class_gbif), function(i) widen_taxa(class_list=class_gbif,i=i))
list_itis <- lapply(1:length(class_itis_remove_bad_ids), function(i) widen_taxa(class_list=class_itis_remove_bad_ids,i=i))
list_ncbi <- lapply(1:length(class_ncbi), function(i) widen_taxa(class_list=class_ncbi,i=i))


taxa_ncbi <- plyr::rbind.fill(list_ncbi)
taxa_itis <- plyr::rbind.fill(list_itis)
taxa_gbif <- plyr::rbind.fill(list_ncbi)

taxa_ncbi$taxa_db <- "ncbi"
taxa_itis$taxa_db <- "itis"
taxa_gbif$taxa_db <- "gbif"



taxa_taxize <- bind_rows(taxa_gbif,taxa_itis,taxa_ncbi)

names(taxa_taxize)

taxa_taxize_unique <- taxa_taxize[!duplicated(taxa_taxize$species),] 

grepl(taxa_taxize_unique$species,"c(")

taxa_taxize_unique$match_name <- "" # Creating an empty column

taxa_taxize_unique[1657:1660,]

for(i in 1:dim(taxa_taxize_unique)[1]) {
   x <- agrep(taxa_taxize_unique$species[i], afcd_scinames,
   ignore.case=TRUE, value=TRUE,
   max.distance = 0.02, useBytes = TRUE)
   x <- paste0(x,"")
   taxa_taxize_unique$match_name[i] <- x
} 

afcd_taxa <- merge(taxa_taxize_unique,afcd_dat,all.y=TRUE,by.x="species",by.y="Scientific.Name")

# ok, so still missing a good bit of information from species that aren't in these databases...
# but, we can extract the genus from the species name and at least fill in the upper classifications
# by matching that genus with another row where we have complete information

# also still missing information from infoods (biodiv3,latinfoods,MOZ, USA, Koriea, New Zealand (basically all the blank Scientific Names at the top)
# this COULD be because it's cooked.... but need to look into it, just subset blank scientific names in AFCD_merged.csv to see them

write.csv(afcd_taxa,
    file.path(wk_dir,"data","OutputsFromR","afcd_final.csv"),
    row.names=FALSE
  )



head(afcd_taxa)


