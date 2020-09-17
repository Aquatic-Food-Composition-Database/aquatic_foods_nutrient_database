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
# Revised: 09/04/2020
# ===============================================================================


#_____________________________________________________________________________________________
# read data and load libraries directory defaults
# ________________________________________________________________________________________________
library(tidyverse)
library(taxize)
library(taxizedb)
library(data.table)
library(pbapply)

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
  mutate(
    Scientific.Name=trimws(Scientific.Name),
    Scientific.Name=str_replace_all(Scientific.Name,"\n",""), #some had hidden line breaks
    Scientific.Name=str_replace_all(Scientific.Name,"Theragra chalcogramma","Gadus chalcogrammus"), #new name (Alaska pollock)
    Scientific.Name=str_replace_all(Scientific.Name,"Cystoseria","Cystoseira"), #misspelled genus
    Scientific.Name=str_to_sentence(Scientific.Name)
    )


afcd_scinames <- unique(afcd_dat_all_names$Scientific.Name)
afcd_scinames <- afcd_scinames[afcd_scinames!="" & is.na(afcd_scinames)==FALSE]

id_tx_ncbi <- taxizedb::name2taxid(afcd_scinames,db="ncbi", out_type="summary")
id_tx_itis <- taxizedb::name2taxid(afcd_scinames,db="itis", out_type="summary")
id_tx_gbif <- taxizedb::name2taxid(afcd_scinames,db="gbif", out_type="summary")

# remove these two because all names in them are contained in the above, see commented script in lines immediately below
# id_tx_wfo <- taxizedb::name2taxid(afcd_scinames,db="wfo", out_type="summary")
# id_tx_tpl <- taxizedb::name2taxid(afcd_scinames,db="tpl", out_type="summary")

# setdiff(id_tx_tpl$name,id_tx_gbif$name) #gbif has everything in tpl
# setdiff(id_tx_wfo$name,id_tx_gbif$name) #gbif has everything in wfo
# setdiff(id_tx_ncbi$name,id_tx_gbif$name) #gbif has everything in wfo

class_ncbi <- taxizedb::classification(id_tx_ncbi$id,db="ncbi")
class_itis <- taxizedb::classification(id_tx_itis$id,db="itis")
class_gbif <- taxizedb::classification(id_tx_gbif$id,db="gbif")


widen_taxa_func <- function(class_list,i) {
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

# the itis database did not have a few id's, which also made parts of the list output having 
# no rows... which made it impossible to work with... so need to remove those bad IDs from itis
## create a function that returns a logical value
identify_empty_rows_func <- function(x) {
    is.data.frame(x) && sum(dim(x)) == 0L
}
## apply it over the list
empty <- unlist(lapply(class_itis, identify_empty_rows_func))
empty_itis <- names(empty)[empty==TRUE]
class_itis_remove_bad_ids <- class_itis[!(names(class_itis) %in% empty_itis)]
## remove the empties
list_gbif <- pblapply(1:length(class_gbif), function(i) widen_taxa_func(class_list=class_gbif,i=i))
list_itis <- pblapply(1:length(class_itis_remove_bad_ids), function(i) widen_taxa_func(class_list=class_itis_remove_bad_ids,i=i))
list_ncbi <- pblapply(1:length(class_ncbi), function(i) widen_taxa_func(class_list=class_ncbi,i=i))


taxa_ncbi <- plyr::rbind.fill(list_ncbi)
taxa_itis <- plyr::rbind.fill(list_itis)
taxa_gbif <- plyr::rbind.fill(list_ncbi)

taxa_ncbi$taxa_db <- "ncbi"
taxa_itis$taxa_db <- "itis"
taxa_gbif$taxa_db <- "gbif"

#must be some bugs in the package... it included a few taxa_id's where species were NA
# this creates some issues in the merge and string manipulation down below, so remove them now
taxa_gbif <- taxa_gbif[is.na(taxa_gbif$species) != TRUE,] 
taxa_itis <- taxa_itis[is.na(taxa_itis$species) != TRUE,] 
taxa_ncbi <- taxa_ncbi[is.na(taxa_ncbi$species) != TRUE,] 

taxa_taxize <- bind_rows(taxa_gbif,taxa_itis,taxa_ncbi) %>%
  drop_na(genus)


# basically want to order the dataset giving priority to NCBI, then ITIS, then GBIF - fortunately these are reverse alphabetical
  # so can use order to do so. 
taxa_taxize <- taxa_taxize[order(taxa_taxize$taxa_db,decreasing = TRUE),]  #had to do this in base... b/c tidyverse isn't good at this.
# now remove any duplicates... with preference to NCBI > ITIS > GBIF
taxa_taxize_unique <- taxa_taxize[!duplicated(taxa_taxize$species),] 


afcd_taxa <- merge(taxa_taxize_unique,afcd_dat,all.y=TRUE,by.x="species",by.y="Scientific.Name")


# ok, so still missing a good bit of information from species that aren't in these databases...
# but, I extracted the genus from the species name and at least fill in the upper classifications
# by matching that genus with another row where we have complete information

afcd_taxa$species <- trimws(afcd_taxa$species,"both") #remove white spaces on some of these variables 
# now fills in any missing genus information from the first word of the species name


afcd_taxa$genus <- sapply(1: dim(afcd_taxa)[1],function(i) 
  if(is.na(afcd_taxa$genus[i])==TRUE) {afcd_taxa$genus[i] <- strsplit(afcd_taxa$species[i]," ")[[1]][1]} else {afcd_taxa$genus[i] <- afcd_taxa$genus[i]}
  )

# here make any obvious modifications to names ahead of running the code below that fills the data 
afcd_taxa <- afcd_taxa %>%
  mutate(
    genus=str_replace(genus,"Scorpena","Scorpaena"),
    genus=str_replace(genus,"tinca","Tinca")
    ) 

# afcd_taxa <- read.csv(
#     file.path(wk_dir,"data","OutputsFromR","AFCD_final.csv"),
#     header=TRUE
#   )

# seq_test <- 420:424
seq_true <- 1:dim(afcd_taxa)[1]


# creates a list of the FILLED taxonomic information, based on genus
list_kingdom_fam <- pblapply(seq_true,function(i) {
  # i=422
  if( is.na(afcd_taxa$species[i])==FALSE ) {
    afcd_taxa[i,c("kingdom","phylum","class","order","family","genus")] <- afcd_taxa[i,c("kingdom","phylum","class","order","family","genus")]
    } 
  if( (is.na(afcd_taxa$genus[i])==FALSE || afcd_taxa$genus=="") && (is.na(afcd_taxa$family[i])==FALSE) ) {
    afcd_taxa[i,c("kingdom","phylum","class","order","family","genus")] <- afcd_taxa[i,c("kingdom","phylum","class","order","family","genus")]
    }
  if( (is.na(afcd_taxa$genus[i])==FALSE) && (is.na(afcd_taxa$family[i])==TRUE) && afcd_taxa$genus[i]!="" ) {
    same_genus <- afcd_taxa[afcd_taxa$genus %like% afcd_taxa$genus[i],c("kingdom","phylum","class","order","family","genus") ]
    fills <- same_genus

    fill_kingdom_fam <- distinct(fills[complete.cases(fills),])
    if(dim(fill_kingdom_fam)[1]>0) {
      afcd_taxa[i,c("kingdom","phylum","class","order","family","genus")] <- fill_kingdom_fam
      } else {
        afcd_taxa[i,c("kingdom","phylum","class","order","family","genus")] <- fills[1,]  
        }
    }
  return(afcd_taxa[i,c("kingdom","phylum","class","order","family","genus")])

  }
)

# before adding to the taxonomic columns, the list must be converted to a dataframe
filled_kingdom_fam <- as.data.frame(rbindlist(list_kingdom_fam))

afcd_taxa[seq_true,c("kingdom","phylum","class","order","family","genus")] <- filled_kingdom_fam

# add information for this aquatic plant
for(i in seq_true) {
  if(afcd_taxa$species[i]=="Enhydra fluctuans") {
  afcd_taxa$kingdom[i]<-"Plantae"
  afcd_taxa$phylum[i]<-"Magniliophyta"
  afcd_taxa$class[i]<-"Magnoliospida"
  afcd_taxa$order[i]<-"Asterales"
  afcd_taxa$family[i]<-"Asteraceae"
  afcd_taxa$genus[i]<-"Enhydra"
  }
}

afcd_taxa <- afcd_taxa %>% 
        arrange(is.na(.),kingdom,phylum,class) %>%
        select(-Class.worms,-Order.worms,-Family.worms,-Genus.worms)
# also still missing information from infoods (biodiv3,latinfoods,MOZ, USA, Koriea, New Zealand (basically all the blank Scientific Names at the top)
# this COULD be because it's cooked.... but need to look into it, just subset blank scientific names in AFCD_merged.csv to see them

write.csv(afcd_taxa,
    file.path(wk_dir,"data","OutputsFromR","aquatic_food_composition_database","AFCD_live.csv"),
    row.names=FALSE
  )

