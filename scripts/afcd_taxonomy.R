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
library(readxl)

# filenames
afcd_merged_file <- "AFCD_merged.csv"
asfis_file <- "ASFIS_sp_2020.xlsx"

# work directory, set yours using directory here!
directory <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Nutrition/Separate/aquatic_foods_nutrient_database"

afcd_dat <- read.csv(
  file.path(directory,"data","OutputsFromR",afcd_merged_file),
  header = TRUE)

asfis_spec_info <- read_excel(
  file.path(directory,"data","ASFIS_sp",asfis_file),
  sheet = "ASFIS_All_for_publishing"
  )


Sys.setlocale('LC_ALL','C') #sets language to eliminate multibyte error


#_____________________________________________________________________________________________
# Adds taxonomic information to dataset
# using the package "taxize"
# ________________________________________________________________________________________________
afcd_taxa <- afcd_dat %>%
  select(Scientific.Name) %>%
  distinct() %>%
  mutate(
    Scientific.Name=trimws(Scientific.Name),
    Scientific.Name=str_replace_all(Scientific.Name,"\n",""), #some had hidden line breaks
    Scientific.Name=str_replace_all(Scientific.Name,"Theragra chalcogramma","Gadus chalcogrammus"), #new name (Alaska pollock)
    Scientific.Name=str_replace_all(Scientific.Name,"Cystoseria","Cystoseira"), #misspelled genus
    Scientific.Name=str_to_sentence(Scientific.Name)
    )


afcd_scinames <- unique(afcd_taxa$Scientific.Name)
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

## remove the empties, this will create some list-col warnings, but that will be addressed below
list_gbif <- pblapply(1:length(class_gbif), function(i) widen_taxa_func(class_list=class_gbif,i=i))
list_itis <- pblapply(1:length(class_itis_remove_bad_ids), function(i) widen_taxa_func(class_list=class_itis_remove_bad_ids,i=i))
list_ncbi <- pblapply(1:length(class_ncbi), function(i) widen_taxa_func(class_list=class_ncbi,i=i))

# bind the lists
taxa_ncbi <- plyr::rbind.fill(list_ncbi)
taxa_itis <- plyr::rbind.fill(list_itis)
taxa_gbif <- plyr::rbind.fill(list_ncbi)
# add database names so we know where they came from
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


# merges thee xtracted taxonomic information in with afcd (placing taxonomic info first )
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
#     file.path(directory,"data","OutputsFromR","AFCD_final.csv"),
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
        # select(-Class.worms,-Order.worms,-Family.worms,-Genus.worms) %>%
        mutate(# first make all names lowercase (less character matching required) and remove all the weird spaces and classifications (e.g., "families:__") used in a few of these
          taxa_name = enc2utf8(species), #fix multibyte encoding issue by encoding all to UTF8
          taxa_name = tolower(taxa_name), #some of these are genus, family, or higher taxonomic class... so keeping this more general!
          genus = tolower(genus),
          family = tolower(family),
          order = tolower(order),
          class = tolower(class),
          phylum = tolower(phylum),
          kingdom = tolower(kingdom),
          taxa_name=str_replace_all(taxa_name,"  ",""),
          taxa_name=str_squish(taxa_name),
          taxa_name=str_replace_all(taxa_name,"families:",""),
          taxa_name=str_replace_all(taxa_name,"family:",""),
          genus=str_replace_all(genus,"families:",""),
          genus=str_replace_all(genus,"family:",""),    
          genus = str_replace_all(genus,"<U+FFFD>spp.","")
          )


# also still missing information from infoods (biodiv3,latinfoods,MOZ, USA, Koriea, New Zealand (basically all the blank Scientific Names at the top)
# this COULD be because it's cooked.... but need to look into it, just subset blank scientific names in AFCD_merged.csv to see them



#__________________________________________
# clean up taxonomic names in the database, and fill where needed
# _________________________________________
length_taxa_name <- sapply(strsplit(afcd_taxa$taxa_name, " "), length)
ending_in_ae <- str_detect(afcd_taxa$taxa_name,"ae$")
ending_in_ae <- ifelse(is.na(ending_in_ae)==TRUE,FALSE,ending_in_ae)
afcd_taxa[ending_in_ae==TRUE & length_taxa_name==1,]$family <- afcd_taxa[ending_in_ae==TRUE & length_taxa_name==1,]$genus
afcd_taxa[ending_in_ae==TRUE & length_taxa_name==1,][,c("taxa_name","genus")] <- c(NA,NA)

afcd_taxa[which(afcd_taxa$taxa_name=="bothidae and pleuronectidae"),][c("genus","family","order")] <- c(NA,NA,"pleuronectiformes")
afcd_taxa[which(afcd_taxa$taxa_name=="penaeidae and pandalidae"),][c("genus","family","order")] <- c(NA,NA,"decapoda")
afcd_taxa[which(afcd_taxa$taxa_name=="loligoidae and ommastrephidae"),][c("genus","family","order","phylum")] <- c(NA,NA,NA,"mollusca")
afcd_taxa[which(afcd_taxa$taxa_name=="percichthyidae and centrarchidae"),][c("genus","family","order")] <- c(NA,NA,"perciformes")
afcd_taxa[which(afcd_taxa$taxa_name=="ostreidae family including crassostrea gigas"),][c("genus","family")] <- c(NA,"ostreidae")
afcd_taxa[which(afcd_taxa$taxa_name=="abalistus stellatus"),][c("family","order")] <- c("serranidae","perciformes")
afcd_taxa[which(afcd_taxa$taxa_name=="amblypharongodon mola"),][c("genus","family","order")] <- c("amblypharyngodon","cyprinidae","cypriniformes") #misspelled
afcd_taxa[which(afcd_taxa$taxa_name=="amphiora anceps"),][c("taxa_name","genus","family","order")] <- c("amphiroa anceps","amphiroa","corallinaceae","corallinales") #misspelled
afcd_taxa[which(afcd_taxa$taxa_name=="amphora spp"),][c("genus","family","order")] <- c("amphiroa","corallinaceae","corallinales") #misspelled ... b/c i am assuming this is NOT a diatom...

afcd_taxa[which(afcd_taxa$taxa_name=="gastropoda"),][c("taxa_name","genus","family","order","class","phylum")] <- c("gastropoda",NA,NA,NA,"gastropoda","mollusca")
afcd_taxa[which(afcd_taxa$taxa_name=="bivalvia"),][c("taxa_name","genus","family","order","class","phylum")] <- c("bivalvia",NA,NA,NA,"bivalvia","mollusca")
afcd_taxa[which(afcd_taxa$taxa_name=="anabus testudineus"),][c("taxa_name","genus","family","order")] <- c("anabas testudineus","anabas","anabantidae","anabantiformes")
afcd_taxa[which(afcd_taxa$taxa_name=="anadara<U+FFFD>spp."),][c("taxa_name","genus","family","order","class")] <- c("anadara spp.","anadara","arcidae","arcida","bivalvia")
afcd_taxa[which(afcd_taxa$taxa_name=="anadora semillis"),][c("taxa_name","genus","family","order","class")] <- c("anadara senilis","anadara","arcidae","arcida","bivalvia")
afcd_taxa[which(afcd_taxa$taxa_name=="anchenoglanis occidentalis"),][c("taxa_name","genus","family","order")] <- c("auchenoglanis occidentalis","auchenoglanis","claroteidae","siluriformes")
afcd_taxa[which(afcd_taxa$taxa_name=="angelichthys isabelita"),][c("taxa_name","genus","family","order")] <- c("holacanthus bermudensis","holacanthus","pomacanthidae","perciformes") #old name according to worms
afcd_taxa[which(afcd_taxa$taxa_name=="anguila bicolour"),][c("taxa_name","genus","family","order")] <- c("anguilla bicolor","anguilla","anguillidae","anguilliformes") #mispelled

afcd_taxa[which(afcd_taxa$taxa_name=="apharus rutilanus"),][c("taxa_name")] <- c("aphareus rutilans") #mispelled
afcd_taxa[which(afcd_taxa$taxa_name=="aphareus rutilans"),][c("genus")] <- c("aphareus")
afcd_taxa[which(afcd_taxa$taxa_name=="aphareus rutilans"),][c("family")] <- c("lutjanidae") #mispelled
afcd_taxa[which(afcd_taxa$taxa_name=="aphareus rutilans"),][c("order")] <- c("perciformes") #mispelled


afcd_taxa[which(afcd_taxa$taxa_name=="apoleichthus taprobanensis"),][c("genus")] <- c("apolemichthys") # no species, genus mispelled
afcd_taxa[which(afcd_taxa$taxa_name=="apoleichthus taprobanensis"),][c("family")] <- c("pomacanthidae") #mispelled
afcd_taxa[which(afcd_taxa$taxa_name=="apoleichthus taprobanensis"),][c("order")] <- c("perciformes") #mispelled

afcd_taxa[which(afcd_taxa$taxa_name=="arapaimagigas"),][c("taxa_name","genus","family","order")] <- c("arapaima gigas","arapaima","arapaimidae","osteoglossiformes") #mispelled

afcd_taxa[which(afcd_taxa$taxa_name=="archosogus unimaculatus"),][c("taxa_name","genus","family","order")] <- c("archosargus rhomboidalis","archosargus","sparidae","perciformes") #mispelled

afcd_taxa[which(afcd_taxa$taxa_name=="areliscus joyneri"),][c("taxa_name")] <- c("cynoglossus joyneri") #no longer the name
afcd_taxa[which(afcd_taxa$taxa_name=="cynoglossus joyneri"),][c("genus")] <- c("cynoglossus") # no species, genus mispelled
afcd_taxa[which(afcd_taxa$taxa_name=="cynoglossus joyneri"),][c("family")] <- c("cynoglossidae") #mispelled
afcd_taxa[which(afcd_taxa$taxa_name=="cynoglossus joyneri"),][c("order")] <- c("pleuronectiformes") #mispelled

afcd_taxa[which(afcd_taxa$taxa_name=="argyreiosus brevoorti"),][c("taxa_name","genus","family","order")] <- c("selene brevoortii","selene","carangidae","carangiformes") #mispelled

afcd_taxa[which(afcd_taxa$taxa_name=="aristichthys nobili"),][c("taxa_name")] <- c("hypophthalmichthys nobilis") #mispelled
afcd_taxa[which(afcd_taxa$taxa_name=="hypophthalmichthys nobilis"),][c("genus")] <- c("hypophthalmichthys")
afcd_taxa[which(afcd_taxa$taxa_name=="hypophthalmichthys nobilis"),][c("family")] <- c("cyprinidae")
afcd_taxa[which(afcd_taxa$taxa_name=="hypophthalmichthys nobilis"),][c("order")] <- c("cypriniformes")

afcd_taxa[which(afcd_taxa$taxa_name=="aristichthys nobilis"),][c("taxa_name")] <- c("hypophthalmichthys nobilis") #mispelled
afcd_taxa[which(afcd_taxa$taxa_name=="hypophthalmichthys nobilis"),][c("genus")] <- c("hypophthalmichthys")
afcd_taxa[which(afcd_taxa$taxa_name=="hypophthalmichthys nobilis"),][c("family")] <- c("cyprinidae")
afcd_taxa[which(afcd_taxa$taxa_name=="hypophthalmichthys nobilis"),][c("order")] <- c("cypriniformes")

afcd_taxa[which(afcd_taxa$taxa_name=="aspitrigla cuculus"),][c("taxa_name")] <- c("chelidonichthys cuculus")
afcd_taxa[which(afcd_taxa$taxa_name=="chelidonichthys cuculus"),][c("genus")] <- c("chelidonichthys")
afcd_taxa[which(afcd_taxa$taxa_name=="chelidonichthys cuculus"),][c("family")] <- c("triglidae")
afcd_taxa[which(afcd_taxa$taxa_name=="chelidonichthys cuculus"),][c("order")] <- c("scorpaeniformes")


afcd_taxa[which(afcd_taxa$taxa_name=="astacus, orconectes, and procambarus spp."),][c("genus")] <- c(NA) #mispelled
afcd_taxa[which(afcd_taxa$taxa_name=="astacus, orconectes, and procambarus spp."),][c("family")] <- c(NA) #mispelled
afcd_taxa[which(afcd_taxa$taxa_name=="astacus, orconectes, and procambarus spp."),][c("order")] <- c("decapoda") #mispelled
afcd_taxa[which(afcd_taxa$taxa_name=="astacus, orconectes, and procambarus spp."),][c("class")] <- c("malacostraca") #mispelled

afcd_taxa[which(afcd_taxa$taxa_name=="asthenosoma ijimai"),][c("genus","family","order","class","phylum")] <- c("asthenosoma","echinothuriidae","echinothurioida","echinoidea","echinodermata") #mispelled

afcd_taxa[which(afcd_taxa$taxa_name=="astroconger myriaster"),][c("taxa_name")] <- c("conger myriaster")
afcd_taxa[which(afcd_taxa$taxa_name=="atlantic mackerel"),][c("taxa_name","genus","family","order")] <- c("scomber scombrus","scomber","scombridae","scombriformes")


afcd_taxa[which(afcd_taxa$taxa_name=="conger myriaster"),][c("genus")] <- c("conger")
afcd_taxa[which(afcd_taxa$taxa_name=="conger myriaster"),][c("family")] <- c("congridae")
afcd_taxa[which(afcd_taxa$taxa_name=="conger myriaster"),][c("order")] <- c("anguilliformes")

afcd_taxa[which(afcd_taxa$taxa_name=="atherinidas"),][c("family")] <- c("atherinidae")
afcd_taxa[which(afcd_taxa$taxa_name=="atherinidas"),][c("order")] <- c("atheriniformes")
afcd_taxa[which(afcd_taxa$taxa_name=="atherinidas"),][c("order")] <- c("actinopterygii")

afcd_taxa[which(afcd_taxa$taxa_name=="atlantic mackerel"),][c("taxa_name")] <- c("scomber scombrus")
afcd_taxa[which(afcd_taxa$taxa_name=="scomber scombrus"),][c("genus")] <- c("scomber")


afcd_taxa[which(afcd_taxa$taxa_name=="aulacomya ater"),][c("taxa_name")] <- c("aulacomya atra")
afcd_taxa[which(afcd_taxa$taxa_name=="aulacomya atra"),][c("genus")] <- c("aulacomya")
afcd_taxa[which(afcd_taxa$taxa_name=="aulacomya atra"),][c("family")] <- c("mytilidae")
afcd_taxa[which(afcd_taxa$taxa_name=="aulacomya atra"),][c("order")] <- c("mytilida")

afcd_taxa[which(afcd_taxa$taxa_name=="austrophycis marginata"),][c("taxa_name")] <- c("notophycis marginata")
afcd_taxa[which(afcd_taxa$taxa_name=="notophycis marginata"),][c("genus")] <- c("notophycis")
afcd_taxa[which(afcd_taxa$taxa_name=="notophycis marginata"),][c("family")] <- c("moridae")
afcd_taxa[which(afcd_taxa$taxa_name=="notophycis marginata"),][c("order")] <- c("gadiformes")

afcd_taxa[which(afcd_taxa$taxa_name=="avocettina sp"),][c("genus")] <- c("avocettina")
afcd_taxa[which(afcd_taxa$taxa_name=="avocettina sp"),][c("family")] <- c("nemichthydae")
afcd_taxa[which(afcd_taxa$taxa_name=="avocettina sp"),][c("order")] <- c("anguilliformes")

afcd_taxa[which(afcd_taxa$taxa_name=="bagre pinimmaculatus"),][c("family","order")] <- c("ariidae","siluriformes")

afcd_taxa[which(afcd_taxa$taxa_name=="bahaschia argus"),][c("taxa_name","genus","family","order","class","phylum")] <- c("bohadschia argus","bohadschia","holothuriidae","holothuriida","holothuroidea","echinodermata")

afcd_taxa[which(afcd_taxa$taxa_name=="barbonemus gonionotus"),][c("taxa_name")] <- c("barbonymus gonionotus")
afcd_taxa[which(afcd_taxa$taxa_name=="barbonymus gonionotus"),][c("genus")] <- c("barbonymus")
afcd_taxa[which(afcd_taxa$taxa_name=="barbonymus gonionotus"),][c("family")] <- c("cyprinidae")
afcd_taxa[which(afcd_taxa$taxa_name=="barbonymus gonionotus"),][c("order")] <- c("cypriniformes")

afcd_taxa[which(afcd_taxa$taxa_name=="bassanago albescens"),][c("genus")] <- c("barbonymus gonionotus")
afcd_taxa[which(afcd_taxa$taxa_name=="bassanago albescens"),][c("family")] <- c("cyprinidae")
afcd_taxa[which(afcd_taxa$taxa_name=="bassanago albescens"),][c("order")] <- c("cypriniformes")

afcd_taxa[which(afcd_taxa$taxa_name=="batillus cornutus"),][c("family")] <- c("turbinidae")
afcd_taxa[which(afcd_taxa$taxa_name=="batillus cornutus"),][c("order")] <- c("trochida")
afcd_taxa[which(afcd_taxa$taxa_name=="batillus cornutus"),][c("class")] <- c("gastropoda")

afcd_taxa[which(afcd_taxa$taxa_name=="batissa violocea"),][c("family")] <- "cyrenidae"
afcd_taxa[which(afcd_taxa$taxa_name=="batissa violocea"),][c("order")] <- "venerida"
afcd_taxa[which(afcd_taxa$taxa_name=="batissa violocea"),][c("class")] <- "bivalvia"

afcd_taxa[which(afcd_taxa$taxa_name=="bero elegans"),][c("family")] <- "cottidae"
afcd_taxa[which(afcd_taxa$taxa_name=="bero elegans"),][c("order")] <- "scorpaeniformes"
afcd_taxa[which(afcd_taxa$taxa_name=="bero elegans"),][c("class")] <- "actinopterygii"

afcd_taxa[which(afcd_taxa$taxa_name=="bero elegans"),][c("family")] <- "cottidae"
afcd_taxa[which(afcd_taxa$taxa_name=="bero elegans"),][c("order")] <- "scorpaeniformes"
afcd_taxa[which(afcd_taxa$taxa_name=="bero elegans"),][c("class")] <- "actinopterygii"

afcd_taxa[which(afcd_taxa$taxa_name=="borbonymus schwanenfeldii"),][c("taxa_name","genus","family","order")] <- c("barbonymus schwanenfeldii","barbonymus","cyprinidae","cypriniformes")

afcd_taxa[which(afcd_taxa$taxa_name=="bothus podus"),][c("taxa_name","genus","family","order")] <- c("bothus podas","bothus","bothidae","pleuronectiformes")

afcd_taxa[which(afcd_taxa$taxa_name=="botrycladia leptopoda"),][c("taxa_name")] <- c("botryocladia leptopoda")
afcd_taxa[which(afcd_taxa$taxa_name=="botryocladia leptopoda"),][c("genus")] <- c("botryocladia")
afcd_taxa[which(afcd_taxa$taxa_name=="botryocladia leptopoda"),][c("family")] <- c("rhodymeniaceae")
afcd_taxa[which(afcd_taxa$taxa_name=="botryocladia leptopoda"),][c("order")] <- c("rhodymeniales")
afcd_taxa[which(afcd_taxa$taxa_name=="botryocladia leptopoda"),][c("class")] <- c("florideophyceae")
afcd_taxa[which(afcd_taxa$taxa_name=="botryocladia leptopoda"),][c("phylum")] <- c("rhodophyta")

afcd_taxa[which(afcd_taxa$taxa_name=="box boops"),][c("genus","family","order")] <- c("boops","sparidae","perciformes")
afcd_taxa[which(afcd_taxa$taxa_name=="brachyplatistoma flavicans"),][c("taxa_name","genus","family","order")] <- c("zungaro zungaro","zungaro","pimelodidae","siluriformes")


afcd_taxa[which(afcd_taxa$taxa_name=="brogniartella byssoides"),][c("family")] <- c("rhodomelaceae")
afcd_taxa[which(afcd_taxa$taxa_name=="brogniartella byssoides"),][c("order")] <- c("ceramiales")
afcd_taxa[which(afcd_taxa$taxa_name=="brogniartella byssoides"),][c("class")] <- c("florideophyceae")
afcd_taxa[which(afcd_taxa$taxa_name=="brogniartella byssoides"),][c("phylum")] <- c("rhodophyta")

afcd_taxa[which(afcd_taxa$taxa_name=="buccinulum cornea"),][c("taxa_name")] <- c("euthria cornea")
afcd_taxa[which(afcd_taxa$taxa_name=="euthria cornea"),][c("genus")] <- c("euthria")
afcd_taxa[which(afcd_taxa$taxa_name=="euthria cornea"),][c("family")] <- c("buccinidae")
afcd_taxa[which(afcd_taxa$taxa_name=="euthria cornea"),][c("order")] <- c("neogastropoda")
afcd_taxa[which(afcd_taxa$taxa_name=="euthria cornea"),][c("class")] <- c("gastropoda")
afcd_taxa[which(afcd_taxa$taxa_name=="euthria cornea"),][c("phylum")] <- c("mollusca")

afcd_taxa[which(afcd_taxa$taxa_name=="butis gymnopomus"),][c("family")] <- c("eleotridae")
afcd_taxa[which(afcd_taxa$taxa_name=="butis gymnopomus"),][c("order")] <- c("perciformes")
afcd_taxa[which(afcd_taxa$taxa_name=="butis gymnopomus"),][c("class")] <- c("actinopterygii")

afcd_taxa[which(afcd_taxa$taxa_name=="calcalburnus mossulensis"),][c("taxa_name")] <- c("chalcalburnus mossulensis")
afcd_taxa[which(afcd_taxa$taxa_name=="chalcalburnus mossulensis"),][c("genus")] <- c("alburnus")
afcd_taxa[which(afcd_taxa$taxa_name=="chalcalburnus mossulensis"),][c("family")] <- c("cyprinidae")
afcd_taxa[which(afcd_taxa$taxa_name=="chalcalburnus mossulensis"),][c("order")] <- c("cypriniformes")

afcd_taxa[which(afcd_taxa$genus=="caldophora sp"),][c("taxa_name")] <- c("cladophora sp")
afcd_taxa[which(afcd_taxa$genus=="cladophora sp"),][c("genus")] <- c("cladophora")
afcd_taxa[which(afcd_taxa$genus=="cladophora sp"),][c("family")] <- c("cladophoraceae")
afcd_taxa[which(afcd_taxa$genus=="cladophora sp"),][c("order")] <- c("cladophorales")
afcd_taxa[which(afcd_taxa$genus=="cladophora sp"),][c("class")] <- c("ulvophyceae")

afcd_taxa[which(afcd_taxa$genus=="calliblepharis spp"),][c("family")] <- c("cystocloniaceae") #from algaebase
afcd_taxa[which(afcd_taxa$genus=="calliblepharis spp"),][c("order")] <- c("gigartinales")
afcd_taxa[which(afcd_taxa$genus=="calliblepharis spp"),][c("class")] <- c("florideophyceae")
afcd_taxa[which(afcd_taxa$genus=="calliblepharis spp"),][c("phylum")] <- c("rhodophyta")

afcd_taxa[which(afcd_taxa$taxa_name=="calliurichthys japonicus"),][c("family")] <- c("callionymidae")
afcd_taxa[which(afcd_taxa$taxa_name=="calliurichthys japonicus"),][c("order")] <- c("perciformes")

afcd_taxa[which(afcd_taxa$taxa_name=="callorhincus callorhincus"),][c("taxa_name")] <- c("callorhinchus callorhinchus") #two different wrong spellings
afcd_taxa[which(afcd_taxa$taxa_name=="callorhynchus callorhynchus"),][c("taxa_name")] <- c("callorhinchus callorhinchus")

afcd_taxa[which(afcd_taxa$taxa_name=="callorhinchus callorhinchus"),][c("genus")] <- c("callorhinchus ")
afcd_taxa[which(afcd_taxa$taxa_name=="callorhinchus callorhinchus"),][c("family")] <- c("callorhinchidae")
afcd_taxa[which(afcd_taxa$taxa_name=="callorhinchus callorhinchus"),][c("order")] <- c("chimaeriformes")
afcd_taxa[which(afcd_taxa$taxa_name=="callorhinchus callorhinchus"),][c("class")] <- c("chondrichthyes")
afcd_taxa[which(afcd_taxa$taxa_name=="callorhinchus callorhinchus"),][c("phylum")] <- c("chordata")

afcd_taxa[which(afcd_taxa$taxa_name=="callorhynchus"),][c("genus")] <- c("callorhinchus ")
afcd_taxa[which(afcd_taxa$taxa_name=="callorhynchus"),][c("family")] <- c("callorhinchidae")
afcd_taxa[which(afcd_taxa$taxa_name=="callorhynchus"),][c("order")] <- c("chimaeriformes")
afcd_taxa[which(afcd_taxa$taxa_name=="callorhynchus"),][c("class")] <- c("chondrichthyes")
afcd_taxa[which(afcd_taxa$taxa_name=="callorhynchus"),][c("phylum")] <- c("chordata")

afcd_taxa[which(afcd_taxa$taxa_name=="cambarus spp."),][c("family")] <- c("cambaridae")
afcd_taxa[which(afcd_taxa$taxa_name=="cambarus spp."),][c("order")] <- c("decapoda")
afcd_taxa[which(afcd_taxa$taxa_name=="cambarus spp."),][c("class")] <- c("malacostraca")
afcd_taxa[which(afcd_taxa$taxa_name=="cambarus spp."),][c("phylum")] <- c("crustacea")


afcd_taxa[which(afcd_taxa$taxa_name=="cantharus lineatus"),][c("genus")] <- c("spondyliosoma cantharus")
afcd_taxa[which(afcd_taxa$taxa_name=="spondyliosoma cantharus"),][c("family")] <- c("sparidae")
afcd_taxa[which(afcd_taxa$taxa_name=="spondyliosoma cantharus"),][c("order")] <- c("perciformes")
afcd_taxa[which(afcd_taxa$taxa_name=="spondyliosoma cantharus"),][c("class")] <- c("actinopterygii")

afcd_taxa[which(afcd_taxa$taxa_name=="caramium boydenoo"),][c("taxa_name")] <- c("ceramium boydenoo")
afcd_taxa[which(afcd_taxa$taxa_name=="ceramium boydenoo"),][c("genus")] <- c("ceramium")
afcd_taxa[which(afcd_taxa$taxa_name=="ceramium boydenoo"),][c("family")] <- c("ceramiaceae")
afcd_taxa[which(afcd_taxa$taxa_name=="ceramium boydenoo"),][c("order")] <- c("ceramiales")
afcd_taxa[which(afcd_taxa$taxa_name=="ceramium boydenoo"),][c("class")] <- c("florideophyceae")
afcd_taxa[which(afcd_taxa$taxa_name=="ceramium boydenoo"),][c("phylum")] <- c("rhodophyta")

afcd_taxa[which(afcd_taxa$taxa_name=="carassium carassium"),][c("genus")] <- c("carassius carassius")
afcd_taxa[which(afcd_taxa$taxa_name=="carassius carassius"),][c("family")] <- c("cyprinidae")
afcd_taxa[which(afcd_taxa$taxa_name=="carassius carassius"),][c("order")] <- c("cypriniformes")

afcd_taxa[which(afcd_taxa$taxa_name=="carasssius carassius"),][c("genus")] <- c("carassius carassius")
afcd_taxa[which(afcd_taxa$taxa_name=="carassius carassius"),][c("family")] <- c("cyprinidae")
afcd_taxa[which(afcd_taxa$taxa_name=="carassius carassius"),][c("order")] <- c("cypriniformes")


afcd_taxa[which(afcd_taxa$taxa_name=="cardium papyraceum"),][c("taxa_name")] <- c("Fulvia laevigata")
afcd_taxa[which(afcd_taxa$taxa_name=="Fulvia laevigata"),][c("genus")] <- c("fulvia")
afcd_taxa[which(afcd_taxa$taxa_name=="Fulvia laevigata"),][c("family")] <- c("cardiidae")
afcd_taxa[which(afcd_taxa$taxa_name=="Fulvia laevigata"),][c("order")] <- c("cardiida")
afcd_taxa[which(afcd_taxa$taxa_name=="Fulvia laevigata"),][c("class")] <- c("bivalvia")


afcd_taxa[which(afcd_taxa$genus=="spirulina"),][c("genus")] <- c("spirulinales")
afcd_taxa[which(afcd_taxa$genus=="spirulina"),][c("family")] <- c("spirulinaceae")
afcd_taxa[which(afcd_taxa$genus=="spirulina"),][c("order")] <- c("spirulinales")
afcd_taxa[which(afcd_taxa$genus=="spirulina"),][c("class")] <- c("cyanophyceae")
afcd_taxa[which(afcd_taxa$genus=="spirulina"),][c("phylum")] <- c("cyanobacteria")

afcd_taxa[which(afcd_taxa$genus=="barbodes"),][c("family")] <- c("cyprinidae")
afcd_taxa[which(afcd_taxa$genus=="barbodes"),][c("order")] <- c("cypriniformes")

afcd_taxa[which(afcd_taxa$genus=="acrosiphonia"),][c("family")] <- c("cyprinidae")
afcd_taxa[which(afcd_taxa$genus=="acrosiphonia"),][c("order")] <- c("cypriniformes")


afcd_taxa[which(afcd_taxa$genus=="carpioides"),][c("family")] <- c("catostomidae")
afcd_taxa[which(afcd_taxa$genus=="carpioides"),][c("order")] <- c("cypriniformes")

afcd_taxa[which(afcd_taxa$genus=="curimata"),][c("family")] <- c("curimatidae")
afcd_taxa[which(afcd_taxa$genus=="curimata"),][c("order")] <- c("characiformes")

afcd_taxa[which(afcd_taxa$genus=="eleginus"),][c("family")] <- c("gadidae")
afcd_taxa[which(afcd_taxa$genus=="curimata"),][c("order")] <- c("characiformes")
afcd_taxa[which(afcd_taxa$genus=="helostoma"),][c("family")] <- c("helostomatidae")
afcd_taxa[which(afcd_taxa$genus=="helostoma"),][c("order")] <- c("anabantiformes")
afcd_taxa[which(afcd_taxa$genus=="porphyra"),][c("family")] <- c("bangiaceae")
afcd_taxa[which(afcd_taxa$genus=="porphyra"),][c("order")] <- c("bangiales")
afcd_taxa[which(afcd_taxa$genus=="porphyra"),][c("class")] <- c("bangiophyceae")
afcd_taxa[which(afcd_taxa$genus=="porphyra"),][c("phylum")] <- c("rhodophyta")

afcd_taxa[which(afcd_taxa$genus=="acanthopagus"),][c("genus")] <- c("acanthopagrus")
afcd_taxa[which(afcd_taxa$genus=="acanthopagus"),][c("family")] <- c("sparidae")
afcd_taxa[which(afcd_taxa$genus=="acanthopagus"),][c("order")] <- c("perciformes")

afcd_taxa[which(afcd_taxa$genus=="acrosiphonia"),][c("genus")] <- c("acanthopagrus")
afcd_taxa[which(afcd_taxa$genus=="acrosiphonia"),][c("family")] <- c("ulotrichaceae")
afcd_taxa[which(afcd_taxa$genus=="acrosiphonia"),][c("order")] <- c("ulotrichales")
afcd_taxa[which(afcd_taxa$genus=="acrosiphonia"),][c("class")] <- c("ulvophyceae")
afcd_taxa[which(afcd_taxa$genus=="acrosiphonia"),][c("phylum")] <- c("chlorophyta")


afcd_taxa[which(afcd_taxa$genus=="adenocystis"),][c("family")] <- c("adenocystaceae")
afcd_taxa[which(afcd_taxa$genus=="adenocystis"),][c("order")] <- c("ectocarpales")
afcd_taxa[which(afcd_taxa$genus=="adenocystis"),][c("class")] <- c("phaeophyceae")
afcd_taxa[which(afcd_taxa$genus=="adenocystis"),][c("phylum")] <- c("ochrophyta")


afcd_taxa[which(afcd_taxa$genus=="aegla"),][c("family")] <- c("aeglidae")
afcd_taxa[which(afcd_taxa$genus=="aegla"),][c("order")] <- c("decapoda")
afcd_taxa[which(afcd_taxa$genus=="aegla"),][c("class")] <- c("malacostraca")
afcd_taxa[which(afcd_taxa$genus=="aegla"),][c("phylum")] <- c("arthropoda")

afcd_taxa[which(afcd_taxa$genus=="aglaothamnion"),][c("family")] <- c("callithamniaceae")
afcd_taxa[which(afcd_taxa$genus=="aglaothamnion"),][c("order")] <- c("ceramiales")
afcd_taxa[which(afcd_taxa$genus=="aglaothamnion"),][c("class")] <- c("florideophyceae")
afcd_taxa[which(afcd_taxa$genus=="aglaothamnion"),][c("phylum")] <- c("rhodophyta")

afcd_taxa[which(afcd_taxa$genus=="agrammus"),][c("family")] <- c("hexagrammidae")
afcd_taxa[which(afcd_taxa$genus=="agrammus"),][c("order")] <- c("scorpaeniformes")
afcd_taxa[which(afcd_taxa$genus=="agrammus"),][c("class")] <- c("actinopterygii")

afcd_taxa[which(afcd_taxa$genus=="agrammus"),][c("family")] <- c("ailiidae")
afcd_taxa[which(afcd_taxa$genus=="agrammus"),][c("order")] <- c("siluriformes")


afcd_taxa[which(afcd_taxa$genus=="alcichthys"),][c("family")] <- c("cottidae")
afcd_taxa[which(afcd_taxa$genus=="alcichthys"),][c("order")] <- c("scorpaeniformes")

afcd_taxa[which(afcd_taxa$genus=="allotheutis"),][c("family")] <- c("loliginidae")
afcd_taxa[which(afcd_taxa$genus=="allotheutis"),][c("order")] <- c("myopsida")
afcd_taxa[which(afcd_taxa$genus=="allotheutis"),][c("class")] <- c("cephalopoda")

afcd_taxa[which(afcd_taxa$genus=="allotheutis"),][c("family")] <- c("loliginidae")
afcd_taxa[which(afcd_taxa$genus=="allotheutis"),][c("order")] <- c("myopsida")
afcd_taxa[which(afcd_taxa$genus=="allotheutis"),][c("class")] <- c("cephalopoda")

afcd_taxa[which(afcd_taxa$genus=="allotheutis"),][c("family")] <- c("loliginidae")
afcd_taxa[which(afcd_taxa$genus=="allotheutis"),][c("order")] <- c("myopsida")
afcd_taxa[which(afcd_taxa$genus=="allotheutis"),][c("class")] <- c("cephalopoda")

afcd_taxa[which(afcd_taxa$genus=="anadara"),][c("family")] <- c("arcidae")
afcd_taxa[which(afcd_taxa$genus=="anadara"),][c("order")] <- c("arcida")
afcd_taxa[which(afcd_taxa$genus=="anadara"),][c("class")] <- c("bivalvia")

afcd_taxa[which(afcd_taxa$genus=="anadyomene"),][c("family")] <- c("anadyomenaceae")
afcd_taxa[which(afcd_taxa$genus=="anadyomene"),][c("order")] <- c("cladophorales")
afcd_taxa[which(afcd_taxa$genus=="anadyomene"),][c("class")] <- c("ulvophyceae")

afcd_taxa[which(afcd_taxa$genus=="ankistrodesmus"),][c("family")] <- c("selenastraceae")
afcd_taxa[which(afcd_taxa$genus=="ankistrodesmus"),][c("order")] <- c("sphaeropleales")
afcd_taxa[which(afcd_taxa$genus=="ankistrodesmus"),][c("class")] <- c("chlorophyceae")
afcd_taxa[which(afcd_taxa$genus=="ankistrodesmus"),][c("phylum")] <- c("chlorophyta")

afcd_taxa[which(afcd_taxa$genus=="aorichthys"),][c("family")] <- c("bagridae")
afcd_taxa[which(afcd_taxa$genus=="aorichthys"),][c("order")] <- c("siluriformes")

afcd_taxa[which(afcd_taxa$family=="ambassidae"),][c("order")] <- c("perciformes")
afcd_taxa[which(afcd_taxa$family=="callanthiidae"),][,c("order")] <- c("perciformes")
afcd_taxa[which(afcd_taxa$family=="centropomidae"),][,c("order")] <- c("perciformes")
afcd_taxa[which(afcd_taxa$family=="embiotocidae"),][,c("order")] <- c("perciformes")
afcd_taxa[which(afcd_taxa$family=="lactariidae"),][,c("order")] <- c("perciformes")
afcd_taxa[which(afcd_taxa$family=="malacanthidae"),][,c("order")] <- c("perciformes")
afcd_taxa[which(afcd_taxa$family=="menidae"),][,c("order")] <- c("perciformes")
afcd_taxa[which(afcd_taxa$family=="moronidae"),][,c("order")] <- c("perciformes")
afcd_taxa[which(afcd_taxa$family=="pachychilidae"),][c("class")] <- c("gastropoda")
afcd_taxa[which(afcd_taxa$family=="pachychilidae"),][c("phylum")] <- c("mollusca")
afcd_taxa[which(afcd_taxa$family=="planorbidae"),][c("class")] <- c("gastropoda")
afcd_taxa[which(afcd_taxa$family=="planorbidae"),][c("phylum")] <- c("mollusca")

afcd_taxa[which(afcd_taxa$family=="platyrhinidae"),][,c("order")] <- c("myliobatiformes")
afcd_taxa[which(afcd_taxa$family=="platyrhinidae"),][,c("phylum")] <- c("chondrichthyes")
afcd_taxa[which(afcd_taxa$family=="platyrhinidae"),][,c("class")] <- c("chordata")

afcd_taxa[which(afcd_taxa$family=="pomacanthidae"),][,c("order")] <- "perciformes"
afcd_taxa[which(afcd_taxa$family=="potamididae"),][c("class")] <- c("gastropoda")
afcd_taxa[which(afcd_taxa$family=="scatophagidae"),][,c("order")] <- "perciformes"
afcd_taxa[which(afcd_taxa$family=="semisulcospiridae"),][,c("order")] <- c("sorbeoconcha")
afcd_taxa[which(afcd_taxa$family=="semisulcospiridae"),][,c("class")] <- c("gastropoda")
afcd_taxa[which(afcd_taxa$family=="semisulcospiridae"),][,c("phylum")] <- c("mollusca")
afcd_taxa[which(afcd_taxa$family=="sillaginidae"),][,c("order")] <- c("perciformes")
afcd_taxa[which(afcd_taxa$family=="sphyraenidae"),][,c("order")] <- c("perciformes")


afcd_taxa[which(afcd_taxa$class=="actinopteri"),][,c("class")] <- "actinopterygii"
afcd_taxa[which(afcd_taxa$class=="teleostei"),][,c("class")] <- "actinopterygii"

 

afcd_taxa[which(afcd_taxa$class=="phaeophyceae"),][,c("phylum")] <- c("ochrophyta")

afcd_taxa[which(afcd_taxa$order=="pristiformes/rhiniformes group"),][,c("order")] <- "rhinopristiformes"


afcd_taxa[which(afcd_taxa$family=="platyrhinidae"),][,c("order")] <- c("myliobatiformes")
afcd_taxa[which(afcd_taxa$family=="platyrhinidae"),][,c("phylum")] <- c("chondrichthyes")
afcd_taxa[which(afcd_taxa$family=="platyrhinidae"),][,c("class")] <- c("chordata")

# now fill any NAs in our taxonomic data
afcd_taxa <- setDT(as.data.frame.matrix(afcd_taxa,stringsAsFactors = FALSE))

# afcd_taxa[ending_in_ae!=TRUE & length_taxa_name>1,] <- afcd_taxa[ending_in_ae!=TRUE & length_taxa_name>1,][, family:= family[!is.na(family)][1L] , by = genus] #fill family by genus (EXCEPT where the lowest classification is family)

afcd_taxa[ending_in_ae!=TRUE & length_taxa_name>1,] <- afcd_taxa[ending_in_ae!=TRUE & length_taxa_name>1,][, family:= family[!is.na(family)][1L] , by = genus]
# afcd_taxa[, family:= family[!is.na(family)][1L] , by = genus]
afcd_taxa[ending_in_ae!=TRUE & length_taxa_name>1,] <- afcd_taxa[ending_in_ae!=TRUE & length_taxa_name>1,][, order:= order[!is.na(order)][1L] , by = family] #fill order by family
afcd_taxa[ending_in_ae!=TRUE & length_taxa_name>1,] <- afcd_taxa[ending_in_ae!=TRUE & length_taxa_name>1,][, class:= class[!is.na(class)][1L] , by = order] #fill class by order
afcd_taxa[ending_in_ae!=TRUE & length_taxa_name>1,] <- afcd_taxa[ending_in_ae!=TRUE & length_taxa_name>1,][, phylum:= phylum[!is.na(phylum)][1L] , by = class] #fill phylum by class



afcd_taxa[afcd_taxa$genus=="porphyra",][,c("taxa_name","genus","family","order","class","phylum")] 
# this creates one bug that repeats over itself. There are a few hundred rows with no family information in our database 
# so it incorrectly fills order and phylum from the last known family association. we don't want that



# removes all incorrect order and phylum associations where family is NA (no known associations with information)
afcd_taxa[is.na(afcd_taxa$family)==TRUE,]$order <- NA
afcd_taxa[is.na(afcd_taxa$family)==TRUE,]$class <- NA
afcd_taxa[is.na(afcd_taxa$family)==TRUE,]$phylum <- NA

afcd_taxa[which(afcd_taxa$Study.ID.number=="974" & afcd_taxa$taxa_name=="monodontidae and balaenidae") ,][,c("order")] <- "artiodactyla"
afcd_taxa[which(afcd_taxa$Study.ID.number=="974" & afcd_taxa$taxa_name=="monodontidae and balaenidae") ,][,c("class")] <- "mammalia"

afcd_taxa[which(afcd_taxa$Study.ID.number=="974" & afcd_taxa$taxa_name=="phosidae and odobenidae") ,][,c("order")] <- "carnivora"
afcd_taxa[which(afcd_taxa$Study.ID.number=="974" & afcd_taxa$taxa_name=="phosidae and odobenidae") ,][,c("class")] <- "mammalia"



asfis_subs <- asfis_spec_info %>%
  select(ISSCAAP,Family,Order,Scientific_name) %>%
  distinct()


#__________________________________________
# collect all the cleaned taxa names
# _________________________________________

afcd_taxa_names <- data.frame(
    taxa_name = tolower(afcd_taxa$taxa_name),
    genus = tolower(afcd_taxa$genus),
    family = tolower(afcd_taxa$family),
    order = tolower(afcd_taxa$order),
    class = tolower(afcd_taxa$class),
    phylum = tolower(afcd_taxa$phylum)
    ) %>%
  distinct() %>%
  filter(
    taxa_name != "",
    taxa_name != "etc."
    )

#__________________________________________
# write to folder
# _________________________________________

write.csv(afcd_taxa_names,
    file.path(directory,"data","OutputsFromR","aquatic_food_composition_database","afcd_taxonomic_names_only.csv"),
    row.names=FALSE
  )

write.csv(afcd_taxa,file.path(directory,"data","OutputsFromR","afcd_with_taxa.csv"),row.names = FALSE)

