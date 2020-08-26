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
library(plyr);library(dplyr);library(here)

afcd_merged_file <- "AFCD_merged.csv"
worms_2017_file <- "WoRMS.6.2017.csv"


wk_dir <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Nutrition/Separate/aquatic_foods_nutrient_database"

afcd_dat <- read.csv(
  file.path(wk_dir,"data","OutputsFromR",afcd_merged_file),
  header = TRUE)

# taxonomic database from the World Register of Marine Species or (WoRMS). It's a bigger file. 
worms <- read.csv(
  file.path(wk_dir,"data",worms_2017_file),
  header=T,stringsAsFactors = F)


Sys.setlocale('LC_ALL','C') #sets language to eliminate multibyte error







#_____________________________________________________________________________________________
# Adds taxonomic information to dataset
# : uses fuzzy merge approach on scientific name to attach associated class, order,family,AFCD 
# ________________________________________________________________________________________________


#combines typically used scientific names from national databases, fishbase, FAO, and preferred (ie RAM Legacy)
worms$potential.scinames <- paste(worms$scientificName,worms$acceptedNameUsage ,sep=", ")

# take a subset to make this whole thing faster... probably don't need:
# Platyhelmintes (flatworms)'
# Annelida (segmented worms)
# Porifera (sponges)
# Nematoda (round worms)
# Bryozoa 
# Nemertea (ribbon worms)
# Cnidaria (jellyfish, anemones)
phylum.to.exclude <- c("Platyhelmintes","Annelida","Porifera",
                       "Nematoda","Bryozoa","Nemertea","Cnidaria")
worms <- worms[ !(worms$phylum %in% phylum.to.exclude),] 


afcd_dat_all_names <- afcd_dat %>%
  select(Scientific.Name) %>%
  distinct()
afcd_dat_all_names$Scientific.Name <- trimws(afcd_dat_all_names$Scientific.Name,which=c("both")) %>% distinct()
afcd_dat_all_names <- as.data.frame(apply(afcd_dat_all_names, 2, function(x) gsub("^$|^ $", NA, x)))

afcd_dat_all_names$match.names <- "" #create blank column to be filled
afcd_dat_all_names <- as.data.frame(afcd_dat_all_names)
afcd_dat_all_names$Scientific.Name <- as.character(afcd_dat_all_names$Scientific.Name)

worms <- as.data.frame(worms,stringsAsFactors=F)

#below is a messy merge for each row based on how closely the character string
# matches a string in the potential scientific names dataset
#if it matches, it is added to the matched name vector

# fuzzy merge to add multiple science names where available
#!!! Commented out b/c takes a LONG time to run, the imported CSV immediately below is the output
# includes a counter so progress can be marked.

for(i in 1:dim(afcd_dat_all_names)[1]) {
  x <- agrep(afcd_dat_all_names$Scientific.Name[i], worms$potential.scinames,
             ignore.case=TRUE, value=TRUE,
             max.distance = 0.05, useBytes = TRUE)
  x <- paste0(x,"")
  afcd_dat_all_names$match.names[i] <- x
  Sys.sleep(.1)      # some loop operations to create a progress bar (takes a LONG time)
  cat(i, paste0(dim(afcd_dat_all_names)[1]),"\r")
  flush.console()
}

write.csv(afcd_dat_all_names,
  here("OutputsFromR","matched.worms.AFCD.csv"),
  row.names=FALSE)


# ok, here's the shortcut, I commented out  the fuzzy loop because it took ages. 
# I saved the output as a csv and commented out the actual fuzzy merge and just load that output below.
afcd_dat_all_names <- read.csv(
  file.path(wk_dir,"data","OutputsFromR","matched.worms.genus.csv"),
  header=T,stringsAsFactors = F)

worms_taxonomy <- worms[,which(names(worms) %in% c("potential.scinames","class","order","family","genus"))]

names(worms_taxonomy) <- c("Class_w","Order_w",
                           "Family_w","Genus_w","alt.scinames")

names_worms_phylo <- merge(worms_taxonomy,afcd_dat_all_names,by="alt.scinames")
names_worms_phylo <- names_worms_phylo[!duplicated(names_worms_phylo),]

afcd_worms <- merge(names_worms_phylo,afcd_dat, by="Scientific.Name",all.y = T)





# now add in any missing Order, Family names in WoRMS using AFCD existing data (from FISHBASE)
for(i in 1:dim(afcd_worms)[1]) {
  if(is.na(afcd_worms$Order_w[i])==T) {
    afcd_worms$Order_w[i] <- afcd_worms$Order.worms[i]
  }
  if(is.na(afcd_worms$Family_w[i])==T) {
    afcd_worms$Family_w[i] <- afcd_worms$Family.worms[i]
  }
  if(is.na(afcd_worms$Class_w[i])==T) {
    afcd_worms$Class_w[i] <- afcd_worms$Class.worms[i]
  }
  if(is.na(afcd_worms$Genus_w[i])==T) {
    afcd_worms$Genus_w[i] <- afcd_worms$Genus.worms[i]
  }
}

afcd_worms$Class_w

# write finalized dataset to CSV
write.csv(
  afcd_worms,
  here("data","OutputsFromR","AFCD_final.csv"),
  row.names = F) 
