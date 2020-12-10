# ===============================================================================
# Match freshwater species from Beth to AFCD
#
#
# Name: J. Zachary (Zach) Koehn
# Email: zkoehn@gmail.com
# For: Blue Food Assessment Nutrition Paper
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
library(stringdist)


# filenames
afcd_merged_file <- "AFCD_live.csv"
freshwater_file <- "fw_species_tonnes consumed.csv"
marine_file <- "SAU_species_catch.csv"
# work directory, set yours using wk_dir here!
wk_dir <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Nutrition/Separate/aquatic_foods_nutrient_database"

afcd_dat <- read.csv(
  file.path(wk_dir,"data","OutputsFromR","aquatic_food_composition_database",afcd_merged_file),
  header = TRUE)

fw_dat <- read.csv(
  file.path(wk_dir,"data","gap_assessment",freshwater_file),
  header = TRUE)
mar_dat <- read.csv(
  file.path(wk_dir,"data","gap_assessment",marine_file),
  header = TRUE)

Sys.setlocale('LC_ALL','C') #sets language to eliminate multibyte error






afcd_dat_all_names <- data.frame(
		sci_name = tolower(afcd_dat$species),
		genus = tolower(afcd_dat$genus),
		family = tolower(afcd_dat$family),
		order = tolower(afcd_dat$order),
		class = tolower(afcd_dat$class),
		phylum = tolower(afcd_dat$phylum)
		) %>%
	distinct() %>%
	mutate(
		sci_name=str_replace_all(sci_name,"  ",""),
		sci_name=str_squish(sci_name),
		sci_name=str_replace_all(sci_name,"families:",""),
		sci_name=str_replace_all(sci_name,"family:",""),
		genus=str_replace_all(genus,"families:",""),
		genus=str_replace_all(genus,"family:",""),		
		genus = str_replace_all(genus,"<U+FFFD>spp.","")
		) %>%
	drop_na(sci_name) %>%
	filter(
		sci_name != "",
		sci_name != "etc."
		)


length_taxa_name <- sapply(strsplit(afcd_dat_all_names$sci_name, " "), length)
ending_in_ae <- str_detect(afcd_dat_all_names$sci_name,"ae$")
afcd_dat_all_names[ending_in_ae==TRUE & length_taxa_name==1,]$family <- afcd_dat_all_names[ending_in_ae==TRUE & length_taxa_name==1,]$genus
afcd_dat_all_names[ending_in_ae==TRUE & length_taxa_name==1,][,c("sci_name","genus")] <- c(NA,NA)

afcd_dat_all_names[which(afcd_dat_all_names$sci_name=="bothidae and pleuronectidae"),][c("genus","family","order")] <- c(NA,NA,"pleuronectiformes")
afcd_dat_all_names[which(afcd_dat_all_names$sci_name=="penaeidae and pandalidae"),][c("genus","family","order")] <- c(NA,NA,"decapoda")
afcd_dat_all_names[which(afcd_dat_all_names$sci_name=="loligoidae and ommastrephidae"),][c("genus","family","order","phylum")] <- c(NA,NA,NA,"mollusca")
afcd_dat_all_names[which(afcd_dat_all_names$sci_name=="percichthyidae and centrarchidae"),][c("genus","family","order")] <- c(NA,NA,"perciformes")
afcd_dat_all_names[which(afcd_dat_all_names$sci_name=="ostreidae family including crassostrea gigas"),][c("genus","family")] <- c(NA,"ostreidae")

afcd_dat_all_names[which(str_detect(afcd_dat_all_names$genus,"ae$")==TRUE) ,]$family <- afcd_dat_all_names[which(str_detect(afcd_dat_all_names$genus,"ae$")==TRUE) ,]$genus
afcd_dat_all_names[which(str_detect(afcd_dat_all_names$genus,"ae$")==TRUE) ,][,c("sci_name","genus")] <- c(NA,NA)



afcd_dat_all_names[which(afcd_dat_all_names$sci_name=="gastropoda"),][,c("sci_name","genus","family","order","class","phylum")] <- c(NA,NA,NA,NA,"gastropoda","mollusca")
afcd_dat_all_names[which(afcd_dat_all_names$sci_name=="bivalvia"),][,c("sci_name","genus","family","order","class","phylum")] <- c(NA,NA,NA,NA,"bivalvia","mollusca")

afcd_dat_all_names[which(afcd_dat_all_names$genus=="spirulina"),][c("family")] <- c("spirulinaceae")
afcd_dat_all_names[which(afcd_dat_all_names$genus=="spirulina"),][c("order")] <- c("spirulinales")
afcd_dat_all_names[which(afcd_dat_all_names$genus=="spirulina"),][c("class")] <- c("cyanophyceae")
afcd_dat_all_names[which(afcd_dat_all_names$genus=="spirulina"),][c("phylum")] <- c("cyanobacteria")

afcd_dat_all_names[which(afcd_dat_all_names$genus=="barbodes"),][c("family")] <- c("cyprinidae")
afcd_dat_all_names[which(afcd_dat_all_names$genus=="barbodes"),][c("order")] <- c("cypriniformes")

afcd_dat_all_names[which(afcd_dat_all_names$genus=="carpioides"),][c("family")] <- c("catostomidae")
afcd_dat_all_names[which(afcd_dat_all_names$genus=="carpioides"),][c("order")] <- c("cypriniformes")

afcd_dat_all_names[which(afcd_dat_all_names$genus=="curimata"),][c("family")] <- c("curimatidae")
afcd_dat_all_names[which(afcd_dat_all_names$genus=="curimata"),][c("order")] <- c("characiformes")

afcd_dat_all_names[which(afcd_dat_all_names$genus=="eleginus"),][c("family")] <- c("gadidae")
afcd_dat_all_names[which(afcd_dat_all_names$genus=="curimata"),][c("order")] <- c("characiformes")
afcd_dat_all_names[which(afcd_dat_all_names$genus=="helostoma"),][c("family")] <- c("helostomatidae")
afcd_dat_all_names[which(afcd_dat_all_names$genus=="helostoma"),][c("order")] <- c("anabantiformes")


afcd_dat_all_names[which(afcd_dat_all_names$family=="ambassidae"),][c("order")] <- c("perciformes")
afcd_dat_all_names[which(afcd_dat_all_names$family=="callanthiidae"),][,c("order")] <- c("perciformes")
afcd_dat_all_names[which(afcd_dat_all_names$family=="centropomidae"),][,c("order")] <- c("perciformes")
afcd_dat_all_names[which(afcd_dat_all_names$family=="embiotocidae"),][,c("order")] <- c("perciformes")
afcd_dat_all_names[which(afcd_dat_all_names$family=="lactariidae"),][,c("order")] <- c("perciformes")
afcd_dat_all_names[which(afcd_dat_all_names$family=="malacanthidae"),][,c("order")] <- c("perciformes")
afcd_dat_all_names[which(afcd_dat_all_names$family=="menidae"),][,c("order")] <- c("perciformes")
afcd_dat_all_names[which(afcd_dat_all_names$family=="moronidae"),][,c("order")] <- c("perciformes")
afcd_dat_all_names[which(afcd_dat_all_names$family=="pachychilidae"),][c("class")] <- c("gastropoda")
afcd_dat_all_names[which(afcd_dat_all_names$family=="pachychilidae"),][c("phylum")] <- c("mollusca")
afcd_dat_all_names[which(afcd_dat_all_names$family=="planorbidae"),][c("class")] <- c("gastropoda")
afcd_dat_all_names[which(afcd_dat_all_names$family=="planorbidae"),][c("phylum")] <- c("mollusca")

afcd_dat_all_names[which(afcd_dat_all_names$family=="platyrhinidae"),][,c("order")] <- c("myliobatiformes")
afcd_dat_all_names[which(afcd_dat_all_names$family=="platyrhinidae"),][,c("phylum")] <- c("chondrichthyes")
afcd_dat_all_names[which(afcd_dat_all_names$family=="platyrhinidae"),][,c("class")] <- c("chordata")

afcd_dat_all_names[which(afcd_dat_all_names$family=="pomacanthidae"),][,c("order")] <- "perciformes"
afcd_dat_all_names[which(afcd_dat_all_names$family=="potamididae"),][c("class")] <- c("gastropoda")
afcd_dat_all_names[which(afcd_dat_all_names$family=="scatophagidae"),][,c("order")] <- "perciformes"
afcd_dat_all_names[which(afcd_dat_all_names$family=="semisulcospiridae"),][,c("order")] <- c("sorbeoconcha")
afcd_dat_all_names[which(afcd_dat_all_names$family=="semisulcospiridae"),][,c("class")] <- c("gastropoda")
afcd_dat_all_names[which(afcd_dat_all_names$family=="semisulcospiridae"),][,c("phylum")] <- c("mollusca")
afcd_dat_all_names[which(afcd_dat_all_names$family=="sillaginidae"),][,c("order")] <- c("perciformes")
afcd_dat_all_names[which(afcd_dat_all_names$family=="sphyraenidae"),][,c("order")] <- c("perciformes")
afcd_dat_all_names[which(afcd_dat_all_names$family=="paguridae"),][,c("order")] <- c("decapoda")
afcd_dat_all_names[which(afcd_dat_all_names$family=="paguridae"),][,c("class")] <- c("malacostraca")


afcd_dat_all_names[which(afcd_dat_all_names$class=="phaeophyceae"),][,c("phylum")] <- c("ochrophyta")

# now fill any NAs in our taxonomic data
setDT(afcd_dat_all_names[ending_in_ae!=TRUE & length_taxa_name>1,])[, family:= family[!is.na(family)][1L] , by = genus] #fill family by genus (EXCEPT where the lowest classification is family)
setDT(afcd_dat_all_names)[, order:= order[!is.na(order)][1L] , by = family] #fill order by family
setDT(afcd_dat_all_names)[, class:= class[!is.na(class)][1L] , by = order] #fill class by order
setDT(afcd_dat_all_names)[, phylum:= phylum[!is.na(phylum)][1L] , by = class] #fill phylum by class

# this creates one bug that repeats over itself. There are a few hundred rows with no family information in our database 
# so it incorrectly fills order and phylum from the last known family association. we don't want that

# removes all incorrect order and phylum associations where family is NA (no known associations with information)
afcd_dat_all_names[is.na(afcd_dat_all_names$family)==TRUE,]$order <- NA
afcd_dat_all_names[is.na(afcd_dat_all_names$family)==TRUE,]$class <- NA
afcd_dat_all_names[is.na(afcd_dat_all_names$family)==TRUE,]$phylum <- NA

afcd_dat_all_names[which(afcd_dat_all_names$sci_name=="gobiidae"),]

write.csv(
	afcd_dat_all_names,
	file.path(
		wk_dir,
		"data",
		"OutputsFromR",
		"afcd_gaps",
		"afcd_taxonomy.csv"
		),
	row.names=FALSE
	)


length(unique(afcd_dat_all_names$sci_name))
length(unique(afcd_dat_all_names$genus))
length(unique(afcd_dat_all_names$family))
length(unique(afcd_dat_all_names$order))
length(unique(afcd_dat_all_names$phylum))

# Freshwater match


# clean freshwater names to make match more efficient remove any missing values or NAs from list (not great for the merge)
fw_sci <- unique(fw_dat$sci.name)
fw_sci <- fw_sci[fw_sci!=""]
fw_sci <- fw_sci[is.na(fw_sci)==FALSE]
fw_sci <- tolower(fw_sci)
fw_sci <- sapply(fw_sci, function(x) as.character(x))
names(fw_sci) <- NULL

fw_match_index_species <- amatch(fw_sci,afcd_dat_all_names$sci_name,maxDist=2)
fw_match_species <- afcd_dat_all_names$sci_name[fw_match_index_species]
# length(fw_match_species[is.na(fw_match_species)==TRUE])
# species_test <- data.frame(
# 	fw_taxa=fw_sci,
# 	match_afcd=fw_match_species
# 	)
# species_test[order(species_test$fw_taxa),]



fw_match_index_genus <- amatch(sapply(strsplit(fw_sci," "), `[`, 1),afcd_dat_all_names$genus,maxDist=2)
fw_match_genus <- afcd_dat_all_names$genus[fw_match_index_genus]
# length(fw_match_genus[is.na(fw_match_genus)==TRUE])
# genus_test <- data.frame(
# 	fw_taxa=fw_sci,
# 	match_afcd=fw_match_genus
# 	)
# genus_test[order(genus_test$fw_taxa),]


fw_match_index_family <- amatch(fw_sci,afcd_dat_all_names$family,maxDist=1)
fw_match_family <- afcd_dat_all_names$family[fw_match_index_family]
# length(fw_match_family[is.na(fw_match_family)==TRUE])
# family_test <- data.frame(
# 	fw_taxa=fw_sci,
# 	match_afcd=fw_match_family
# 	)
# family_test[order(family_test$fw_taxa),]


fw_match_index_order <- amatch(fw_sci,afcd_dat_all_names$order,maxDist=1)
fw_match_order <- afcd_dat_all_names$order[fw_match_index_order]
# length(fw_match_order[is.na(fw_match_order)==TRUE])
# order_test <- data.frame(
# 	fw_taxa=fw_sci,
# 	match_afcd=fw_match_order
# 	)
# order_test[order(order_test$fw_taxa),]


fw_match_index_class <- match(fw_sci,afcd_dat_all_names$class)
fw_match_class <- afcd_dat_all_names$class[fw_match_index_class]
# length(fw_match_class[is.na(fw_match_class)==TRUE])
# order_test <- data.frame(
# 	fw_taxa=fw_sci,
# 	match_afcd=fw_match_class
# 	)
# order_test[order(order_test$fw_taxa),]

fw_match_index_phylum <- match(fw_sci,afcd_dat_all_names$phylum)
fw_match_phylum <- afcd_dat_all_names$phylum[fw_match_index_phylum]
# length(fw_match_phylum[is.na(fw_match_phylum)==TRUE])
# order_test <- data.frame(
# 	fw_taxa=fw_sci,
# 	match_afcd=fw_match_phylum
# 	)
# order_test[order(order_test$fw_taxa),]

fw_match <- ifelse(is.na(fw_match_species)==TRUE,fw_match_genus,fw_match_species)
fw_match <- ifelse(is.na(fw_match)==TRUE,fw_match_family,fw_match)
fw_match <- ifelse(is.na(fw_match)==TRUE,fw_match_order,fw_match)
fw_match <- ifelse(is.na(fw_match)==TRUE,fw_match_class,fw_match)
fw_match <- ifelse(is.na(fw_match)==TRUE,fw_match_phylum,fw_match)

fw_match_info <- data.frame(
	fw_taxa_name=fw_sci,
	fw_match_key=fw_match,
	afcd_match_species=fw_match_species,
	afcd_match_genus=fw_match_genus,
	afcd_match_family=fw_match_family,
	afcd_match_order=fw_match_order,
	afcd_match_class=fw_match_class,
	afcd_match_phylum=fw_match_phylum
	)


1-dim(fw_match_info[is.na(fw_match_info$fw_match_key)==TRUE,])[1]/dim(fw_match_info)[1]


write.csv(
	fw_match_info,
	file.path(
		wk_dir,
		"data",
		"OutputsFromR",
		"afcd_gaps",
		"fw_afcd_gaps.csv"
		),
	row.names=FALSE
	)


# # This fills the higher level taxonomic information for any taxa for which we have a match at the lower taxonomic level (e.g., if we have genus, it fills family)
# fw_match_info$afcd_match_family <- afcd_dat_all_names$family[ifelse(is.na(fw_match_info$afcd_match_genus)==FALSE,match(fw_match_info$afcd_match_genus,afcd_dat_all_names$genus),NA)]
# fw_match_info$afcd_match_order <- afcd_dat_all_names$order[ifelse(is.na(fw_match_info$afcd_match_family)==FALSE,match(fw_match_info$afcd_match_family,afcd_dat_all_names$family),NA)]




# Marine match

# clean the SAU names to make string matching more efficient
mar_sci <- unique(mar_dat$scientific_name)
mar_sci <- mar_sci[mar_sci!=""]
mar_sci <- mar_sci[is.na(mar_sci)==FALSE]
mar_sci <- tolower(mar_sci)
mar_sci <- sapply(mar_sci, function(x) as.character(x))


length_taxa_name_sau <- sapply(strsplit(mar_sci, " "), length) #calculates the # of words in each string, we don't want anything with rep
misc_not_identified <- str_detect(mar_sci,"identified|miscellaneous") #remove miscellaneous or "not identified" taxonomic info from match (it actually finds some for marine fishes not identified)

rem_dup_word <- function(x){ #function to remove duplicated words associated with subspecies (e.g., clupea pallasii pallasii)... helps with match from https://stackoverflow.com/questions/20283624/removing-duplicate-words-in-a-string-in-r
x <- tolower(x)
paste(unique(trimws(unlist(strsplit(x,split=" ",fixed=F,perl=T)))),collapse = 
" ")
}



mar_match_index_species <- amatch(
	sapply(mar_sci, function(x) rem_dup_word(x)),
	afcd_dat_all_names$sci_name,
	maxDist=2)
mar_match_species <- afcd_dat_all_names$sci_name[mar_match_index_species]
# length(mar_match_species[is.na(mar_match_species)==TRUE])
# species_test <- data.frame(
# 	mar_taxa=mar_sci,
# 	match_afcd=mar_match_species
# 	)
# species_test[order(species_test$mar_taxa),]

# need to figure out how to ONLY do this for values with no more than two words in each string (Marine nei etc is getting converted to morone)
mar_match_index_genus <- amatch(sapply(strsplit(mar_sci," "), `[`, 1),afcd_dat_all_names$genus,maxDist=2)
mar_match_genus <- afcd_dat_all_names$genus[mar_match_index_genus]
length(mar_match_genus[is.na(mar_match_genus)==TRUE])
# genus_test <- data.frame(
# 	mar_taxa=mar_sci,
# 	match_afcd=mar_match_genus
# 	)
# genus_test[order(genus_test$mar_taxa),]


mar_match_index_family <- amatch(mar_sci,afcd_dat_all_names$family,maxDist=1)
mar_match_family <- afcd_dat_all_names$family[mar_match_index_family]
# length(mar_match_family[is.na(mar_match_family)==TRUE])
# family_test <- data.frame(
# 	mar_taxa=mar_sci,
# 	match_afcd=mar_match_family
# 	)
# family_test[order(family_test$mar_taxa),]


mar_match_index_order <- amatch(mar_sci,afcd_dat_all_names$order,maxDist=1)
mar_match_order <- afcd_dat_all_names$order[mar_match_index_order]
# length(mar_match_order[is.na(mar_match_order)==TRUE])
# order_test <- data.frame(
# 	mar_taxa=mar_sci,
# 	match_afcd=mar_match_order
# 	)
# order_test[order(order_test$mar_taxa),]

mar_match_index_class <- match(mar_sci,afcd_dat_all_names$class)
mar_match_class <- afcd_dat_all_names$class[mar_match_index_class]
# length(mar_match_class[is.na(mar_match_class)==TRUE])
# order_test <- data.frame(
# 	mar_taxa=mar_sci,
# 	match_afcd=mar_match_class
# 	)
# order_test[order(order_test$mar_taxa),]

mar_match_index_phylum <- match(mar_sci,afcd_dat_all_names$phylum)
mar_match_phylum <- afcd_dat_all_names$phylum[mar_match_index_phylum]
# length(mar_match_phylum[is.na(mar_match_phylum)==TRUE])
# order_test <- data.frame(
# 	mar_taxa=mar_sci,
# 	match_afcd=mar_match_phylum
# 	)
# order_test[order(order_test$mar_taxa),]
mar_sci[mar_sci=="cephalopoda"]
afcd_dat_all_names$class[which(afcd_dat_all_names$class=="cephalopoda")]

mar_match <- ifelse(is.na(mar_match_species)==TRUE,mar_match_genus,mar_match_species)
mar_match <- ifelse(is.na(mar_match)==TRUE,mar_match_family,mar_match)
mar_match <- ifelse(is.na(mar_match)==TRUE,mar_match_order,mar_match)
mar_match <- ifelse(is.na(mar_match)==TRUE,mar_match_class,mar_match)
mar_match <- ifelse(is.na(mar_match)==TRUE,mar_match_phylum,mar_match)

mar_match_info <- data.frame(
	mar_taxa_name=mar_sci,
	mar_match_key=mar_match,
	afcd_match_species=mar_match_species,
	afcd_match_genus=mar_match_genus,
	afcd_match_family=mar_match_family,
	afcd_match_order=mar_match_order,
	afcd_match_class=mar_match_class,
	afcd_match_phylum=mar_match_phylum
	)


mar_match_info[str_detect(mar_match_info$mar_taxa_name,"identified|miscellaneous"),][c("mar_match_key","afcd_match_genus")] <- NA

write.csv(
	mar_match_info,
	file.path(
		wk_dir,
		"data",
		"OutputsFromR",
		"afcd_gaps",
		"mar_afcd_gaps.csv"
		),
	row.names=FALSE
	)




mar_sci <- as.data.frame(mar_sci)
names(mar_sci) <- "mar_data"

mar_gaps <- left_join(mar_sci,mar_match_info,by=c("mar_data"="mar_match_key"))
names(mar_gaps)[1] <- "sau_match_name"

mar_match_info[which(mar_match_info$afcd_match_genus=="brevoortia"),]
# now merge this with the SAU landings data Daniel extracted
sau_landings <- mar_dat %>%
	select(scientific_name,tonnes) %>%
	mutate(scientific_name=tolower(scientific_name))

mar_gaps <- left_join(
	mar_match_info,
	sau_landings,
	by=c("mar_taxa_name"="scientific_name")) 


1-dim(mar_gaps[is.na(mar_gaps$mar_match_key)==TRUE,])[1]/dim(mar_gaps)[1]


write.csv(
	mar_gaps,
	file.path(
		wk_dir,
		"data",
		"OutputsFromR",
		"afcd_gaps",
		"mar_afcd_gaps_with_landings.csv"
		),
	row.names=FALSE
	)
