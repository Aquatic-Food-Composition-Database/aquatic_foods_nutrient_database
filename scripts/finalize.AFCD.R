# ===============================================================================
# Clean new AFCD database
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

synthesis_data <- "AFCD.w.FAO.USDA.csv"
worms_2017 <- "WoRMS.6.2017.csv"

g <- read.csv(
  here("data","OutputsFromR",synthesis_data),
  stringsAsFactors = F,header=T)
# taxonomic database from the World Register of Marine Species or (WoRMS). It's a bigger file. 
worms <- read.csv(
  here("data",worms_2017),
  header=T,stringsAsFactors = F)


Sys.setlocale('LC_ALL','C') #sets language to eliminate multibyte error





#_____________________________________________________________________________________________
# Cleans dataset
# : transforms mass for some nutrients where needed
# ________________________________________________________________________________________________


colnames(g) <- gsub("\\.\\.","\\.",colnames(g))
colnames(g) <- gsub("\\.\\.","\\.",colnames(g))



#converts all nutrient values to numeric class, but not the metadata from the merge halfway through
g[,20:378] <- apply(g[,20:378],MARGIN = 2,
                          function(x)
                            as.numeric(x)
)
#stop here for metadata from FAO (will move to front in EXCEL after exporting)
g[,393:length(g)] <- apply(g[,20:length(g)],MARGIN = 2,
                    function(x)
                      as.numeric(x)
)


# Check to make sure data is in the right unit. USDA Amino Acid data measures 
# amino acids in terms of grams and not micrograms (so need to multiply by 1e3)

# extract offending rows from USDA
amino.manip <- g[which( (g$Country.ISO3 == "USA")),]
#save all other rows to bind with following opeartions on offending rows
rest.unmanip <- g[which( (g$Country.ISO3 != "USA")),]

# vector based transformation for all aminos g[,30:51]
amino.manip[,30:51] <- lapply(amino.manip[30:51],
                             function(x){x*1e3}
)
#the very few aspartic acid values in USDA do appear to be in mg, so transform back
amino.manip$Aspartic.acid <- amino.manip$Aspartic.acid*rep_len(1e-3,length.out = length(amino.manip$Aspartic.acid)) 

# bind transformed amino values with rest of dataset. 

#search for other outliers (fatty acids thankfully good)
g.mod <- plyr::rbind.fill(rest.unmanip,amino.manip)



#scientific name from FAO Biodiversity dataset not correctly merged (ASFIS.Scientific.name), so do so here. 

for(i in 1:length(g.mod$Country.ISO3)) {
  if(g.mod$Country.ISO3[i]=="FAO.biodiv3"  & is.na(g.mod$Scientific.Name[i])==T) {
    g.mod$Scientific.Name[i] <- g.mod$ASFIS.Scientific.name[i]
  }
}


g.mod$Scientific.Name <- gsub("\\s*\\[[^\\]]+\\)","",g.mod$Scientific.Name)
g.mod$Scientific.Name <- gsub("\\s*\\([^\\)]+\\)","",g.mod$Scientific.Name)




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


g.mod.all.names <- subset(g.mod,select=c(Scientific.Name))
g.mod.all.names$Scientific.Name <- trimws(g.mod.all.names$Scientific.Name,which=c("both"))
g.mod.all.names <- as.data.frame(apply(g.mod.all.names, 2, function(x) gsub("^$|^ $", NA, x)))

g.mod.all.names$match.names <- "" #create blank column to be filled
g.mod.all.names <- as.data.frame(g.mod.all.names)
g.mod.all.names$Scientific.Name <- as.character(g.mod.all.names$Scientific.Name)
worms <- as.data.frame(worms,stringsAsFactors=F)

#below is a messy merge for each row based on how closely the character string
# matches a string in the potential scientific names dataset
#if it matches, it is added to the matched name vector

# fuzzy merge to add multiple science names where available
#!!! Commented out b/c takes a LONG time to run, the imported CSV immediately below is the output
# includes a counter so progress can be marked.
for(i in 1:dim(g.mod.all.names)[1]) {
  x <- agrep(g.mod.all.names$Scientific.Name[i], worms$potential.scinames,
             ignore.case=TRUE, value=TRUE,
             max.distance = 0.05, useBytes = TRUE)
  x <- paste0(x,"")
  g.mod.all.names$match.names[i] <- x
  Sys.sleep(.1)      # some loop operations to create a progress bar (takes a LONG time)
  cat(i, "of 3979\r")
  flush.console()
}

write.csv(g.mod.all.names,
  here("OutputsFromR","matched.worms.AFCD.csv"),
  row.names=FALSE)


# ok, here's the shortcut, I commented out  the fuzzy loop because it took ages. 
# I saved the output as a csv and commented out the actual fuzzy merge and just load that output below.
g.mod.all.names <- read.csv(
  here("data","OutputsFromR","matched.worms.genus.csv"),
  header=T,stringsAsFactors = F)

worms.taxonomy <- worms[,which(names(worms) %in% c("potential.scinames","class","order","family","genus"))]

names(worms.taxonomy) <- c("Class.worms","Order.worms",
                           "Family.worms","Genus.worms","alt.scinames")

g.mod.names.worms.phylo <- merge(worms.taxonomy,g.mod.all.names,by="alt.scinames")
g.mod.names.worms.phylo <- g.mod.names.worms.phylo[!duplicated(g.mod.names.worms.phylo),]

g.mod.worms <- merge(g.mod.names.worms.phylo,g.mod, by="Scientific.Name",all.y = T)



# now add in any missing Order, Family names in WoRMS using AFCD existing data (from FISHBASE)
for(i in 1:dim(g.mod.worms)[1]) {
  if(is.na(g.mod.worms$Order.worms[i])==T) {
    g.mod.worms$Order.worms[i] <- g.mod.worms$Order[i]
  }
  if(is.na(g.mod.worms$Family.worms)==T) {
    g.mod.worms$Family.worms[i] <- g.mod.worms$Family[i]
  }
}

# with these filled in, no need for the old order and Family columns
g.mod.final <- subset(g.mod.worms, select=-c(Order,Family,X.1,X) )

# write finalized dataset to CSV
write.csv(
  g.mod.final,
  here("data","OutputsFromR","AFCD.final.csv"),
  row.names = F) 
