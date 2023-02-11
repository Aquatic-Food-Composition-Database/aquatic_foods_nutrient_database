# ===============================================================================
# Incorporate two separate UN Food and Agricultural Organisation food composition databases

# 1) FAO/INFOODS Global Food Composition Database for Fish and Shellfish 
# at a global level, nutrient data (energy, macronutrients, main minerals and vitamins, amino acids and fatty acids) 
# for various fish and shellfish species, covering different catch regions and/or origins of aquaculture production

# 2) FAO/INFOODS Food Composition Database for Biodiversity 
# To allow food composition database compilers to include nutritional values 
# for wild and underutilized foods as well as for foods below species level based on the data 

# Name: J. Zachary (Zach) Koehn
# Email: zkoehn@gmail.com
# For: Chris Golden - Harvard University
# Date started: 01/31/2017
# Revised: 04/30/2020
# ===============================================================================

#_____________________________________________________________________________________________
# read data and load libraries
# ________________________________________________________________________________________________

library(tidyverse);
library(here)
library(plyr)

fao.infoods.file <- "fao.infoods.ufish1.csv"
fao.biodiv.file <- "fao.infoods.biodiv3.csv"
AFCD.file <- "binded.mod.AFCD.csv"
fao.infoods.scinames.file <- "fao.infoods.ufish.scinames.csv"

# read data
fao.global <- read.csv(
  here::here("data",fao.infoods.file),
  header = T,stringsAsFactors = F)

fao.biodiv <- read.csv(
  here::here("data",fao.biodiv.file),
  header = T,stringsAsFactors = F)

AFCD.for.bind.dat <- read.csv(
  here::here("data",AFCD.file),
  stringsAsFactors = F,header = T)

fao.infoods.scinames <- read.csv(
  here::here("data",fao.infoods.scinames.file),
  header=T,stringsAsFactors = F)


#FAO INFoods metadata variables useful for merge include:
# ISSCAAP - ISSCAAP Code (a double figure code dividing commercial species into groups according to their taxonomical, ecological and economical features)
# 3-Alpha - Inter-Agency 3-Alpha Code (a three letter code which identifies each species in a unique way to be used for exchange of data with national correspondents and among fishery agencies).
# INFOODS Ufish does not have scientific names in the same .csv as nutrient values
# science names are included in another tab of workbook, and share 3-Alpha codes, so merge by those
colnames(fao.infoods.scinames) <- c("FAO.3A_CODE","Food.name.in.English","Scientific.name")
fao.infoods.scinames <- fao.infoods.scinames[,-2]


#but clean science names first of taxonomists (sorry to those who like these things... there is no standard format==nightmare)
#remove names of taxonomists from scientific name for merge with AFCD-FISH
#removes taxonimist identifier specified by "(Taxonomist surname)"
fao.infoods.scinames$Scientific.name<- sub("\\(.*","",fao.infoods.scinames$Scientific.name) 
fao.infoods.scinames$Scientific.name<- sub(",.*","",fao.infoods.scinames$Scientific.name)
fao.infoods.scinames$Scientific.name<- sub("\\[","",fao.infoods.scinames$Scientific.name)
fao.infoods.scinames$Scientific.name<- sub("\xae","",fao.infoods.scinames$Scientific.name)
#trim whitespace at the end of the strings
fao.infoods.scinames$Scientific.name <- trimws(fao.infoods.scinames$Scientific.name,which=c("left"))
fao.infoods.scinames$Scientific.name <- trimws(fao.infoods.scinames$Scientific.name,which=c("right"))


#merge scinames with INFOODs
fao.infoods <- merge(fao.infoods.scinames,fao.global,by=c("FAO.3A_CODE"))
# Add categorical variable for which database each row originates, to add to "Country.ISO"
fao.infoods$Country.ISO3 <- "FAO.infoods.ufish1"
fao.biodiv$Country.ISO3 <- "FAO.biodiv3"




intersect(colnames(fao.infoods),colnames(fao.biodiv))

#bind together FAO infoods and FAO Biodiversity and filling like columns (any unique columns go to back of dataframe)
fao.data <- plyr::rbind.fill(fao.infoods,fao.biodiv)

#for merging with the USDA and AFCD data, fao data needs to rename a number of columns
names(fao.data)[2] <- "Scientific.Name"
names(fao.data)[11] <- "Energy.total.metabolizable.calculated.from.the.energy.producing.food.components.original.as.from.source.kj"
names(fao.data)[12] <- "Energy.total.metabolizable.calculated.from.the.energy.producing.food.components.original.as.from.source.kcal"
names(fao.data)[207] <- "Energy.gross.determined.by.direct.analysis.using.bomb.calorimetry.kcal"
names(fao.data)[208] <- "Dry.Matter"
names(fao.data)[210] <- "Protein.total.calculated.from.total.nitrogen"  
names(fao.data)[211] <- "Protein.total.method.of.determination.unknown.or.variable"
names(fao.data)[212] <- "Nitrogen.protein"
names(fao.data)[213] <- "Nitrogen.nonprotein"
names(fao.data)[214] <- "Fat.total"
names(fao.data)[215] <- "Fat.method.of.determination.unknown.or.mixed.methods"
names(fao.data)[216] <- "Fatty.acids.not.identified"
names(fao.data)[217] <- "Fatty.acids.total.trans"
names(fao.data)[218] <- "Carbohydrate.total.calculated.by.difference"
names(fao.data)[219] <- "Carbohydrate.method.of.determination.unknown.or.variable"
names(fao.data)[229] <- "Fluoride"
names(fao.data)[230] <- "Lithium"
names(fao.data)[233] <- "Salt"
names(fao.data)[235] <- "Rubidium"
names(fao.data)[237] <- "Tin"
names(fao.data)[240] <- "Aluminum"
names(fao.data)[242] <- "Barium"
names(fao.data)[252] <- "Vitamin.A.method.of.determination.unknown"
names(fao.data)[264] <- "Thiamin"
names(fao.data)[266] <- "Vitamin.B6.total.calculated.by.summation"
names(fao.data)[281] <- "Ornithine"
names(fao.data)[283] <- "Alanine.plus.Arginine"
names(fao.data)[284] <- "Asparagine.plus.Aspartic.Acid"
names(fao.data)[285] <- "Glutamine.plus.Glutamic.Acid" 
names(fao.data)[286] <- "Glutamine.plus.Histidine" 
names(fao.data)[287] <- "Cholesterol.determined.by.enzymatic.or.chromatographic.method"
names(fao.data)[289] <- "Fatty.acids.total.free"


# cleans dataset for merge with 
names(AFCD.for.bind.dat)[22] <- "Energy.total.metabolizable.calculated.from.the.energy.producing.food.components.original.as.from.source.kj"
names(AFCD.for.bind.dat)[23] <- "Energy.total.metabolizable.calculated.from.the.energy.producing.food.components.original.as.from.source.kcal"
names(AFCD.for.bind.dat)[24] <- "Energy.gross.determined.by.direct.analysis.using.bomb.calorimetry.kj"
names(AFCD.for.bind.dat)[25] <- "Energy.gross.determined.by.direct.analysis.using.bomb.calorimetry.kcal"
colnames(AFCD.for.bind.dat) <- gsub("\\.\\.","\\.",colnames(AFCD.for.bind.dat))
colnames(AFCD.for.bind.dat) <- gsub("\\.\\.","\\.",colnames(AFCD.for.bind.dat))
colnames(fao.data) <- gsub("\\.\\.","\\.",colnames(fao.data))
colnames(fao.data) <- gsub("\\.\\.","\\.",colnames(fao.data))

AFCD.w.FAO.USDA <- rbind.fill(AFCD.for.bind.dat,fao.data)
colnames(AFCD.w.FAO.USDA)




#converts all nutrient values to numeric class, but not the metadata from the merge halfway through
AFCD.w.FAO.USDA[,20:377] <- apply(AFCD.w.FAO.USDA[,20:378],MARGIN = 2,
                    function(x)
                      as.numeric(x)
)
#stop here for metadata from FAO (will move to front in EXCEL after exporting)
AFCD.w.FAO.USDA[,393:length(AFCD.w.FAO.USDA)] <- apply(AFCD.w.FAO.USDA[,20:393:length(AFCD.w.FAO.USDA)],MARGIN = 2,
                           function(x)
                             as.numeric(x)
)



# integrate the processing/preparation from USDA.scrape


# include processing coding (r=raw; p=processed in any way) for newly integrated data

Sys.setlocale('LC_ALL','C') #sets language to eliminate multibyte error
for(i in 1:length(AFCD.w.FAO.USDA$Processing)) {
  if(AFCD.w.FAO.USDA$Country.ISO3[i]=="FAO.infoods.ufish1" && grepl("raw",AFCD.w.FAO.USDA$Food.name.in.English)[i]==T)
  {
    AFCD.w.FAO.USDA$Processing[i] <- "r"
  }
}

for(i in 1:length(AFCD.w.FAO.USDA$Processing)) {
  if(AFCD.w.FAO.USDA$Country.ISO3[i]=="FAO.biodiv3" && grepl("raw",AFCD.w.FAO.USDA$Food.name.in.English)[i]==T)
  {
    AFCD.w.FAO.USDA$Processing[i] <- "r"
  } 
}


#coding for type of food prep (e.g., raw, baked, smoked, grilled, pickled)
for(i in 1:length(AFCD.w.FAO.USDA[,1])) {
  if(
    grepl("raw",AFCD.w.FAO.USDA$Food.name.in.English[i])==T
  ) {
    AFCD.w.FAO.USDA$Preparation[i] <- "raw"
  }
  if(
    grepl("canned",AFCD.w.FAO.USDA$Food.name.in.English[i])==T
  ) {
    AFCD.w.FAO.USDA$Preparation[i] <- "c"
  }
  if(
    grepl("grilled",AFCD.w.FAO.USDA$Food.name.in.English[i])==T
  ) {
    AFCD.w.FAO.USDA$Preparation[i] <- "r"
  }
  if(
    grepl("steamed",AFCD.w.FAO.USDA$Food.name.in.English[i])==T
  ) {
    AFCD.w.FAO.USDA$Preparation[i] <- "st"
  }
  if(
    grepl("boiled",AFCD.w.FAO.USDA$Food.name.in.English[i])==T
  ) {
    AFCD.w.FAO.USDA$Preparation[i] <- "boil"
  }
  if(
    grepl("baked",AFCD.w.FAO.USDA$Food.name.in.English[i])==T
  ) {
    AFCD.w.FAO.USDA$Preparation[i] <- "bake"
  }
  if(
    grepl("fried",AFCD.w.FAO.USDA$Food.name.in.English[i])==T
  ) {
    AFCD.w.FAO.USDA$Preparation[i] <- "f"
  }
  if(
    grepl("smoked",AFCD.w.FAO.USDA$Food.name.in.English[i])==T
  ) {
    AFCD.w.FAO.USDA$Preparation[i] <- "sm"
  }
  if(
    grepl("kippered",AFCD.w.FAO.USDA$Food.name.in.English[i])==T
  ) {
    AFCD.w.FAO.USDA$Preparation[i] <- "sm"
  }
  if(
    grepl("microwaved",AFCD.w.FAO.USDA$Food.name.in.English[i])==T
  ) {
    AFCD.w.FAO.USDA$Preparation[i] <- "m"
  }
  if(
    grepl("dried",AFCD.w.FAO.USDA$Food.name.in.English[i])==T
  ) {
    AFCD.w.FAO.USDA$Preparation[i] <- "d"
  }
  if(
    grepl("dry heat",AFCD.w.FAO.USDA$Food.name.in.English[i])==T
  ) {
    AFCD.w.FAO.USDA$Preparation[i] <- "b"
  }
  if(
    grepl("moist heat",AFCD.w.FAO.USDA$Food.name.in.English[i])==T
  ) {
    AFCD.w.FAO.USDA$Preparation[i] <- "st"
  }
  if(
    grepl("pickled",AFCD.w.FAO.USDA$Food.name.in.English[i])==T
  ) {
    AFCD.w.FAO.USDA$Preparation[i] <- "pkl"
  }
} 

# export finalized dataset
write.csv(
  AFCD.w.FAO.USDA, 
  here::here("data","OutputsFromR","AFCD.w.FAO.USDA.csv"),
  row.names = FALSE
)

