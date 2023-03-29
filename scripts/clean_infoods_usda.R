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

AFCD.w.FAO.USDA <- AFCD.w.FAO.USDA %>%
  select(
    GBD.Macro:Component.name,USDA.ndbno:RefID,Code:Latest.revision.in.version,Other,Edible.portion.coefficient:Tannins.total,EDIBLE:Fatty.acid.24.6,Dry.Matter:Wax.total
    ) %>%
  mutate(
    across(Edible.portion.coefficient:Wax.total,enc2utf8),
    across(Edible.portion.coefficient:Wax.total,function(x) as.numeric)
  )

AFCD.w.FAO.USDA[,37:409] <- apply(AFCD.w.FAO.USDA[,37:409],2,function(x) enc2utf8(x))
AFCD.w.FAO.USDA[,37:409] <- apply(AFCD.w.FAO.USDA[,37:409],2,function(x) as.numeric(x))

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


AFCD.w.FAO.USDA <- AFCD.w.FAO.USDA %>%
  # now change over the part from what we now know to be hidden in 'other'
  # unique(AFCD.w.FAO.USDA$Other)
  # based on the above
  mutate(
    Parts=ifelse(Other %in%c(
      "Anatomical parts excluded prior to analysis: Viscera.",
      "Anatomical parts excluded prior to analysis: Viscera, barbel.",
      "Anatomical parts excluded prior to analysis: Viscera, scales.",
      "Anatomical parts excluded prior to analysis: Viscera, fins.",
      "Anatomical parts excluded prior to analysis: Viscera, fins, scales, gills, operculum.",
      "Anatomical parts excluded prior to analysis: Viscera, fins, scales."
      ),"whole gutted",Parts)) %>%
  mutate(
    Parts=ifelse(Other %in% c(
      "Anatomical parts excluded prior to analysis: Viscera, shell, legs, tail.",
      "Anatomical parts excluded prior to analysis: Bones, viscera, fins.",
      "Anatomical parts excluded prior to analysis: Bones, viscera, scales, gills, fins.",
      "Anatomical parts excluded prior to analysis: Bones, viscera, barbell, gills.",
      "Anatomical parts excluded prior to analysis: Bones, fins, viscera.",
      "Anatomical parts excluded prior to analysis: Bones, viscera, gills, barbell.",
      "Anatomical parts excluded prior to analysis: Bones, viscera, fins, scales, operculum.",
      "Anatomical parts excluded prior to analysis: Bones, viscera, fins, scales, gills.",
      "Anatomical parts excluded prior to analysis: Bones, viscera, fins, barbell.",
      "Anatomical parts excluded prior to analysis: Bones, viscera, fins, scales.",
      "Anatomical parts excluded prior to analysis: Bones, viscera, fins, snout.",
      "Anatomical parts excluded prior to analysis: Bones, viscera, fins, skin, dorsal spine, snout.",
      "[Muscle B cut]",
      "Whole fillet"
      ),"muscle tissue",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"dressed w/ head"),"whole gutted",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"cleaned fish"),"whole gutted",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"edible portion (gutted)"),"whole gutted",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"without head"),"whole",Parts)) %>% 
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"edible parts"),"whole",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"edible part"),"whole",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"edible portion"),"whole",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"fillet"),"muscle tissue",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"deheaded, peeled"),"muscle tissue",Parts)) %>%  
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"edible muslce"),"muscle tissue",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"flesh"),"muscle tissue",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"flesh,"),"muscle tissue",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"muscle"),"muscle tissue",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"meat"),"muscle tissue",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"canned"),"muscle tissue",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"foot"),"muscle tissue",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"whole"),"whole",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"whole,"),"whole",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"mantle"),"mantle",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"roe"),"roe",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"eggs"),"roe",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"liver"),"liver",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"soft tissue"),"muscle tissue",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"tissue"),"muscle tissue",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"soft body"),"muscle tissue",Parts)) %>%
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"smoked"),"muscle tissue",Parts)) %>%  
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"skinned"),"muscle tissue",Parts)) %>%  
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"skin"),"muscle tissue",Parts)) %>% 
  mutate(Parts=ifelse(Country.ISO3=="USA" & is.na(Parts),"muscle tissue",Parts)) %>% 
  
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"hepatopancreas"),"viscera tissue",Parts)) %>% 
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"gonad*"),"reproductive tissue",Parts)) %>% 
  mutate(Parts=ifelse(str_detect(Food.name.in.English,"head"),"head",Parts)) %>% 
  mutate(Scientific.Name=ifelse(str_detect(Food.name.in.English,"Spotted featherback"),"Chitala spp.",Scientific.Name)) %>%  
  mutate(Scientific.Name=ifelse(str_detect(Food.name.in.English,"Bronze featherback"),"Notopterus notopterus",Scientific.Name)) %>%  
  mutate(Scientific.Name=ifelse(str_detect(Food.name.in.English,"Nile tilapia"),"Oreochromis niloticus",Scientific.Name)) %>%  
  mutate(Scientific.Name=ifelse(str_detect(Food.name.in.English,"pollock, Alaska"),"Gadus chalcogrammus",Scientific.Name)) %>%  
  mutate(Scientific.Name=ifelse(str_detect(Food.name.in.English,"cod, Pacific"),"Gadus macrocephalus",Scientific.Name)) %>%  
  mutate(Scientific.Name=ifelse(str_detect(Food.name.in.English,"crab, dungeness"),"Metacarcinus magister",Scientific.Name)) %>%  
mutate(
    Parts=ifelse(Other %in% c(
    "Anatomical parts excluded prior to analysis: No parts removed."
  ),"whole",Parts))

# AFCD.w.FAO.USDA %>%
#   filter(is.na(Parts)) %>%
#   View()

# export finalized dataset
write.csv(
  AFCD.w.FAO.USDA, 
  here::here("data","OutputsFromR","AFCD.w.FAO.USDA.csv"),
  row.names = FALSE
)

