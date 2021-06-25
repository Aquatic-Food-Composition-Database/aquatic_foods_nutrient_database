# ===============================================================================
# Pulls from USDA database and merge with existing data. 
#
#
# Name: J. Zachary (Zach) Koehn
# Email: zkoehn@gmail.com
# For: Chris Golden - Harvard University, synthesizing aquatic foods database
# Date started: 01/31/2017
# Revised: 04/05/2020
# ===============================================================================

api.key <- "dHktMbVe1CasPvCeXZmiapiqQzfkQAiC1U4w2icV"
library(here)
library(jsonlite)



# # important nutrients to scrape (including those necessary to determine Drewnowski et al method)
# Scientific.Name,alt.scinames,Country.ISO3,
# # from fulgoni, Drewnowskiet al
#     # macronutrients, vitamins, minierals
#     Protein.total.calculated.from.total.nitrogen, nutrients=203#best coverage versus from protein nitrogen or unknown
#     dietary fibre, nutrients=291
#     Vitamin.A.retinol.activity.equivalent.RAE.calculated.by.summation.of.the.vitamin.A.activities.of.retinol.and.the.active.carotenoids, nutrients=320
#     Vitamin.C, , nutrients=401
#     Calcium, nutrients=301
#     Iron.total, nutrients=303
#     #LIM
#     Fatty.acids.total.saturated, nutrients=606
#     total sugars , nutrients=269
#     Sodium, nutrients=307
#    
#     # other important nutrients
#     
#     Zinc, nutrients=309
#     Selenium, nutrients=317
#     Vitamin.B12, nutrients=418
#     Vitamin.D (D2+D3), nutrients=328
#     Fatty.acid.20.5.n3, nutrients=629 #eicosapentaenoic acid (EPA)
#     Fatty.acid.22.6.n3 nutrients=621 #docosahexaenoic acid (DPA)
# 
#     Energy.total.metabolizable.calculated.from.the.energy.producing.food.components.original.as.from.source.kcal, nutrients=208 #more non-NA observations than bomb calorimetry
#     Fatty.acids.total.monounsaturated, nutrients=645
#     Fatty.acids.total.polyunsaturated, nutrients=646
# 
#     # notable missing nutrients
#     iodine
#     total PUFA n3
#     total PUFA n6

# # and the list of food groups for extracting
# "https://api.nal.usda.gov/ndb/list?format=json&lt=g&sort=n&api_key=DEMO_KEY"
# finfish and shellfish, fg=1500
# beef products, fg=1300
# dairy and egg products, fg=0100
# lamb, veal, game products, fg=1700
# pork products, fg=1000
# poultry products, fg=0500
# vegetables and veg product, fg=1100



# ________________________________________________________________________________________________
# Scrape USDA database for nutrient information to integrate with existing GENuS-FISH
# : broken down by metadata summary, macronutrients and micronutrients, fatty acids
# : because of limits on free use, had to break down the API calls, which was done by nutrient groups
# ________________________________________________________________________________________________

#create url links for each nutrient and food group
# this is currently only for seafood (fg=1500)
# energy
energy.url <- "http://api.nal.usda.gov/ndb/nutrients/?format=json&api_key=dHktMbVe1CasPvCeXZmiapiqQzfkQAiC1U4w2icV&nutrients=208&fg=1500&max=300"
#protein amino acids
protein.aminos.url <- "http://api.nal.usda.gov/ndb/nutrients/?format=json&api_key=dHktMbVe1CasPvCeXZmiapiqQzfkQAiC1U4w2icV&nutrients=203&nutrients=513&nutrients=511&nutrients=507&nutrients=515&nutrients=516&nutrients=512&nutrients=503&nutrients=504&nutrients=505&nutrients=506&nutrients=508&nutrients=517&nutrients=518&&=nutrients=502&nutrients=501&nutrients=509&nutrients=510&nutrients514&fg=1500&max=300"
#fatty acids (saturated, monounsaturated, polyunsaturated, total)
saturated.fatty.acids.url <- "http://api.nal.usda.gov/ndb/nutrients/?format=json&api_key=dHktMbVe1CasPvCeXZmiapiqQzfkQAiC1U4w2icV&nutrients=607&nutrients=608&nutrients=609&nutrients=610&nutrients=611&nutrients=612&nutrients=613&nutrients=614&nutrients=615&nutrients=624&nutrients=654&fg=1500&max=300"
monounsaturated.fatty.acids.url <- "http://api.nal.usda.gov/ndb/nutrients/?format=json&api_key=dHktMbVe1CasPvCeXZmiapiqQzfkQAiC1U4w2icV&nutrients=625&nutrients=697&nutrients=626&nutrients=687&nutrients=674&nutrients=663&nutrients=617&nutrients=628&nutrients=664&nutrients=630&fg=1500&max=1500"
polyunsaturated.fatty.acids.url <- "http://api.nal.usda.gov/ndb/nutrients/?format=json&api_key=dHktMbVe1CasPvCeXZmiapiqQzfkQAiC1U4w2icV&nutrients=618&nutrients=619&nutrients=853&nutrients=852&nutrients=689&nutrients=620&nutrients=858&nutrients=629&nutrients=631&nutrients=857&nutrients=621&fg=1500&max=1500"
# PUFA.potential.limited.fatty.acids.url <- "http://api.nal.usda.gov/ndb/nutrients/?format=json&api_key=dHktMbVe1CasPvCeXZmiapiqQzfkQAiC1U4w2icV&nutrients=851&fg=1500&max=1500"
totals.lipids.url <- "http://api.nal.usda.gov/ndb/nutrients/?format=json&api_key=dHktMbVe1CasPvCeXZmiapiqQzfkQAiC1U4w2icV&nutrients=204&nutrients=606&nutrients=645&nutrients=646&nutrients=605&fg=1500&max=1500"
#carbohydrates
carbs.url <- "http://api.nal.usda.gov/ndb/nutrients/?format=json&api_key=dHktMbVe1CasPvCeXZmiapiqQzfkQAiC1U4w2icV&nutrients=205&nutrients=209&nutrients=291&fg=1500&max=300"
# water ash+others  
water.ash.others.url <- "http://api.nal.usda.gov/ndb/nutrients/?format=json&api_key=dHktMbVe1CasPvCeXZmiapiqQzfkQAiC1U4w2icV&nutrients=255&nutrients=207&fg=1500&max=300"
#minerals and trace elements
minerals.elements.url <- "http://api.nal.usda.gov/ndb/nutrients/?format=json&api_key=dHktMbVe1CasPvCeXZmiapiqQzfkQAiC1U4w2icV&nutrients=301&nutrients=312&nutrients=303&nutrients=306&nutrients=304&nutrients=303&nutrients=306&nutrients=304&nutrients=315&nutrients=307&nutrients=305&nutrients=317&nutrients=309&fg=1500&max=300"
# vitamins   
vitamins.url <- "http://api.nal.usda.gov/ndb/nutrients/?format=json&api_key=dHktMbVe1CasPvCeXZmiapiqQzfkQAiC1U4w2icV&nutrients=320&nutrients=318&nutrients=319&nutrients=322&nutrients=321&nutrients=324&nutrients=326&nutrients=325&nutrients=328&nutrients=323&nutrients=404&nutrients=417&nutrients=406&nutrients=410&nutrients=410&nutrients=415&nutrients=418&nutrients=401&nutrients=430&fg=1500&max=300"
# sterols.flavonoids
sterols.flavonoids.url <- "http://api.nal.usda.gov/ndb/nutrients/?format=json&api_key=dHktMbVe1CasPvCeXZmiapiqQzfkQAiC1U4w2icV&nutrients=601&nutrients=641&fg=1500&max=300"

# paste experiment
ndbno <- 15015
api.key <- "dHktMbVe1CasPvCeXZmiapiqQzfkQAiC1U4w2icV"
url <- paste("https://api.nal.usda.gov/ndb/reports/?ndbno=",ndbno,"&type=s&format=json&,api_key=",api.key)
gadus.morhua <- "https://api.nal.usda.gov/ndb/reports/?ndbno=15015&type=s&format=json&api_key=dHktMbVe1CasPvCeXZmiapiqQzfkQAiC1U4w2icV"
GM.doc <- fromJSON(gadus.morhua,flatten = T)
GM.doc$report$food$nutrients$se
#run API for each nutrient type
energy.doc <- fromJSON(energy.url,flatten=T)

protein.aminos.doc <- fromJSON(protein.aminos.url,flatten=T)
saturated.fatty.acids.doc <- fromJSON(saturated.fatty.acids.url,flatten = T)
monounsaturated.fatty.acids.doc <- fromJSON(monounsaturated.fatty.acids.url,flatten = T)
polyunsaturated.fatty.acids.doc <- fromJSON(polyunsaturated.fatty.acids.url,flatten = T)
total.lipids.doc <- fromJSON(totals.lipids.url,flatten = T)
carbs.doc <- fromJSON(carbs.url,flatten=T)
water.ash.others.doc <- fromJSON(water.ash.others.url,flatten=T)
minerals.elements.doc <- fromJSON(minerals.elements.url,flatten=T)
vitamins.doc <- fromJSON(vitamins.url,flatten=T)  
sterols.flavonoids.doc <- fromJSON(sterols.flavonoids.url,flatten=T)



# ________________________________________________________________________________________________
# protocol for scraping USDA database for science names associated with each unique USDA NDB number
# Purpose: merge science names in with nutriiton composition call to USDA API  
# _________________________________________________________________________________________________

# ndbno.vec <- unique(summary.document$report$foods$ndbno)
# # create URL for each NDB number that will be called by JSON operation via vectorized operations
# list<-sapply(ndbno.vec, function(x) 
#   url<-paste0("https://api.nal.usda.gov/ndb/reports/?ndbno=", x,"&type=f&format=json&api_key=",api.key,sep="")
#   # fromJSON(url,flatten = T)$report$food$sn
# )
# url.vector <- as.vector(list)
# #Call for dataframe in JSON using USDA-API specificed URL
# extract.scinames<- sapply(url.vector, function(x)
#   fromJSON(x,flatten = T)$report$food$sn
# )
# scinames<-as.vector(extract.scinames)
#
#
# ndbno.scinames <- cbind(ndbno.vec,as.vector(scinames))
# ndbno.scinames
# # writecsv so don't have to waste time
# # calling the USDA API every time want to refer to science names
# # note will need to recall if anything new is added
# write.csv(ndbno.scinames,"NDB.scinames.csv")

# ________________________________________________________________________________________________
# appending nutrient info in long-form for each of the species and 
# for each nutrient group extracted above
# Purpose: creates table that can be merged by NDB number
# _________________________________________________________________________________________________
ndbno.scinames<-read.csv("NDB.scinames.csv",header=1,stringsAsFactors = F)
colnames(ndbno.scinames) <- c("rowID","ndbno","scientific.name")
#remove names of taxonomists from scientific name for merge with GENuS-FISH
ndbno.scinames$scientific.name<- sub("\\(.*","\\",ndbno.scinames$scientific.name) #removes taxonimist identifier specified by "(Taxonomist surname)"
ndbno.scinames$scientific.name<- sub(" L.","",ndbno.scinames$scientific.name)# remove taxonomist identifer
ndbno.scinames$scientific.name<- sub(" Tilesius","",ndbno.scinames$scientific.name)# remove taxonomist identifier Tilesius
ndbno.scinames$scientific.name<- sub(" Milne-Edwards","",ndbno.scinames$scientific.name)# remove taxonomist identifier Milne-Edwards
ndbno.scinames$scientific.name<- sub(" Valenciennes","",ndbno.scinames$scientific.name)# remove taxonomist identifer
ndbno.scinames$scientific.name<- sub(" L.","",ndbno.scinames$scientific.name)# remove taxonomist identifer
ndbno.scinames$scientific.name<- sub(" Girard","",ndbno.scinames$scientific.name)# remove taxonomist identifer
ndbno.scinames$scientific.name<- sub(" Richardson","",ndbno.scinames$scientific.name)# remove taxonomist identifer
ndbno.scinames$scientific.name<- sub(" Goode and Bean","",ndbno.scinames$scientific.name)# remove taxonomist identifer
ndbno.scinames$scientific.name<- sub(" Lamarck","",ndbno.scinames$scientific.name)# remove taxonomist identifer
ndbno.scinames$scientific.name<- sub(" Rathbun","",ndbno.scinames$scientific.name)# remove taxonomist identifer
ndbno.scinames$scientific.name<- sub(" Dana","",ndbno.scinames$scientific.name)# remove taxonomist identifer
#trim whitespace at the end of the strings
ndbno.scinames$scientific.name <- trimws(ndbno.scinames$scientific.name,which=c("left"))
ndbno.scinames$scientific.name <- trimws(ndbno.scinames$scientific.name,which=c("right"))
# ndbno.scinames$scientific.name<- sub(" ","_",ndbno.scinames$scientific.name)# remove taxonomist identifer
# head(ndbno.scinames)

#include food names
foodnames<-cbind(energy.doc$report$foods$ndbno,energy.doc$report$foods$name)
colnames(foodnames) <- c("ndbno","food.name")
names.for.merge<-merge(ndbno.scinames,foodnames,by="ndbno",all=T)
#energy
column.names<-c("ndbno",energy.doc$report$foods$nutrients[[1]]$nutrient)
energy.mat<-matrix(nrow=length(as.data.frame(energy.doc)[,3]),ncol=(1+length(energy.doc$report$foods$nutrients[[1]]$nutrient)))
colnames(energy.mat)<-column.names
for(i in 1:length(as.data.frame(energy.doc)[,3])) {
  energy.mat[i,]<-c(energy.doc$report$food$ndbno[[i]],
                    energy.doc$report$foods$nutrients[[i]]$gm)
}

#protein and amino acids
column.names<-c("ndbno",protein.aminos.doc$report$foods$nutrients[[1]]$nutrient)
protein.aminos.mat<-matrix(nrow=length(as.data.frame(protein.aminos.doc)[,3]),ncol=(1+length(protein.aminos.doc$report$foods$nutrients[[1]]$nutrient)))
colnames(protein.aminos.mat)<-column.names
for(i in 1:length(as.data.frame(protein.aminos.doc)[,3])) {
  protein.aminos.mat[i,]<-c(protein.aminos.doc$report$food$ndbno[[i]],
                    protein.aminos.doc$report$foods$nutrients[[i]]$gm)
}

#saturated fatty acids
column.names<-c("ndbno",saturated.fatty.acids.doc$report$foods$nutrients[[1]]$nutrient)
saturated.fatty.acids.mat<-matrix(nrow=length(as.data.frame(saturated.fatty.acids.doc)[,3]),ncol=(1+length(saturated.fatty.acids.doc$report$foods$nutrients[[1]]$nutrient)))
colnames(saturated.fatty.acids.mat)<-column.names
for(i in 1:length(as.data.frame(saturated.fatty.acids.doc)[,3])) {
  saturated.fatty.acids.mat[i,]<-c(saturated.fatty.acids.doc$report$food$ndbno[[i]],
                    saturated.fatty.acids.doc$report$foods$nutrients[[i]]$gm)
}
#monounsaturated fatty acids
column.names<-c("ndbno",monounsaturated.fatty.acids.doc$report$foods$nutrients[[1]]$nutrient)
monounsaturated.fatty.acids.mat<-matrix(nrow=length(as.data.frame(monounsaturated.fatty.acids.doc)[,3]),ncol=(1+length(monounsaturated.fatty.acids.doc$report$foods$nutrients[[1]]$nutrient)))
colnames(monounsaturated.fatty.acids.mat)<-column.names
for(i in 1:length(as.data.frame(monounsaturated.fatty.acids.doc)[,3])) {
  monounsaturated.fatty.acids.mat[i,]<-c(monounsaturated.fatty.acids.doc$report$food$ndbno[[i]],
                    monounsaturated.fatty.acids.doc$report$foods$nutrients[[i]]$gm)
}
# polyunsaturated fatty acids
column.names<-c("ndbno",polyunsaturated.fatty.acids.doc$report$foods$nutrients[[1]]$nutrient)
polyunsaturated.fatty.acids.mat<-matrix(nrow=length(as.data.frame(polyunsaturated.fatty.acids.doc)[,3]),ncol=(1+length(polyunsaturated.fatty.acids.doc$report$foods$nutrients[[1]]$nutrient)))
colnames(polyunsaturated.fatty.acids.mat)<-column.names
for(i in 1:length(as.data.frame(polyunsaturated.fatty.acids.doc)[,3])) {
  polyunsaturated.fatty.acids.mat[i,]<-c(polyunsaturated.fatty.acids.doc$report$food$ndbno[[i]],
                    polyunsaturated.fatty.acids.doc$report$foods$nutrients[[i]]$gm)
}
# total lipid summaries
column.names<-c("ndbno",total.lipids.doc$report$foods$nutrients[[1]]$nutrient)
total.lipids.mat<-matrix(nrow=length(as.data.frame(total.lipids.doc)[,3]),ncol=(1+length(total.lipids.doc$report$foods$nutrients[[1]]$nutrient)))
colnames(total.lipids.mat)<-column.names
for(i in 1:length(as.data.frame(total.lipids.doc)[,3])) {
  total.lipids.mat[i,]<-c(total.lipids.doc$report$food$ndbno[[i]],
                    total.lipids.doc$report$foods$nutrients[[i]]$gm)
}
#carbohydrates
column.names<-c("ndbno",carbs.doc$report$foods$nutrients[[1]]$nutrient)
carbs.mat<-matrix(nrow=length(as.data.frame(carbs.doc)[,3]),ncol=(1+length(carbs.doc$report$foods$nutrients[[1]]$nutrient)))
colnames(carbs.mat)<-column.names
for(i in 1:length(as.data.frame(carbs.doc)[,3])) {
  carbs.mat[i,]<-c(carbs.doc$report$food$ndbno[[i]],
                    carbs.doc$report$foods$nutrients[[i]]$gm)
}
# water ash and others
column.names<-c("ndbno",water.ash.others.doc$report$foods$nutrients[[1]]$nutrient)
water.ash.others.mat<-matrix(nrow=length(as.data.frame(water.ash.others.doc)[,3]),ncol=(1+length(water.ash.others.doc$report$foods$nutrients[[1]]$nutrient)))
colnames(water.ash.others.mat)<-column.names
for(i in 1:length(as.data.frame(water.ash.others.doc)[,3])) {
  water.ash.others.mat[i,]<-c(water.ash.others.doc$report$food$ndbno[[i]],
                    water.ash.others.doc$report$foods$nutrients[[i]]$gm)
}
# minerals and elements
column.names<-c("ndbno",minerals.elements.doc$report$foods$nutrients[[1]]$nutrient)
minerals.elements.mat<-matrix(nrow=length(as.data.frame(minerals.elements.doc)[,3]),ncol=(1+length(minerals.elements.doc$report$foods$nutrients[[1]]$nutrient)))
colnames(minerals.elements.mat)<-column.names
for(i in 1:length(as.data.frame(minerals.elements.doc)[,3])) {
  minerals.elements.mat[i,]<-c(minerals.elements.doc$report$food$ndbno[[i]],
                    minerals.elements.doc$report$foods$nutrients[[i]]$gm)
}
# vitamins
column.names<-c("ndbno",vitamins.doc$report$foods$nutrients[[1]]$nutrient)
vitamins.mat<-matrix(nrow=length(as.data.frame(vitamins.doc)[,3]),ncol=(1+length(vitamins.doc$report$foods$nutrients[[1]]$nutrient)))
colnames(vitamins.mat)<-column.names
for(i in 1:length(as.data.frame(vitamins.doc)[,3])) {
  vitamins.mat[i,]<-c(vitamins.doc$report$food$ndbno[[i]],
                    vitamins.doc$report$foods$nutrients[[i]]$gm)
}
# sterols, flavenoids, etc
column.names<-c("ndbno",sterols.flavonoids.doc$report$foods$nutrients[[1]]$nutrient)
sterols.flavonoids.mat<-matrix(nrow=length(as.data.frame(sterols.flavonoids.doc)[,3]),ncol=(1+length(sterols.flavonoids.doc$report$foods$nutrients[[1]]$nutrient)))
colnames(sterols.flavonoids.mat)<-column.names
for(i in 1:length(as.data.frame(sterols.flavonoids.doc)[,3])) {
  sterols.flavonoids.mat[i,]<-c(sterols.flavonoids.doc$report$food$ndbno[[i]],
                    sterols.flavonoids.doc$report$foods$nutrients[[i]]$gm)
}

#merge together all tables based on NDB number starting with the names matrix
first <- merge(names.for.merge,energy.mat,by="ndbno",all=T)
incl.protein <- merge(first,protein.aminos.mat,by="ndbno",all=T) #for some reason protein API only has 2 species... something wrong with nutrient values here
incl.satfats <- merge(incl.protein,saturated.fatty.acids.mat,by="ndbno",all=T)
incl.monounsatfats <- merge(incl.satfats,monounsaturated.fatty.acids.mat,by="ndbno",all=T)
incl.polyunsatfats <- merge(incl.monounsatfats,polyunsaturated.fatty.acids.mat,by="ndbno",all=T)
incl.lipid.sums <- merge(incl.polyunsatfats,total.lipids.mat,by="ndbno",all=T)
incl.carbs <- merge(incl.lipid.sums,carbs.mat,by="ndbno",all=T)
incl.water.ash <- merge(incl.carbs,water.ash.others.mat,by="ndbno",all=T)
incl.minerals.elements <- merge(incl.water.ash,minerals.elements.mat,by="ndbno",all=T)
incl.vitamins <- merge(incl.minerals.elements,vitamins.mat,by="ndbno",all=T)
incl.sterols.flavs <- merge(incl.vitamins,sterols.flavonoids.mat,by="ndbno",all=T)

#Cleaned all USDA fish/shellfish nutrition data
all.usda.relevant.data <- cbind("USA",incl.sterols.flavs)
# extract specific product types
raw.usda.data <- usda.relevant.data[grep(", raw", usda.relevant.data$food.name), ]
canned.usda.data <- usda.relevant.data[grep(", canned", usda.relevant.data$food.name), ]
cooked.usda.data <- usda.relevant.data[grep(", cooked", usda.relevant.data$food.name), ]
baked.usda.data <- usda.relevant.data[grep(", baked", usda.relevant.data$food.name), ]

# and store
write.csv(all.usda.relevant.data,
          here("OutputsFromR","raw.usda.data.csv")
          )



