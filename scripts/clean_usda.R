# ===============================================================================
# Cleans  USDA data from Food Data Central (extracted by API)
#
#
# Name: J. Zachary (Zach) Koehn
# Email: zkoehn@gmail.com
# Date started: 09/06/2022

# ===============================================================================
library(tidyverse)
library(here)

raw_usda <- read_csv(file=here("data","usa","usda_api_extract.csv"))

usda_clean <- raw_usda %>%
  mutate(
    scientific_name= sub("\\(.*","\\",ndbno.scinames$scientific.name) #removes taxonimist identifier specified by "(Taxonomist surname)"
    scientific_name= sub(" L.","",ndbno.scinames$scientific.name)# remove taxonomist identifer
    scientific_name= sub(" Tilesius","",ndbno.scinames$scientific.name)# remove taxonomist identifier Tilesius
    scientific_name= sub(" Milne-Edwards","",ndbno.scinames$scientific.name)# remove taxonomist identifier Milne-Edwards
    scientific_name= sub(" Valenciennes","",ndbno.scinames$scientific.name)# remove taxonomist identifer
    scientific_name= sub(" L.","",ndbno.scinames$scientific.name)# remove taxonomist identifer
    scientific_name= sub(" Girard","",ndbno.scinames$scientific.name)# remove taxonomist identifer
    scientific_name= sub(" Richardson","",ndbno.scinames$scientific.name)# remove taxonomist identifer
    scientific_name= sub(" Goode and Bean","",ndbno.scinames$scientific.name)# remove taxonomist identifer
    scientific_name= sub(" Lamarck","",ndbno.scinames$scientific.name)# remove taxonomist identifer
    scientific_name= sub(" Rathbun","",ndbno.scinames$scientific.name)# remove taxonomist identifer
    scientific_name= sub(" Dana","",ndbno.scinames$scientific.name)# remove taxonomist identifer
    scientific_name= trimws(ndbno.scinames$scientific.name,which=c("left"))
    scientific_name= trimws(ndbno.scinames$scientific.name,which=c("right"))
  )
  # clean scientific names
  # remove names of taxonomists from scientific name for merge with GENuS-FISH
  # modify nutrient names for merge with AFCD

  unique(raw_usda)
  
  
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



