



#__________________________________________
# read data and load libraries directory defaults
# _________________________________________
library(dplyr);library(here);library(readxl)
Sys.setlocale('LC_ALL','C') #sets language to eliminate multibyte error if it arrises

# source script of functions
source(here("functions","func_cleaning_fct.R"))

# load raw data files
food_details_file <- "Release1_Food_details_file.xlsx"
food_database_file <- "Release1_Food_nutrient_database.xlsx"

# hoping this doesn't remain an issue, but can't use the here package to locate
# things in google drive because they force you into a folder called "My Drive" 
# that just doens't play nice with reading and writing files, so have to 
# specify it as "My Drive"... instead of just the pathname... see help file here
# https://support.google.com/drive/thread/8914333?hl=en

# each user must define their working directory 
# *where the NutrientDatabase folder is stored*
zach_drive_wd <- file.path("/Volumes/GoogleDrive/My Drive/BFA_Nutrition/Data_and_Script/NutritionDatabase")


food_details_dat <- read_excel(
  file.path(zach_drive_wd,"data","australia_database",food_details_file), 
  sheet="Release 1 - Food File")

food_database_dat <- read_excel(
  file.path(zach_drive_wd,"data","australia_database",food_database_file),
  sheet="Solid per 100g & liq per 100mL",
  col_names=FALSE,skip=2)



# __________________________________________
# First, subset a data frame the aquatic food product codes we care about
# using the food details dataset
# __________________________________________

aquatic_foods_classifications <- c( #vector of aquatic food types
  "Seaweeds",
  "Fin fish, fresh, frozen",
  "Fin fish, battered or crumbed",
  "Eel",
  "Molluscs, fresh, frozen",
  "Crustacea, fresh, frozen",
  "Packed fin fish",
  "Smoked fish"
  )

# subset details dataset for aquatic foods
aquatic_food_details_dat <- food_details_dat %>%
  rename(food_category='Classification Name') %>%
  filter(food_category %in% aquatic_foods_classifications)
  
names(aquatic_food_details_dat)
names(food_database_dat)

# __________________________________________
# Clean up nutrient database
# __________________________________________

# first, whenever a nutrient has multiple units of measurement the excel file has a merged value
# across the nutrient name, when this is read into R, the second of the merged cells is NA, so need to 
# fill in nutrient names for all second merged cells

food_database_dat[1,2:dim(food_database_dat)[2]] <- sapply(2:dim(food_database_dat)[2],
       function(x)
    if(is.na(food_database_dat[1,x])==TRUE) {food_database_dat[1,x] <- food_database_dat[1,x-1]} else {
      food_database_dat[1,x] <- food_database_dat[1,x]
    }
  )

food_database_dat[1,173:180]

# second we deal with the nutrients where there are multiple measurements. In all cases, the 
# first nutrient measurement is some ratio value (e.g., %) whereas the second is the gram/mg mass 
# measurement we want. So, for any nutrient with two measurements, 
# we can take the second measurement and drop the first. 
nutr_names <- as.character(food_database_dat[1,])

duplicated(nutr_names,incomparables=TRUE)[108:115]
#bit of weird logic, but this allows me to select ONLY columns with a) unique values and b) the second nutrient unit measurement (i.e. *not* the ratio measurement)
food_database_dat <- food_database_dat[,duplicated(nutr_names,incomparables=TRUE,fromLast = TRUE)==FALSE]



# create new names that include nutrients and units
new_colnames <-  sapply(1:dim(food_database_dat)[2], function(x) clean_colnames_aus_fct(dat=food_database_dat,x=x))
colnames(food_database_dat) <- new_colnames #set these new names as the column names
food_database_dat <- food_database_dat[-c(1,2),] #and remove the old units


# __________________________________________
# Merge food details with nutrient database
# __________________________________________

# use "Public Food Key" variable as key to merge, merge onto aquatic_food_details_dat
