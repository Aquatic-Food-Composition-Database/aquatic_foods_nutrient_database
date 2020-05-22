#============================================================
# Cleaning FAO's West Africa food composition table dataset
# for merge with AFCT

#Name: J. Zachary (Zach) Koehn
#Email: zkoehn@stanford.edu
#Date started: 05/05/2020
#Revised: 05/11/2020
#============================================================
# we want all rows below "Fish and its products" section


#__________________________________________
# read data and load libraries directory defaults
# _________________________________________
library(dplyr);library(here);library(readxl)
Sys.setlocale('LC_ALL','C') #sets language to eliminate multibyte error if it arrises

# source script of functions
source(here("scripts","functions","func_cleaning_fct.R"))
Sys.setlocale("LC_ALL", 'en_US.UTF-8')

# load raw data files
fao_west_africa_file <- "WAFCT_2019.xlsx"
merge_key_file <- "database_merge_key.xlsx"



merge_key <- read_excel(
  here("data",merge_key_file),
  sheet="key"
)

food_details_dat <- read_excel(
  here("data",fao_west_africa_file), 
  sheet="06 NV_stat_57 (per 100g EP)",
  col_names = FALSE
  ) 

# subset for just aquatic foods, which is a pretty simple slice using
# row numbers from the raw excel spreadsheet
# also need to extract the header column to place on top of the data frame.

# extract the rownames
# get the data structure right
food_details_dat[3,1:5] <- food_details_dat[1,1:5]
df_names <- as.vector(food_details_dat[3,])
df_names[9] <- "ENERC_kj"
df_names[10] <- "ENERC_kcal"

# subset the data just for aquatic foods, this is rows 2724:3021 in the raw excel spreadsheet
fao_wa_aquatic_foods_dat <- food_details_dat[2724:3021,]

# extract estimates.. along with standard deviation for each of the values

# first fil in the blank !?! rows witht he correct product information
fao_wa_aquatic_foods_dat[,2] <- fillNAgaps(fao_wa_aquatic_foods_dat[,2])


# re-assign names for column manipulation
colnames(fao_wa_aquatic_foods_dat) <- df_names

# we know there are values in Code column that are NOT important, so remove those
remove_code_values <- c("Non-African data","min","max","median","n")

fao_wa_aquatic_foods_dat <- fao_wa_aquatic_foods_dat %>%
  filter(!Code %in% remove_code_values)  # removes values we aren't interested in... leaving only estimate and standard deviation

fao_wa_aquatic_foods_dat[,6:dim(fao_wa_aquatic_foods_dat)[2]] <- sapply(
  fao_wa_aquatic_foods_dat[,6:dim(fao_wa_aquatic_foods_dat)[2]], 
  as.numeric
  )


# run user-written function that converts nutrient measurement units if needed

fao_wa_conversion_coefs <- coefs_convert_unit_fct(key=merge_key,
                                                original_unit="FAO_west_africa_unit",
                                                convert_to_unit="AFCD_unit",
                                                variables_to_convert="FAO_west_africa_variable_name") %>%
  slice(-1:-5) %>%
  filter(FAO_west_africa_variable_name != "XFA") %>% # and remove fatty acid conversion factor... no unit conversion needed.
  select(FAO_west_africa_variable_name,coefs) #just select the variable name and conversion coefficient


# now take all that information and create a named vector, with JUST the conversion coefficients named by the nutrient to be converted
coefs <- as.numeric(fao_wa_conversion_coefs$coefs)
fao_wa_names <- fao_wa_conversion_coefs$FAO_west_africa_variable_name
names(coefs) <- fao_wa_names #names it

# now figure out which variables match from the conversion vector to 
name_match <- intersect(names(fao_wa_aquatic_foods_dat), names(coefs))
fao_wa_aquatic_foods_dat[name_match] <- sweep(fao_wa_aquatic_foods_dat[name_match], 2, unlist(coefs[name_match]), `*`)




# finally, change names so that it can be readily merged with AFCD

# use function in "functions/func_cleaning_fct.r" create dataframe that 
# includes variable names to change from Aus to AFCD
fao_wa_names_to_convert_to_afcd <- convert_nutrient_names("FAO_west_africa_variable_name") 


fao_wa_aquatic_foods_dat <- fao_wa_aquatic_foods_dat %>%
  data.table::setnames(
    old=fao_wa_names_to_convert_to_afcd$original_dataset,
    new=fao_wa_names_to_convert_to_afcd$AFCD_variable_name)

fao_wa_aquatic_foods_dat$Country.ISO3 <- "FAO.infoods.west.africa" #adds classification for PNDB




# change names of all the values to align with names in AFCD


# separate all estimates and standard deviation values... and then append _sd to all the SD values

fao_wa_aquatic_foods_dat_clean <- fao_wa_aquatic_foods_dat %>%
  mutate(Code = ifelse(Code!="SD","EST","SD")) %>%
  rename("FAO.WA.Sample.Statistic" = "Code")



# save the modified data frames to the folder
write.csv(fao_wa_aquatic_foods_dat_clean,here("data","OutputsFromR","cleaned_fcts","clean_fct_fao_west_africa.csv"),row.names = FALSE)


  
