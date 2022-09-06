# ===============================================================================
# Pulls from New USDA database and merge with existing data (Food Data Central)
#
#
# Name: J. Zachary (Zach) Koehn
# Email: zkoehn@gmail.com
# Date started: 08/31/2022
# Revised: 09/06/2022
# ===============================================================================


library(here)
library(jsonlite)
library(pbapply)

#author's API key
source(here("scripts","usda_api_key.R")) 
# others using this script will need to create one, labelling it
# "api.key" for use with this script. For more info on how to do this, see:
# https://fdc.nal.usda.gov/api-guide.html



# ________________________________________________________________________________________________
# Scrape USDA database for nutrient information to integrate with existing GENuS-FISH
# : broken down by metadata summary, macronutrients and micronutrients, fatty acids
# : because of limits on free use, had to break down the API calls, which was done by nutrient groups
# ________________________________________________________________________________________________

# gathered from all the food.csv's using manual search (in vegetables), only found in SR Legacy and FNDSS (codes included below for that one)
sr_legacy_seaweed_fdc_id <- c(167602,167603,168456,168457,168458,169280,170090,170091,170495,170496)

fdc_sr_legacy <- read.csv(
  file=here("data","usa","fdc_srlegacy_20190402","food.csv"),
  header=TRUE) %>%
  filter(
    food_category_id==15 | fdc_id %in% sr_legacy_seaweed_fdc_id
  )

fdc_foundation <- read.csv(
  file=here("data","usa","fdc_foundation_20220428","food.csv"),
  header=TRUE) %>%
  filter(
    food_category_id==15 | fdc_id %in% c(seaweed_fdc_id) 
  )


fish_and_shellfish <- c( 
  1098741:1099168, #main ingredient
  1099896:1099915, #sandwiches
  1100145:1100172 #soups
)
seaweeds <- c(1103376,1103574,1103575,1103576,1103689,1103724)

fdc_fndss <- read.csv(
  file=here("data","usa","fdc_fndss_20221030","food.csv"),
  header=TRUE) %>%
  filter(
    fdc_id %in% c(fish_and_shellfish,seaweeds) 
  )

fdc_all_dat <- rbind(fdc_sr_legacy,fdc_foundation,fdc_fndss) 
fdc_id_unique <- unique(fdc_all_dat$fdc_id)

# need to complete the calls in two steps due to API 
store_list <- list()
seq_hour_1 <- 1:1000
hour_1_request <- pblapply(seq_hour_1,function(x) {
  tryCatch (
    
    store_list<- append(store_list,usda_api_call(x)) ,
    error = function(e){
      message(paste("An error occurred for item", x, 404,":\n"), e)
      
    })
}
)

seq_hour_2 <- 1001:length(fdc_id_unique)
hour_2_request <- pblapply(seq_hour_2,function(x) {
  tryCatch (
    
    store_list<- append(store_list,usda_api_call(x)) ,
    error = function(e){
      message(paste("An error occurred for item", x, 404,":\n"), e)
      
      })
    }
  )

extract_nutrient_data_from_list <- function(list_obj) {
  # list_obj <- hour_1_request[[600]]
  if(is.null(list_obj)==TRUE) {
    df <- data.frame(
      fdc_id=NA,
      food_description=NA,
      scientific_name=NA,
      nutrient_name=NA,
      nutrient_unit=NA,
      nutrient_value=NA
      )
  } else {
    df <- data.frame(
      fdc_id=ifelse(is.null(list_obj$fdcId)==TRUE,"none",list_obj$fdcId),
      food_description=ifelse(is.null(list_obj$description)==TRUE,"none",list_obj$description),
      scientific_name=ifelse(is.null(list_obj$scientificName)==TRUE,"none",list_obj$scientificName),
      nutrient_name=if(is.null(list_obj$foodNutrients$nutrient.name)==FALSE){list_obj$foodNutrients$nutrient.name} else {"none"},
      nutrient_unit=if(is.null(list_obj$foodNutrients$nutrient.unitName)==FALSE){list_obj$foodNutrients$nutrient.unitName} else {"none"},
      nutrient_value=if(is.null(list_obj$foodNutrients$amount)==FALSE){list_obj$foodNutrients$amount} else {"none"}
      )
  }  
  return(df)
}

length(hour_1_request)


hour_1_simplified <- pblapply(1:length(hour_1_request), function(x) extract_nutrient_data_from_list(list_obj = hour_1_request[[x]]))
hour_1_df <- rbindlist(hour_1_simplified)
hour_2_simplified <- pblapply(1:length(hour_2_request), function(x) extract_nutrient_data_from_list(list_obj = hour_2_request[[x]]))
hour_2_df <- rbindlist(hour_2_simplified)

usda_raw_extract <- rbind(hour_1_df,hour_2_df) %>%
  mutate(
    nutrient_value=as.numeric(nutrient_value)
  )
write.csv(usda_raw_extract,file=here("data","usa","usda_api_extract.csv"),row.names = FALSE)

