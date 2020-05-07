




#__________________________________________
# cleans and combines the nutrient names
# and adds the unit for easier merge with AFCD
# Australia specific
# _________________________________________

clean_colnames_aus_fct <- function(dat,x) {
  # x=1
  # dat <- food_database_dat
  if(is.na(dat[2,x])==FALSE) {
    new_name <- paste0(dat[1,x]," (",dat[2,x],")")
  } else {
    new_name <- dat[1,x]
  }
  return(new_name)
}

#__________________________________________
# Creates a named vector used to change variable names from 
# original dataset to the naming structure in AFCD
# requires column names in data/database_merge_key.xlsx
# so if it is a new dataset, the columns must be added to this spreadsheet
# before merge can happen :) 
# _________________________________________
convert_nutrient_names <- function(orig_dataset_variable_name){
  # test
  # orig_dataset_variable_name <- "Aus_variable_name"
  
  print("see database nutrient merge key spreadsheet (data/database_merge_key.xlsx) for correct orig_dataset_variable_name")
  dat <- select(merge_key,AFCD_variable_name,all_of(orig_dataset_variable_name))  
  dat <- dat[is.na(dat[[orig_dataset_variable_name]])==FALSE,]
  names(dat) <- c("AFCD_variable_name","original_dataset")
  
  return(dat)
  
}


# now need to create a function that evaluates units for nutrient information
# it should maybe
# 1) search to see if they are the same, if so... do tnothing
# 2) if different, for that entire column of nutrient information, multiply by some conversion coefficient
# this could require a conversion table that has all the conversion coefficients, and matches based on the 
# combination of nutrients between the two datasets (mg != g so multiply by 1e-3)
