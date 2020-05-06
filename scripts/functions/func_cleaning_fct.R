




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
  print("see database nutrient merge key spreadsheet (data/database_merge_key.xlsx) for correct orig_dataset_variable_name")
  dat <- select(merge_key,AFCD_variable_name,all_of(orig_dataset_variable_name))  
  dat <- dat[is.na(dat[[orig_dataset_variable_name]])==FALSE,]
  
  return(
    paste0("'",dat[['AFCD_variable_name']],"'","=","'",dat[[orig_dataset_variable_name]],"'")
  )
  
}
