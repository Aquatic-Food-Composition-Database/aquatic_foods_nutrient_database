#============================================================
# Functions for cleaning and merging food composition table 
# into AFCD

#Name: J. Zachary (Zach) Koehn
#Email: zkoehn@stanford.edu
#Date started: 05/04/2020
#Revised: 05/11/2020
#============================================================




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
  
  print("see database nutrient merge key spreadsheet 
        (data/database_merge_key.xlsx) for correct orig_dataset_variable_name")
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


convert_units <- function(ele,dat,original_unit,convert_to_unit) {
  # dat <- merge_key
  # original_unit <- "Aus_unit"
  # convert_to_unit <- "AFCD_unit"
  
  if( is.na(dat[ele,original_unit])==TRUE | is.na(dat[ele,convert_to_unit])==TRUE) {coef <- NA} else {
    if(dat[ele,original_unit]==dat[ele,convert_to_unit]) {coef <- 1}
    if(dat[ele,original_unit]=="g" & dat[ele,convert_to_unit]=="mg") {coef <- 1e3}
    if(dat[ele,original_unit]=="g" & dat[ele,convert_to_unit]=="ug") {coef <- 1e6}
    if(dat[ele,original_unit]=="mg" & dat[ele,convert_to_unit]=="g") {coef <- 1e-3}
    if(dat[ele,original_unit]=="mg" & dat[ele,convert_to_unit]=="ug") {coef <- 1e3}
    if(dat[ele,original_unit]=="ug" & dat[ele,convert_to_unit]=="g") {coef <- 1e-6}
    if(dat[ele,original_unit]=="ug" & dat[ele,convert_to_unit]=="mg") {coef <- 1e-3}  
  }
  
  return(coef)
}



# I think i need to run the sapply inside of another one that subsets to ONLY include variables in the original datasets
# that's fine, because we can just add in the variable names to this as well, that'll be needed for the column-wise 
# multiplication of coefficients when doing the actual conversion anyways. 
coefs_convert_unit_fct <- function(key,original_unit,convert_to_unit,variables_to_convert) {
  # 
  # key=merge_key
  # original_unit="jpn_unit"
  # convert_to_unit="AFCD_unit"
  # variables_to_convert="jpn_variable_name"

  # first remove any value where we don't have a 
  key <- key[is.na(key[,variables_to_convert])==FALSE,] # removes any nutrient value from the  
  multipliers <- sapply(1:dim(key)[1],function(x) 
    convert_units(ele=x,dat=key,
                  original_unit = original_unit,
                  convert_to_unit=convert_to_unit
    )
  )
  df <- cbind(
    key[,variables_to_convert],
    key[,original_unit],
    key[,convert_to_unit])
  
  df$coefs <- multipliers
  return(df)
  
}

# in some cases, teh FCTs have blank rows for statistics associated with each nutrient
# concentration estimate. This fills in those blank rows with the last product.

# comes from R cookbook http://www.cookbook-r.com/Manipulating_data/Filling_in_NAs_with_last_non-NA_value/
fillNAgaps <- function(x, firstBack=TRUE) {
  ## NA's in a vector or factor are replaced with last non-NA values
  ## If firstBack is TRUE, it will fill in leading NA's with the first
  ## non-NA value. If FALSE, it will not change leading NA's.
  
  # If it's a factor, store the level labels and convert to integer
  lvls <- NULL
  if (is.factor(x)) {
    lvls <- levels(x)
    x    <- as.integer(x)
  }
  
  goodIdx <- !is.na(x)
  
  # These are the non-NA values from x only
  # Add a leading NA or take the first good value, depending on firstBack   
  if (firstBack)   goodVals <- c(x[goodIdx][1], x[goodIdx])
  else             goodVals <- c(NA,            x[goodIdx])
  
  # Fill the indices of the output vector with the indices pulled from
  # these offsets of goodVals. Add 1 to avoid indexing to zero.
  fillIdx <- cumsum(goodIdx)+1
  
  x <- goodVals[fillIdx]
  
  # If it was originally a factor, convert it back
  if (!is.null(lvls)) {
    x <- factor(x, levels=seq_along(lvls), labels=lvls)
  }
  
  x
}







