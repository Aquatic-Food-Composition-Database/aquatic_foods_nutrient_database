




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
