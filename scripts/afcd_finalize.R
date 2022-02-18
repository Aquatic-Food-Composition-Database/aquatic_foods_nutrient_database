# ===============================================================================
# Cleans merged AFCD database
# exports to CSV in live folder 

#
#
# Name: J. Zachary (Zach) Koehn
# Email: zkoehn@stanford.edu
# Date started: 08/17/2017
# Revised: 04/28/2021
# ===============================================================================


#_____________________________________________________________________________________________
# read data and load libraries directory defaults
# ____________________________________________________________________________________________
library(tidyverse);library(here)

afcd_dat <- read.csv(
  here::here("data","OutputsFromR","afcd_with_taxa.csv"),
  header = TRUE
  )


source(
  here::here("scripts","afcd_clean_categories.R")
)

#_____________________________________________________________________________________________
# simplify the categories for the part of the food analysed and the preparation of the food,
# using afcd_clean_categories.R
# ____________________________________________________________________________________________



afcd_dat_clean <- afcd_dat %>%
  mutate(
    parts_of_food = case_when(
      Parts %in%  muscle_tissue ~ "muscle_tissue",
      Parts %in%  whole ~ "whole",
      Parts %in% reproductive_tissue ~ "reproductive_tissue",
      Parts %in% body_wall ~ "body_wall",
      Parts %in% gills ~ "gills",
      Parts %in% unknown_part ~ "unknown_part",
      Parts %in% mantle ~ "mantle",
      Parts %in% blade ~ "blade",
      Parts %in% stipe ~ "stipe",
      Parts %in% holdfast ~ "holdfast",
      Parts %in% edible ~ "edible",
      Parts %in% raw ~ "raw",
      Parts %in% skin ~ "skin",
      Parts %in% liver ~ "liver",
      Parts %in% viscera ~ "viscera",
      Parts %in% roe ~ "roe",
      Parts %in% blubber ~ "blubber",
      Parts %in% frond ~ "frond",
      Parts %in% bone ~ "bone",
      Parts %in% flippers ~ "flippers",
      Parts %in% heart ~ "heart",
      Parts %in% gutted ~ "gutted",
      Parts %in% oil ~ "oil",
      Parts %in% larvae ~ "larvae",
      Parts %in% head ~ "head",
      Parts %in% kidney ~ "kidney",
      Parts %in% combination ~ "combination",
      Parts %in% gelatin ~ "gelatin"
    ),
    preparation_of_food = case_when(
      Preparation %in% frozen ~ "frozen",
      Preparation %in% raw ~ "raw",
      Preparation %in% freezedried~  "freeze_dried",
      Preparation %in% unknown_preparation ~ "unknown_preparation",
      Preparation %in% dried ~ "dried",
      Preparation %in% baked ~ "baked",
      Preparation %in% boiled_steamed ~ "boiled_steamed",
      Preparation %in% fried ~ "fried",
      Preparation %in% canned ~ "canned",
      Preparation %in% smoked ~ "smoked",
      Preparation %in% boiled_steamed ~ "steamed",
      Preparation %in% microwaved~ "microwaved",
      Preparation %in% cooked ~ "cooked",
      Preparation %in% grilled ~ "grilled",
      Preparation %in% acid_digestion ~ "acid_digestion",
      Preparation %in% salted ~ "salted",
      Preparation %in% aged ~ "aged",
      Preparation %in% curried ~ "curried",
      Preparation %in% combination ~ "combination"
    ),
    production_category = case_when(
      Wild.Farmed %in% c("w","wild","Wild","wild ") ~ "wild_capture",
      Wild.Farmed %in% c("Captive","darmed","f","farm","farmed","Farmed","farmed ") ~ "farmed",
      Wild.Farmed %in% c("","NA","unknown","Retail Bought ","micropropegated","mix") ~ "unknown",
      is.na(Wild.Farmed) ~ "unknown"
    ), 
    parts_of_food = ifelse(is.na(parts_of_food),"small sample",parts_of_food),
    preparation_of_food = ifelse(is.na(preparation_of_food),"small sample",preparation_of_food)
  ) %>%
  mutate(
    Study.ID.number=ifelse(is.na(Study.ID.number),Country.ISO3,Study.ID.number),
    Study.ID.number=str_replace_all(Study.ID.number,"BGD","Bangladesh_2013"),
    # Korea assumption here, that this is 8th and not 9th revision (published in 2018), because
    # this was part of the older dataset that I believe was used in the 2016 Nature paper
    Study.ID.number=str_replace_all(Study.ID.number,"KOR","Korea_8thRev_2011"), 
    # NZ assumption here, that this is 12th edition, published in 2016, the 11th was published in 2013
    Study.ID.number=str_replace_all(Study.ID.number,"NZL","NewZealand_12th_2016"), 
    #MOZ assumption this comes from the database in FAO-INFOODS, link here: http://www.fao.org/infoods/infoods/tables-and-databases/africa/en/
    Study.ID.number=str_replace_all(Study.ID.number,"MOZ","Mozambique_V2_2011"), 
    # ARG assumption is this comes from ARGENFOODS, linked from FAO-INFOODS: http://www.unlu.edu.ar/~argenfoods/Tablas/Tabla.htm
    Study.ID.number=str_replace_all(Study.ID.number,"ARG","Argentina_2010"), 
    # JPN assumption is this comes from a link in FAO-INFOODS, link here: https://www.mext.go.jp/en/policy/science_technology/policy/title01/detail01/sdetail01/sdetail01/1385122.htm
    Study.ID.number=str_replace_all(Study.ID.number,"JPN","Japan_7thRev_2015"), 
    # GMB assumption is this comes from a link in FAO-INFOODS, link to pdf here:http://www.fao.org/fileadmin/templates/food_composition/documents/pdf/Gambia04082011.pdf
    Study.ID.number=str_replace_all(Study.ID.number,"GMB","Gambia_2011"), 
    # CAN assumption is this comes from a link in FAO-INFOODS, link here: https://www.canada.ca/en/health-canada/services/food-nutrition/healthy-eating/nutrient-data/canadian-nutrient-file-2015-download-files.html
    Study.ID.number=str_replace_all(Study.ID.number,"CAN","Canada_CNF_2015"),
    # UK assumption, I believe this is from 2015 adn NOT the newest 2021: https://www.gov.uk/government/publications/composition-of-foods-integrated-dataset-cofid?utm_source=MW7+List+March+2015&utm_campaign=947c9d4b28-Newsletter_2_December_2013_FINAL12_13_2013&utm_medium=email&utm_term=0_3b8ecbdaea-947c9d4b28-95444717
    Study.ID.number=str_replace_all(Study.ID.number,"GBR","UK_2015"), 
    # Unclear what this one is!! guessing cambodia
    Study.ID.number=str_replace_all(Study.ID.number,"KHM","KHM"), 
    # Unclear what this one is!! guessing Chile but not in FAO-INFOODS
    Study.ID.number=str_replace_all(Study.ID.number,"CHL","Chile_2010"), 
    # Unclear what this one is!! guessing Finland but FAO-INFOODS only has a link to a website with no excel file 
    Study.ID.number=str_replace_all(Study.ID.number,"FIN","FIN"), 
    # Unclear what this one is!! guessing Poland but FAO-INFOODS only has a link to a website with no excel file 
    Study.ID.number=str_replace_all(Study.ID.number,"POL","POL"), 
    # Unclear what this one is!! guessing Russia but FAO-INFOODS only has a link to very old, undigitized, literature
    Study.ID.number=str_replace_all(Study.ID.number,"RUS","RUS"), 
    # Unclear what this one is!! guessing Greece but FAO-INFOODS only has a broken link, as well as the compilers
    Study.ID.number=str_replace_all(Study.ID.number,"GRC","GRC"),
    # Unclear what this one is!! guessing Peru but FAO-INFOODS only has a broken link, as well as the compilers
    Study.ID.number=str_replace_all(Study.ID.number,"PER","PER"),
    # Unclear what this one is!! guessing Israel but FAO-INFOODS only has a broken link, as well as the compilers
    Study.ID.number=str_replace_all(Study.ID.number,"ISL","ISL"),
    # Unclear what this one is!! guessing Malaysia, but the food codes and common product descriptions (where they exist) do not match up
    Study.ID.number=str_replace_all(Study.ID.number,"MYS","Malaysia_1997"),
    # Unclear what this one is!! guessing Italy, but the food codes and common product descriptions (where they exist) do not match up
    Study.ID.number=str_replace_all(Study.ID.number,"ITA","Italy_FCT_2009"), 
    # Unclear what this one is!! guessing Turkey, but the food codes and common product descriptions (where they exist) do not match up
    Study.ID.number=str_replace_all(Study.ID.number,"TUR","Turkey_FCT_2014"), 
    # Unclear what these are!!
    # Study.ID.number=str_replace_all(Study.ID.number,"",""),
    Study.ID.number=str_replace_all(Study.ID.number,"XXX","XXX"),
    edible.portion.coefficient=ifelse(Edible.portion.coefficient>1,Edible.portion.coefficient*0.01,Edible.portion.coefficient),
    Dry.Matter=as.numeric(Dry.Matter),
    Dry.matter=ifelse(is.na(Dry.matter)==TRUE,Dry.Matter,Dry.matter),
    Dry.matter=Dry.matter*0.01,
    Original.FCT.Food.Code=ifelse(Study.ID.number=="USA_USDA_2019",USDA.ndbno,Original.FCT.Food.Code)
  ) %>%
  filter(
    !Study.ID.number %in% c("KHM","ISL","PER","RUS","FIN","GRE","POL","XXX"), #removing from live version because we are unsure of the citation (very old data)
    !(Study.ID.number=="" & Original.FCT.Food.Code=="")
    ) %>%
  # fix error where NAs were imported as 
  mutate(
    Iodine = ifelse(Iodine==0 & Study.ID.number %in% c('Japan_7thRev_2015'),NA,Iodine),
    Aluminum = ifelse(is.na(Aluminum)==TRUE & is.na(Aluminium)==FALSE,Aluminium,Aluminum),
    Aspartic.acid = ifelse(is.na(Aspartic.acid)==TRUE & is.na(Aspartic.acid.1)==FALSE,Aspartic.acid.1,Aspartic.acid),
    Fatty.acids.not.identified = ifelse(is.na(Fatty.acids.not.identified)==TRUE,Other.fatty.acids.not.specifiied,Fatty.acids.not.identified),
    Fatty.acid.18.2.n6 = ifelse(is.na(Fatty.acid.18.2.n6)==TRUE,Fatty.acid.18.2.n6.1,Fatty.acid.18.2.n6),
    Glutamine.plus.Glutamic.Acid = ifelse( (is.na(Glutamine.plus.Glutamic.Acid) == TRUE & is.na(Glutamine.gluamic.acid)==FALSE),Glutamine.gluamic.acid,Glutamine.plus.Glutamic.Acid),
    Glutamine.plus.Glutamic.Acid = ifelse( (Study.ID.number=="Bangladesh_2013" & is.na(Glutamine.plus.Glutamic.Acid) == FALSE ),NA,Glutamine.plus.Glutamic.Acid),
    # Nitrogen nonprotein investigation: Set to NA: 378, 5,612 (%) 696 (unknown vaues) 81 (protein, not nitrogen)
    Nitrogen.nonprotein = ifelse( (Study.ID.number%in%c("378", "5","612","81") & is.na(Nitrogen.nonprotein) == FALSE ),NA,Nitrogen.nonprotein),
    #  Nitrogen non-protein values that need unit conversions: Keep: Biodiv in (mg/100g) ,47 (% per 100g)... already good: 1,10,35,39,40,63 (g/100g
    Nitrogen.nonprotein = ifelse( (Study.ID.number%in%c("FAO_Biodiversity") & is.na(Nitrogen.nonprotein) == FALSE ),Nitrogen.nonprotein*1e-3,Nitrogen.nonprotein),
    Nitrogen.nonprotein = ifelse( (Study.ID.number%in%c("47") & is.na(Nitrogen.nonprotein) == FALSE ),Nitrogen.nonprotein*1e-2,Nitrogen.nonprotein),
    # Nitrogen total values to be removed b/c they have  uncertain values or units (266,64) or are % with no anchor points   (430,1017,1030,126,176,391,130, 461, 495)  
    Nitrogen.total = ifelse( (Study.ID.number%in%c("266","64","430","1017","1030","126","176","391","130", "461", "495") & is.na(Nitrogen.total) == FALSE ),NA,Nitrogen.total),
    # Nitrogen total values to be removed keep but convert to g/100g s: 385,361,378 (mg),
    Nitrogen.total = ifelse( (Study.ID.number%in%c("385","361","378") & is.na(Nitrogen.total) == FALSE ),Nitrogen.total*1e-3,Nitrogen.total),
    Neoxanthin = ifelse( (Study.ID.number%in%c("525","543") & is.na(Neoxanthin) == FALSE ),Neoxanthin*1e3,Neoxanthin),
    Violaxanthin = ifelse( (Study.ID.number%in%c("525","543") & is.na(Violaxanthin) == FALSE ),Violaxanthin*1e3,Violaxanthin),
    Sterols.total = ifelse( (Study.ID.number%in%c("268") & is.na(Ergosterol) == FALSE ),Ergosterol,Sterols.total),
    # Carotenoid total value needs to be converted to mg (already 100g)
    carotenoids.total = ifelse( (Study.ID.number%in%c("525","708","562","1031","672","349","376","750","736","611","364","52") & is.na(carotenoids.total) == FALSE ),carotenoids.total*1e3,carotenoids.total),
    # Carotenoid total value needs to be converted to mg per 100 g (currently mg/g)
    carotenoids.total = ifelse( (Study.ID.number%in%c("1217") & is.na(carotenoids.total) == FALSE ),carotenoids.total*1e2*1e3,carotenoids.total),
    Fatty.acid.20.1.n11 = ifelse( (is.na(Fatty.acid.20.1.n11) == TRUE & is.na(Fatty.acid.20.1.n9.fatty.acid.20.1.n11)==FALSE),Fatty.acid.20.1.n9.fatty.acid.20.1.n11,Fatty.acid.20.1.n11),
    Fatty.acid.20.1.n9 = ifelse( (is.na(Fatty.acid.20.1.n9) == TRUE & is.na(Fatty.acid.18.1.n11.fatty.acid.20.1.n9)==FALSE),Fatty.acid.18.1.n11.fatty.acid.20.1.n9,Fatty.acid.20.1.n9),
    Fatty.acid.22.1.n11 = ifelse( (is.na(Fatty.acid.22.1.n11) == TRUE & is.na(Fatty.acid.22.1.n9.fatty.acid.22.1.n11)==FALSE),Fatty.acid.22.1.n9.fatty.acid.22.1.n11,Fatty.acid.22.1.n11),
    Fatty.acid.conversion.factor = ifelse( (is.na(Fatty.acid.conversion.factor) == TRUE & is.na(Fatty.acid.conversion.factor.for.internal.use)==FALSE),Fatty.acid.conversion.factor.for.internal.use,Fatty.acid.conversion.factor),
    # saponin values come from a single study reporting values as % with no anchor point. 
    Saponins = ifelse( (Study.ID.number%in%c("30") & is.na(Saponins) == FALSE ),NA,Saponins),
    # Convert/keep: 732 (iu/100g (1IU=0.025mcg), 531, 218,932, 113, 914,906,970,46,459,4,465 (mg/100g - all peer review)
    Vitamin.D = ifelse((Study.ID.number%in%c("531","218","932","113","914","906","970","46","459","4","465") & is.na(Vitamin.D) == FALSE ),Vitamin.D*1000,Vitamin.D),
    Vitamin.D = ifelse((Study.ID.number%in%c("732") & is.na(Vitamin.D) == FALSE ),Vitamin.D*0.025,Vitamin.D),
    # now combine Vitamin.D with Vitamin.D.D2.D3., as these represent the same values
    Vitamin.D = ifelse( (is.na(Vitamin.D)==TRUE & is.na(Vitamin.D.D2.D3.)==FALSE) ,Vitamin.D.D2.D3.,Vitamin.D),
    XN = ifelse( (is.na(XN) == TRUE & is.na(Conversion.factor.to.calculate.total.protein.from.nitrogen)==FALSE),Conversion.factor.to.calculate.total.protein.from.nitrogen,XN),
    Original.FCT.Food.Code = ifelse( (is.na(Original.FCT.Food.Code)==TRUE & !is.na(Food.Item.ID)),Food.Item.ID,Original.FCT.Food.Code),
    Study.ID.number = ifelse(Study.ID.number == "" & str_detect(Original.FCT.Food.Code,"09_")==TRUE,"Bangladesh_2013",Study.ID.number),
    Study.ID.number = ifelse(Study.ID.number == "" & str_detect(Original.FCT.Food.Code,"J0*|H0*")==TRUE,"FAO_Pacific_2004",Study.ID.number)
    ) %>%
  # now remove columns that were either simplified or were not updated from the original dataset
  select(-c(
    Parts,Preparation,Wild.Farmed,Processing,Edible.portion.coefficient,Dry.Matter,#updated, so remove
    Class.worms,Order.worms,Family.worms,Genus.worms,species, #updated, so remove
    GBD.Macro,GBD.Sub, FishBase.SAU.Code,ISSCAAP,EDIBLE,FAO.Taxon.Code,Code:Latest.revision.in.version,
    Fatty.acid.18.1.n7.1,Fatty.acid.18.2.n6.1,Fatty.acid.conversion.factor, #remove these duplicated names
    Aluminium, #remove duplicated name with Aluminum
    Aspartic.acid.1, #remove duplicated name with Aspartic Acid
    Asparagine.aspartic.acid, ##remove duplicated name with Asperagine plus aspartic Acid
    Glutamine.gluamic.acid, # remove duplicated name with Glutamine plus glutamic acid (also mispelled :) )
    Vitamin.D.D2.D3., # remove duplciated name with Vitamin.D (summation)
    Ergosterol, #this variable was incorrectly named, should be total sterols... study 268, values total sterols and variable removed
    fatty.acids.total.n3.longchain.polyunsaturated.in.cis.configuration,fatty.acids.total.n3.polyunsaturated.in.cis.configuration,Other.fatty.acids.not.specifiied,# no numeric values (all NAs)
    Sugars.total.expression.unknown,# no numeric values (all NAs)
    starch.total.expression.unknown,# no numeric values (all NAs)
    betaCarotene.cis,# no numeric values (all NAs)
    X25hydroxycholecalciferol,# no numeric values (all NAs)
    Isohamnetin,# no numeric values (all NAs)
    Kaempferol,# no numeric values (all NAs)
    Quercetin,# no numeric values (all NAs)
    Sum.of.18.amino.acids.excluding.glutamine.and.asparagine.,# no numeric values (all NAs)
    RefID,# no values (all NAs)
    Conversion.factor.to.calculate.total.protein.from.nitrogen, #duplicated with Xn (protein nitrogen conversion factor)
    Amino.acids.total.essential.unknown.or.variable.which.AS.are.included.in.total,#unit variability was too high
    Amino.acids.total.nonessential, #unit variability was too high
    Amino.acids.total.precise.definition.not.specified, #unit variability was too high
    Fatty.acid.18.1.n7.fatty.acid.18.1.n9,Fatty.acid.22.1.n11.fatty.acid.22.1.n13,Fatty.acid.20.1.n11.fatty.acid.20.1.n13,
    Fatty.acid.20.1.n11.fatty.acid.20.1.n13,Fatty.acid.22.1.n11.fatty.acid.22.1.n13.1,
    Fatty.acid.20.1.n9.fatty.acid.20.1.n11, #this was incorrectly specified (should be C20.1n11) so has been moved to C20.1 n11 and removed here.
    Fatty.acid.18.1.n11.fatty.acid.20.1.n9, #this was incorrectly specified (should be C20.1n9) so has been moved to C20.1 n9 and removed here.
    Fatty.acid.22.1.n9.fatty.acid.22.1.n11, #this was incorrectly specified (should be C22.1n11) so has been moved to C22.1 n11 and removed here.
    USDA.ndbno,Food.Item.ID, #remove as this is now a part of Original.FCT.Food.Code
    FAO.3A_CODE,alt.scinames,Habitat,Component.name #in the original dataset, not needed here
    )) %>%
  select(taxa_name,kingdom:genus,taxa_db,taxa_id,parts_of_food:production_category,edible.portion.coefficient,Study.ID.number,peer_review,everything(.))

#_____________________________________________________________________________________________
# clean up names
# removing '.' in title, as this is a special chracter, 
# and then make all variable names proper
# ____________________________________________________________________________________________
afcd_names <- names(afcd_dat_clean)
afcd_names_clean <- str_replace_all(afcd_names,"\\.","_")
afcd_names_clean <- str_to_title(afcd_names_clean)

names(afcd_dat_clean) <- afcd_names_clean

# now determine values that were included but have no numeric values (all NA)
# and exclude those values below
afcd_vars_no_vals <- afcd_dat_clean %>%
  select(Ph_hydrogen_ion_concentration:Wax_total) %>%
  select_if(is.logical) %>%
  names()

afcd_dat_clean <- afcd_dat_clean %>%
  rename(
    Cholecalciferol_d3=Cholecalciferol_d3_,
    Ergocalciferol_d2=Ergocalciferol_d2_,
    Isoleucine=Isoleucin,
    retinol_13_cis=X13cis_retinol
    ) %>%
  select(-c(afcd_vars_no_vals))

# variables with 

afcd_cov <- afcd_dat_clean %>%
  select(Ph_hydrogen_ion_concentration:Wax_total) %>%
  pivot_longer(cols = Ph_hydrogen_ion_concentration:Wax_total,names_to = "nutrients",values_to = "values") %>%
  group_by(nutrients) %>%
  drop_na(values) %>%
  filter(values>0) %>%
  summarize(
    cv=sd(values)/mean(values),
    n=length(values),
    max=max(values),
    min=min(values),
    mean=mean(values),
    median=median(values)
  )




#for finding outliers/ incorrectly specified values
afcd_dat_clean %>%
  select(Study_id_number,Vitamin_d,Vitamin_d_d2_d3) %>%
  drop_na(Vitamin_d,Vitamin_d_d2_d3) %>%
  filter(Study_id_number=="USA_USDA_2019")


test <- 
  afcd_dat_clean %>%
  select(
    Study_id_number,
    Amino_acids_total_precise_definition_not_specified
    ) %>%
  pivot_longer(-c(
    Study_id_number)
    ,names_to = "nutrient",values_to = "values") %>%
  drop_na(values) %>%
  distinct() %>%
  group_by(Study_id_number,nutrient) %>%
  summarize(
    median=median(values),
    sd=sd(values),
    max=max(values),
    min=min(values),
    n=length(values)
    ) 
#_____________________________________________________________________________________________
# write to file
# ____________________________________________________________________________________________
write.csv(afcd_dat_clean,
          here("data","OutputsFromR","aquatic_food_composition_database","20220218_AFCD.csv"),
          row.names=FALSE
)
