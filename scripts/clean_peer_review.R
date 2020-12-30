##################
# Cleans data collected from peer review studies for merge with AFCD
# 
# creator: Zach Koehn
# email: zkoehn@stanford.edu
# date started: 07/14/2020
# last date modified: 07/16/2020
##################


##################
# load needed packages and data sets
##################
library(tidyverse);library(here);library(data.table)

directory <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Nutrition/Separate/aquatic_foods_nutrient_database"


# load nutrient information from Camille's datasets in our drive folder
# note this uses the here package, which locates the files based on whatever folder the Rproject is stored in
macro <- read.csv(
  file.path(directory,"data","afcd_peer_review_data","Seafood nutrients","macro nutrients.csv"),
  header=TRUE
)
amino <- read.csv(
  file.path(directory,"data","afcd_peer_review_data","Seafood nutrients","amino acids.csv"),
  header=TRUE
  )
fats <- read.csv(
  file.path(directory,"data","afcd_peer_review_data","Seafood nutrients","fatty_acids.csv"),
  header=TRUE
) %>%
  dplyr::select(-X)
minerals <- read.csv(
  file.path(directory,"data","afcd_peer_review_data","Seafood nutrients","minerals.csv"),
  header=TRUE
)
misc <- read.csv(
  file.path(directory,"data","afcd_peer_review_data","Seafood nutrients","misc.csv"),
  header=TRUE
)
vitamin <- read.csv(
  file.path(directory,"data","afcd_peer_review_data","Seafood nutrients","vitamins.csv"),
  header=TRUE
)


##################
# before merging, determine the metadata that we can use that to merge the data
##################

name_intersect <- Reduce(intersect,
                         tibble::lst(
                           names(macro),
                           names(amino),
                           names(fats),
                           names(minerals),
                           names(misc),
                           names(vitamin)
                         )
)



##################
# merge datasets together
# of note, the first 26 rows in each of these should be the same across all datasets
##################



# now bind all the nutrient data frames together 
# note this also ensures that all names used in the merge are of the same type
# fats <- fats %>% mutate(across(all_of(name_intersect),as.character))

all_nutrients <- full_join(
  macro %>% mutate(across(all_of(name_intersect),as.character)),
  amino %>% mutate(across(all_of(name_intersect),as.character)),
  by=name_intersect) %>% distinct()
all_nutrients_1 <- full_join(
  all_nutrients,
  vitamin %>% mutate(across(all_of(name_intersect),as.character)),
  by=name_intersect) %>% distinct()
all_nutrients_2 <- full_join(
  all_nutrients_1,
  minerals %>% mutate(across(all_of(name_intersect),as.character)),
  by=name_intersect) %>% distinct()
all_nutrients_3 <- full_join(
  all_nutrients_2,
  fats %>% mutate(
    across(all_of(name_intersect),as.character),
    Edible.portion.coefficient_est = as.character(Edible.portion.coefficient_est)
    ),
  by=name_intersect) %>% distinct()
all_nutrients_4 <- full_join(
  all_nutrients_3,
  misc %>% mutate(across(all_of(name_intersect),as.character)),
  by=name_intersect) %>% distinct()

##################
# for now REMOVE relative values, those are cleaned & calculated in "clean_pere_review_relative_values.R"
##################

names(all_nutrients_4)[1] <- "Study.ID.number"

all_nutrients_no_relatives <- all_nutrients_4 %>%
  select(
    -str_subset(names(all_nutrients_4),"PAA."), #for now removes 
    -str_subset(names(all_nutrients_4),"PP."),
    -str_subset(names(all_nutrients_4),"PFA."),
    -str_subset(names(all_nutrients_4),"X."), #old artifact of merging (false rownames)
    -str_subset(names(all_nutrients_4),"_sd"), #removes the standard deviation 
    -Fatty.acid.22.1.n9.fatty.acid.22.1.n11 #duplicated
  ) %>%
  rename_all(funs(
    stringr::str_replace_all( ., "_est", "" )
  ))

##################
# Exclude studies based on Marian Kjellevold's quality control assessment 
##################

  # where DOI exists, exclude by that 
exclude_by_study_id <- c(
  519, #"Shimaa A. Amer, Ali Osman, Naif A. Al-Gabri, Shafika A. M. Elsayed, Ghada I. Abd El-Rahman, Mohamed Tharwat Elabbasy, Shaimaa A. A. Ahmed, & Rowida E. Ibrahim. (2019). The Effect of Dietary Replacement of Fish Meal with Whey Protein Concentrate on the Growth Performance, Fish Health, and Immune Status of Nile Tilapia Fingerlings, Oreochromis niloticus. Animals, 9(1103). https://doi.org/10.3390/ani9121003",
  652, #"Nesrin Emre, Kazım Uysal, Yılmaz Emre, Mustafa Kavaso_lu, & Özgür Akta_. (2018). Seasonal and Sexual Variations of Total Protein, Fat and Fatty Acid Composition of an Endemic Freshwater Fish Species (Capoeta antalyensis. Aquatic Sciences and Engineering, 33(1), 6–10. https://doi.org/10.18864/ASE201802",
  592, #Hawaibam Romharsha, & Chungkham Sarojnalini. (2019). Micro-nutrient Contents of Some Fresh Water Fish Species of Manipur, India. Oriental Journal of Chemistry, 35(4), 1426–1432. https://doi.org/10.13005/ojc/350425
  632, #"Hawaibam Romharsha, & Chungkham Sarojnalini. (2018). Proximate Composition, Total Amino Acids and Essential Mineral Elements of Some Cyprinid Fishes of Manipur, India. Current Research in Nutrition and Food Science, 6(1), 157–164. https://doi.org/10.12944/CRNFSJ.6.1.18",
  109, #"10.12692/ijb/6.5.333-342",
  42,  #"Mumba, P. P., & Jose, M. (2005). Nutrient composition of selected fresh and processed fish species from lake Malawi: A nutritional possibility for people living with HIV/AIDS. International Journal of Consumer Studies, 29(1), 72‚Äì77. https://doi.org/10.1111/j.1470-6431.2005.00377.x",
  573, #"Yu, J., Li, S., Chang, J., Niu, H., Hu, Z., & Han, Y. (2019). Effect of variation in the dietary ratio of linseed oil to fish oil on growth, body composition, tissues fatty acid composition, flesh nutritional value and immune indices in Manchurian trout, Brachymystax lenok. Aquaculture Nutrition, 25(2), 377‚Äì387. https://doi.org/10.1111/anu.12863", #feeding trial
  636, #B. Tang, X. Bu, X. Lian, Y. Zhang, I. Muhammad, Q. Zhou, H. Liu, & Y. Yang. (2018). Effect of replacing fish meal with meat and bone meal on growth, feed utilization and nitrogen and phosphorus excretion for juvenile Pseudobagrus ussuriensis. Aquaculture Nutrition, 24, 894–902. https://doi.org/10.1111/anu.12625
  647, #W. C. Cai, G. Z. Jiang, X. F. Li, C. X. Sun, H. F. Mi, S. Q. Liu, & W. B. Liu. (2018). Effects of complete fish meal replacement by rice protein concentrate with or without lysine supplement on growth performance, muscle development and flesh quality of blunt snout bream (Megalobrama amblycephala). Aquaculture Nutrition, 24, 481–491. https://doi.org/10.1111/anu.12581
  646, #J. S. Zhou, S. S. Liu, H. Ji, & H. B. Yu. (2018). Effect of replacing dietary fish meal with black soldier fly larvae meal on growth and fatty acid composition of Jian carp (Cyprinus carpio var. Jian). Aquaculture Nutrition, 24, 424–433. https://doi.org/10.1111/anu.12574
  756, #F.-J. Li, X. Lin, S.-M. Lin, W.-Y. Chen, & Y. Guan. (2016). Effects of dietary fish oil substitution with linseed oil on growth, muscle fatty acid and metabolism of tilapia (Oreochromis niloticus). Aquaculture Nutrition, 22, 499–508. https://doi.org/10.1111/anu.12270
  547, #Yu, J., Li, S., Niu, H., Chang, J., Hu, Z., & Han, Y. (2019). Influence of dietary linseed oil as substitution of fish oil on whole fish fatty acid composition, lipid metabolism and oxidative status of juvenile Manchurian trout, Brachymystax lenok. Scientific Reports, 9(1), 13846. https://doi.org/10.1038/s41598-019-50243-8
  282, #Di Beneditto, A. P. M., dos Santos, M. V. B., & Vidal, M. V. (2009). Comparison between the diet of two dolphins from south-eastern Brazil: Proximate-composition and caloric value of prey species. Journal of the Marine Biological Association of the United Kingdom, 89(5), 903‚Äì905. https://doi.org/10.1017/S0025315409001519
  419, #Hussain, B., Sultana, T., Sultana, S., Ahmed, Z., & Mahboob, S. (2018). Study on impact of habitat degradation on proximate composition and amino acid profile of Indian major carps from different habitats. Saudi Journal of Biological Sciences, 25(4), 755‚Äì759. https://doi.org/10.1016/j.sjbs.2018.02.004
  642, #El-Sayed M. Younis, Abdullah S. Al-Quffail, Nasser A. Al-Asgah, Abdel-Wahab A. Abdel-Warith, & Yousef S. Al-Hafedh. (2018). Effect of dietary fish meal replacement by red algae, Gracilaria arcuata, on growth performance and body composition of Nile tilapia Oreochromis niloticus. Saudi Journal of Biological Sciences, 25, 198–203. https://doi.org/10.1016/j.sjbs.2017.06.012
  579, #Hussain, B., Sultana, T., Sultana, S., Al-Ghanim, K. A., Al-Misned, F., & Mahboob, S. (2019). Influence of habitat degradation on the fatty acid profiles of fish, microalgae, and zoobenthos in a river ecosystem. Process Safety and Environmental Protection, 123, 24‚Äì32. https://doi.org/10.1016/j.psep.2018.12.024
  337, #10.1016/j.microc.2012.10.019DOI: 10.1016/j.microc.2012.10.019
  95,  #Jabeen, F., & Chaudhry, A. S. (2011). Chemical compositions and fatty acid profiles of three freshwater fish species. Food Chemistry, 125(3), 991‚Äì996. https://doi.org/10.1016/j.foodchem.2010.09.103
  529, #Grayson, J., & Dabrowski, K. (2020). Partial and total replacement of fish oil with fatty acid ethyl esters in the starter diets of rainbow trout (Oncorhynchus mykiss). Aquaculture, 522, 735018. https://doi.org/10.1016/j.aquaculture.2020.735018
  534, #Chen, Y., Sun, Z., Liang, Z., Xie, Y., Su, J., Luo, Q., Zhu, J., Liu, Q., Han, T., & Wang, A. (2020). Effects of dietary fish oil replacement by soybean oil and l-carnitine supplementation on growth performance, fatty acid composition, lipid metabolism and liver health of juvenile largemouth bass, Micropterus salmoides. Aquaculture, 516, 734596. https://doi.org/10.1016/j.aquaculture.2019.734596
  172, #"James J Childress, & Mary H Nygaard. (1973). The chemical composition of midwater fishes as a function of depth of occurrence off southern California. Deep-Sea Research, 20, 1093‚Äì1109.",
  108, #"Sarma, D., Tiwari, T., Das, P., Jha, G., & Das, P. (2011). Proximate and Mineral Composition of Indigenous Hill Stream Fishes of Uttarakhand. Indian Journal of Animal Nutrition, 28(2), 203‚Äì206.",
  599, #"Qiao Y, Mai K, and Ai Q. “Effects of Fish Meal Replaced by Maggot Culture on Growth Performance, Body Composition, and Antioxidant Responses of Hybrid Tilapia (Oreochromis Niloticus _ O. Aureus).” The Israeli Journal of Aquaculture 71 (2019).",
  102, #"Tenyang, N., Womeni, H. M., Linder, M., Tiencheu, B., Villeneuve, P., & Mbiapo, F. T. (2014). The chemical composition, fatty acid, amino acid profiles and mineral content of six fish species commercialized on the Wouri river coast in Cameroon. 10.",
  765, #"Jabeen, F., & Chaudhry, A. S. (2016). Nutritional composition of seven commercially important freshwater fish species and the use of cluster analysis as a tool for their classification. The Journal of Animal & Plant Sciences, 26(1), 282–290.",
  231, #"Narayan, B., Hathwar, S. C., & Hemavathi, D. (2012). Lipid class and fatty acid composition of meat and nonmeat components of selected seafoods. Indian Journal of Fish, 59(1), 133‚Äì139.",
  235, #"Ndome, C., Oriakpono, O., & Ogar, A. (2010). Proximate composition and nutritional values of some commonly consumed fishes from the Cross River Estuary. Journal Freshwater Biology, 19(1), 11‚Äì18.",
  99,  #"Elagba Haj Ali Mohamed. (2013). Proximate and mineral composition in muscle and head tissue of seven commercial species of the Nile fish from Sudan. Asian Journal of Science and Technology, 4(10), 62‚Äì65.",
  431, #"Beaubier, J., & Hipfner, J. M. (2013). Proximate composition and energy density of forage fish delivered to rhinoceros suklet Cerorhinca monocerata nestlings at triangle island, British Colombia. Marine Ornithology, 41, 35‚Äì39.",
  111  #"B Chrisolite, S A Shanmugam, & S siva Senthil Arumugam. (2015). Proximate and mineral composition of fifteen freshwater fishes of Thoothukudi, Tamil NaduI. Journal of Aquaculture in the Tropics, 30(1‚Äì2), 33‚Äì43."
  )

all_nutrients_no_relatives_excluded <- all_nutrients_no_relatives %>%
  filter(
    !Study.ID.number %in% exclude_by_study_id
    )

##################
# clean for merge with existing AFCD data
##################

write.csv(
  all_nutrients_no_relatives_excluded,
  file.path(directory,"data","OutputsFromR","cleaned_fcts",
       "clean_peer_review.csv"
       ),
  row.names = FALSE
)






