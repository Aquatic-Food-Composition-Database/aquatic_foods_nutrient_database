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
macro <- read.csv(
  file.path(directory,"data","afcd_peer_review_data","Seafood nutrients","macro nutrients.csv"),
  header=TRUE
) %>%
  dplyr::select(-X,-X.1,-X.2)

amino <- read.csv(
  file.path(directory,"data","afcd_peer_review_data","Seafood nutrients","amino acids.csv"),
  header=TRUE
  ) %>%
  rename(Study.ID.number=X...Study.ID.number) %>%
  dplyr::select(-X)

fats <- read.csv(
  file.path(directory,"data","afcd_peer_review_data","Seafood nutrients","fatty_acids.csv"),
  header=TRUE
  ) %>%
  rename(Study.ID.number=X...Study.ID.number) %>%
  dplyr::select(-X)

minerals <- read.csv(
  file.path(directory,"data","afcd_peer_review_data","Seafood nutrients","minerals.csv"),
  header=TRUE
  ) %>%
  rename(Study.ID.number=X...Study.ID.number) %>%
  dplyr::select(-X,-X.1,-X.2)

misc <- read.csv(
  file.path(directory,"data","afcd_peer_review_data","Seafood nutrients","misc.csv"),
  header=TRUE
  ) %>%
  rename(Study.ID.number=X...Study.ID.number)
vitamin <- read.csv(
  file.path(directory,"data","afcd_peer_review_data","Seafood nutrients","vitamins.csv"),
  header=TRUE
  ) %>%
  rename(Study.ID.number=X...Study.ID.number)

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
  111,  #"B Chrisolite, S A Shanmugam, & S siva Senthil Arumugam. (2015). Proximate and mineral composition of fifteen freshwater fishes of Thoothukudi, Tamil NaduI. Journal of Aquaculture in the Tropics, 30(1‚Äì2), 33‚Äì43."
  
  # and a subset based on the findings of Marian on Omega 3 outliers
  609, # Hill, J. C., Alam, M. S., Watanabe, W. O., Carroll, P. M., Seaton, P. J., & Bourdelais, A. J. (2019). Replacement of Menhaden Fish Meal by Poultry By-Product Meal in the Diet of Juvenile Red Porgy. North American Journal of Aquaculture, 81(1), 81–93. https://doi.org/10.1002/naaq.10074
  48,  # Xu, J., Yan, B., Teng, Y., Lou, G., & Lu, Z. (2010). Analysis of nutrient composition and fatty acid profiles of Japanese sea bass Lateolabrax japonicus (Cuvier) reared in seawater and freshwater. Journal of Food Composition and Analysis, 23(5), 401‚Äì405. https://doi.org/10.1016/j.jfca.2010.01.010
  692, #X.F. Liang, L. Hu, X. F. Wu, Y. C. Qin, Y. H. Zheng, D. D. Shi, M. Xue, & X. F. Liang. (2017). Substitution of fish meal by fermented soybean meal affects the growth performance and flesh quality of Japanese seabass (Lateolabrax japonicus). Animal Feed Science and Technology, 229, 1–12. https://doi.org/10.1016/j.anifeedsci.2017.03.006
  413, #10.1016/j.jfca.2013.04.002
  526, #Toyes-Vargas, E. A., Parrish, C. C., Viana, M. T., Carre√≥n-Palau, L., Magall√≥n-Serv√≠n, P., & Magall√≥n-Barajas, F. J. (2020). Replacement of fish oil with camelina (Camelina sativa) oil in diets for juvenile tilapia (var. GIFT Oreochromis niloticus) and its effect on growth, feed utilization and muscle lipid composition. Aquaculture, 523, 735177. https://doi.org/10.1016/j.aquaculture.2020.735177
  586, #Soller, F., Roy, L. A., & Davis, D. A. (2019). Replacement of fish oil in plant-based diets for Pacific white shrimp, Litopenaeus vannamei, by stearine fish oil and palm oil. Journal of the World Aquaculture Society, 50(1), 186‚Äì203. https://doi.org/10.1111/jwas.12571
  663, #Matthew R. Dawson, Md Shah Alam, Wade O. Watanabe, & Patrick M. Carroll. (2018). Evaluation of Poultry By-Product Meal as an Alternative to Fish Meal in the Diet of Juvenile Black Sea Bass Reared in a Recirculating Aquaculture System. North American Journal of Aquaculture, 80, 74–87. https://doi.org/10.1002/naaq.10009
  516, #Madibana, M. J., Mlambo, V., Lewis, B. R., & Uys, L. (2020). Dietary seaweed ( Ulva sp.) does not alter fatty acid profiles and concentration in South African juvenile dusky kob ( Argyrosomus japonicus, Sciaenidae) fillet. Journal of Applied Animal Research, 48(1), 7‚Äì13. https://doi.org/10.1080/09712119.2020.1715223
  487, #Lu, J., Jin, M., Luo, J., Hou, Y., & Zhou, Q. (2020). Effects of dietary fish oil substitution with blending vegetable oils on growth performance, antioxidant enzyme activities and tissue fatty acid composition of juvenile swimming crab, Portunus trituberculatus. Aquaculture Nutrition, anu.13062. https://doi.org/10.1111/anu.13062
  617, #He, Y., Lin, G., Rao, X., Chen, L., Jian, H., Wang, M., Guo, Z., & Chen, B. (2018). Microalga Isochrysis galbana in feed for Trachinotus ovatus: Effect on growth performance and fatty acid composition of fish fillet and liver. Aquaculture International, 26(5), 1261–1280. https://doi.org/10.1007/s10499-018-0282-y
  731, #Grigorakis, K., Alexi, N., Vasilaki, A., Giogios, I., & Fountoulaki, E. (2016). Chemical quality and sensory profile of the Mediterranean farmed fish shi drum ( Umbrina cirrosa ) as affected by its dietary protein/fat levels. Italian Journal of Animal Science, 15(4), 681–688. https://doi.org/10.1080/1828051X.2016.1222890
  584, #Gedi, M. A., Magee, K. J., Darwish, R., Eakpetch, P., Young, I., & Gray, D. A. (2019). Impact of the partial replacement of fish meal with a chloroplast rich fraction on the growth and selected nutrient profile of zebrafish ( Danio rerio ). Food & Function, 10(2), 733‚Äì745. https://doi.org/10.1039/C8FO02109K

  #unfortunately this one was described correctly. these are RELATIVE omega 3 values, but they were labeled as absolute..
  292
  ) 

all_nutrients_no_relatives_excluded <- all_nutrients_no_relatives %>%
  filter(
    !Study.ID.number %in% exclude_by_study_id
    ) %>%
  mutate(Scientific.Name=as.character(Scientific.Name))

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
