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
  dplyr::select(-X)

fats <- read.csv(
  file.path(directory,"data","afcd_peer_review_data","Seafood nutrients","fatty_acids.csv"),
  header=TRUE
  ) %>%
  dplyr::select(-X)

minerals <- read.csv(
  file.path(directory,"data","afcd_peer_review_data","Seafood nutrients","minerals.csv"),
  header=TRUE
  ) %>%
  dplyr::select(-X,-X.1,-X.2)

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
all_nutrients_no_relatives <- all_nutrients_4 %>%
  dplyr::select(
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
  652, #"Nesrin Emre, Kazım Uysal, Yılmaz Emre, Mustafa Kavaso_lu, & Ozgur Akta_. (2018). Seasonal and Sexual Variations of Total Protein, Fat and Fatty Acid Composition of an Endemic Freshwater Fish Species (Capoeta antalyensis. Aquatic Sciences and Engineering, 33(1), 6–10. https://doi.org/10.18864/ASE201802",
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
  # experimental
  759, # Bell, J. G., Strachan, F., Roy, W. J., Matthew, C., McDonald, P., Barrows, F. T., & Sprague, M. (2016). Evaluation of barley protein concentrate and fish protein concentrate, made from trimmings, as sustainable ingredients in Atlantic salmon ( Salmo salar L.) feeds. Aquaculture Nutrition, 22(2), 326–334. https://doi.org/10.1111/anu.12250
  
  # And values that are relative only (percentage of weight with no consistent weight) or experimental
  130, # Baghel, R. S., Kumari, P., Reddy, C. R. K., & Jha, B. (2014). Growth, pigments, and biochemical composition of marine red alga Gracilaria crassa. Journal of Applied Phycology, 26(5), 2143‚Äì2150. https://doi.org/10.1007/s10811-014-0250-5
  495, # Jia, R.-B., Wu, J., Li, Z.-R., Ou, Z.-R., Zhu, Q., Sun, B., Lin, L., & Zhao, M. (2020). Comparison of physicochemical properties and antidiabetic effects of polysaccharides extracted from three seaweed species. International Journal of Biological Macromolecules, 149, 81‚Äì92. https://doi.org/10.1016/j.ijbiomac.2020.01.111
  543, # Tracey A. Beacham, Isobel S. Cole, Louisa S. DeDross, Sofia Raikova, Christopher J. Chuck, John Macdonald, Leopoldo Herrera, Tariq Ali, Ruth L. Airs, Andrew Landels, & Michael J. Allen. (2019). Analysis of Seaweeds from SouthWest England as a Biorefinery Feedstock. Applied Sciences, 9(4456). https://doi.org/:10.3390/app9204456
  101, # D Robledo, & Y Freile Pelegr√≠n. (1997). Chemical and mineral composition of six potentially edible seaweed species of Yucatan. Botanica Marina, 40, 301‚Äì306.
  16,  # Zhang, W., Xu, Y., Tahir, H. E., Zou, X., & Wang, P. (2017). Rapid and wide-range determination of Cd(II), Pb(II), Cu(II) and Hg(II) in fish tissues using light addressable potentiometric sensor. Food Chemistry, 221, 541‚Äì547. https://doi.org/10.1016/j.foodchem.2016.11.141
  398, # Hanan Hafez Omar, Batoul Mohamed Abdullatif, Molouk Mohamed El-Kazan, & Adel Mansour El-Gendy. (2013). Red Sea Water and Biochemical Composition of Seaweeds at Southern Coast of Jeddah, Saudi Arabia. Life Science Journal, 10(4), 1073‚Äì1080.
  391, # Karla J McDermid, Brooke Stuercke, & George H Balazs. (2007). Nutritional composition of marine plants in the diet of the green sea turtle (Chelonia mydas) in the Hawaiian Islands. Bulletin of Marine Science, (1), 55‚Äì71.
  1217, # Papaioannou, C. D., Sinanoglou, V. J., Strati, I. F., Proestos, C., Kyrana, V. R., & Lougovois, V. P. (2016). Impact of different preservation treatments on lipids of the smooth clam Callista chione. International Journal of Food Science & Technology, 51(2), 325–332. https://doi.org/10.1111/ijfs.12972
  1025, # García, J. R., Kalinowski, C. T. H., Izquierdo, M. S. L., & Robaina, L. E. R. (2010). Marine and freshwater crab meals in diets for red porgy (Pagrus pagrus): effect on growth, fish composition and skin colour.Aquaculture Research,41(12), 1759-1769.
  
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
  
  # and other outliers across calcium, vitamin A, iron, protein, b12, 
  89,  #Saher, N. U., & Kanwal, N. (2019). Assessment of some heavy metal accumulation and nutritional quality of shellfish with reference to human health and cancer risk assessment: A seafood safety approach. Environmental Science and Pollution Research, 26(5), 5189–5201. https://doi.org/10.1007/s11356-018-3764-6
  155, #Kress, N., Herut, B., Shefer, E., & Hornung, H. (1999). Trace element levels in fish from clean and polluted coastal marine sites in the Mediterranean Sea, Red Sea and North Sea. Helgoland Marine Research, 53(3‚Äì4), 163‚Äì170. https://doi.org/10.1007/s101520050022
  207, #Ratoe Oedjoe, M. D. (2017). Composition of Nutritional Content of Sea Cucumbers (Holothuroidea) in Mania Waters, Sabu Raijua Regency, East Nusa Tenggara. Journal of Aquaculture Research & Development, 08(07). https://doi.org/10.4172/2155-9546.1000502
  281, #Shalaby, S., El-Dakar, A., & Ghoncim, S. (2001). Protein- sparing effect by carbohydrate in diets of rabbitfish, Siganus rivulatus. Egyptian Journal of Aquatic Biology and Fisheries, 5(4), 87‚Äì98. https://doi.org/10.21608/ejabf.2001.1710
  298, #10.12692/ijb/4.10.115-122
  309, #Sohail, M., Khan, M. N., Chaudhry, A. S., & Qureshi, N. A. (2016). Bioaccumulation of heavy metals and analysis of mineral element alongside proximate composition in foot, gills and mantle of freshwater mussels (Anodonta anatina). Rendiconti Lincei, 27(4), 687‚Äì696. https://doi.org/10.1007/s12210-016-0551-5
  392, #Thomas Rosemary, Abimannan Arulkumar, Sadayan Paramasivam, Alicia Mondragon-Portocarrero, & Jose Manuel Miranda. (2019). Biochemical, Micronutrient and Physicochemical Properties of the Dried Red Seaweeds Gracilaria edulis and Gracilaria corticata. Molecules, 24. https://doi.org/doi:10.3390/molecules24122225
  421, #Sakthivel, D., Vijayakumar, N., & Anandan, V. (2014). Biochemical composition of mangrove crab Sesarma brockii from Pondicherry Southeast of India. 3(3), 16.
  507, #Aikaterini Kandyliari, Athanasios Mallouchos, Nikos Papandroulakis, Jaya Prakash Golla, TuKiet T. Lam, Aikaterini Sakellari, Sotirios Karavoltsos, Vasilis Vasiliou, & Maria Kapsokefalou. (2020). Nutrient Composition and Fatty Acid and Protein Profiles of Selected Fish By-Products. Foods, 9(190). https://doi.org/doi:10.3390/foods9020190
  591, #Vivek Sharma, Abhishek Srivastava, and Vinod Kumar Vashistha. “The Effect of Ultraviolet Light Exposure on Proximate Composition, Amino Acid, Fatty Acid and Micronutrients of Cold Water Fish, ‘Barilius Vagra.’” Oriental Journal of Chemistry 35, no. 3 (2019): 1220–26. https://doi.org/10.13005/ojc/350344.
  607, #Mahmoud A. Dar, Aisha A. Belal, & Amany G. Madkour. (2018). The differential abilities of some molluscs to accumulate heavy metalswithin their shells in the Timsah and the Great Bitter lakes, Suez Canal,Egypt. Egyptian Journal of Aquatic Research, 44, 291–298. https://doi.org/10.1016/j.ejar.2018.11.008
  613, #Paiboon Panase, Seksan Uppapong, Siriluck Tuncharoena, Jakkaphan Tanitson, Kayanat Soontornprasit, & Payungsuk Intawicha. (2018). Partial replacement of commercial fish meal with Amazon sailfin catfish Pterygoplichthys pardalis meal in diets for juvenile Mekong giant catfish Pangasianodon gigas. Aquaculture Reports, 12, 25–29. https://doi.org/10.1016/j.aqrep.2018.08.005
  651, #Tawali, A. B., Muhammad Asfar, Meta Mahendradatta, & Suryani Tawali. (2018). Comparison of proximate composition, amino acid, vitamin and mineral contents of whole fish poweder and fish protein concentrate from local Indonesian snakehead fish (Channa striatus). Carpathian Journal of Food Science and Technology, 10(3), 40–46.
  688, #Sotoudeh, A., & Yeganeh, S. (2017). Effects of supplementary fennel ( Foeniculum vulgare ) essential oil in diet on growth and reproductive performance of the ornamental fish, Convict cichlid ( Cichlasoma nigrofasciatum ). Aquaculture Research, 48(8), 4284–4291. https://doi.org/10.1111/are.13249
  746, #Enric Gisbert, Mansour Torfi Mozanzadeh, Yannis Kotzamanis, & Alicia Esteevez. (2016). Weaning wildflathead grey mullet (Mugil cephalus) fry with diets withdifferent levels offish meal substitution. Aquaculture, 462, 92–100. https://doi.org/10.1016/j.aquaculture.2016.04.035
  748, #Sun, H., Tang, J., Yao, X., Wu, Y., Wang, X., & Liu, Y. (2016). Effects of replacement of fish meal with fermented cottonseed meal on growth performance, body composition and haemolymph indexes of Pacific white shrimp, Litopenaeus vannamei Boone, 1931. Aquaculture Research, 47(8), 2623–2632. https://doi.org/10.1111/are.12711
  774, #Tyokumbur, E. (2016). BIOACCUMULATION OF SOME METALS IN THE FISH SAROTHERODON GALILAEUS CAUGHT FROM ALARO STREAM IN IBADAN. International Journal of Pure and Applied Zoology, 4(3), 186–188.
  951, #Kennelly AC: A Nutrient Evaluation of Selected Nuxalk Salmon Preparations. In.; 1986.
  1024,#Company, J. B., & Sarda, F. (1998). Metabolic rates and energy content of deep-sea benthic decapod crustaceans in the western Mediterranean Sea. Deep Sea Research Part I: Oceanographic Research Papers, 45(11), 1861-1880.
  1209, #M.J. Turano, D.A. Davis, & C. R. Arnold. (2002). Optimization of growout diets for red drum, Sciaenops ocellatus. Aquaculture Nutrition, 8, 95–101.
  949, #massive values, cannot validate Ackman RG: Nutritional Composition of Fats in Seafoods. Progress in Food and Nutrition Science 1989, 13:161-241.
  
  # these values need to be double checked, especially the %relative fatty acid methyl ester based fat observations,
  # excluding for now
  1004, #feeding study, data also incorrectly entered into database :( 10.1016/j.aquaculture.2005.06.009
  499, # peer reviewer RAs included this incorrectly, this used % total fatty acid methyl esters.... (10.3390/ijerph17072545)
  270, #peer reviewer RAs used wrong data point (used relative value, not g/100g of edible in Table 2b) 10.1006/jfca.1996.0024 #FAME
  271, # peer reviewer RAs included this incorrectly, this used % total fatty acid methyl esters....
  292 #unfortunately this one was inputed incorrectly. these are RELATIVE omega 3 values, but they were labeled as absolute..
  
  )

studies_dry_weight <- c(
  13,
  50,
  105,
  132,
  134,
  147,
  208,
  250,
  251,
  252,
  256,
  261,
  266,
  274,
  278,
  284,
  302,
  312,
  321,
  322,
  371,
  410,
  457,
  461,
  531,
  612,
  619,
  651,
  652,
  655,
  739,
  749,
  750,
  798,
  810,
  918,
  924,
  970,
  973,
  1400
  )



all_nutrients_no_relatives_excluded <- all_nutrients_no_relatives %>%
  filter(
    !Study.ID.number %in% c(exclude_by_study_id)
    ) %>%
  mutate(
    Scientific.Name=as.character(Scientific.Name)
    )
dim(all_nutrients_no_relatives_excluded)


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
