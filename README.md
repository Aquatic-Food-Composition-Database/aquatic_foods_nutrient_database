# **NutritionDatabase**

Welcome to the Aquatic Foods Composition Database! This repository synthesizes existing nutrient composition data for aquatic food species. These data originate from disparate sources, including national food composition tables (FCT), international datasets from FAO, and other peer reviewed published sources of nutrient composition. New FCT are dynamically added to this comprehensive database. Authorship for this database includes Christopher D. Golden, J. Zachary Koehn, Bapu Vaitla, Camille DeSisto, Kathryn J. Fiorella, and Shakuntala H. Thilsted. 

## **Data**
All raw data and outputs are included in data folder.


## **Scripts**
Scripts included in this repository have different functional roles, including: extracting data from the raw FCT data, cleaning of FCT data, merging of FCT data and results of the literature review, adding taxonomic information, and finally cleaning/finalizing the overall database. 


### *Extracting and cleaning FCT data*

These scripts extracted and/or cleaned the FCT data from national and internatioanl databases as needed. Databases included here include all FCT databases that were available online in machine-readeable formats (e.g., .csv). We intend to continue to build on this set as more databases become available. Note that a number of FCTs were previously compiled as part of a previous project (cite Chris??), so the scripts and descriptions included in the table below pertain only to the tables added as part of this project. 

| Script | Description |
| --- | --- |
| USDA.scrape.R | Uses an API to query online USDA database (no data required), and cleans that USDA data for merge with existing AFCD dataset. |
| add.INFOODSandUSDA.R | Cleans and adds FAO/INFOODS Global Food Composition Database for Fish and Shellfish and FAO/INFOODS Food Composition Database for Biodiversity  |
|  |  |
|  |  |
|  |  |
 

#### Input data
* None to create USDA dataframe

#### Output data
* "raw.usda.data.csv" : This information was bound MANUALLY with the original AFCD dataset "AFCD" into "binded.mod.AFCD.csv". As a result, running this script will not create the dataset combining the original dataset with USDA data, and this script can be ignored. To OutputsFromR subdirectory.



### *2) add.INFOODSandUSDA.R*
Takes "binded.mod.AFCD.csv" data and combines it with two FAO datsets:
1) FAO/INFOODS Global Food Composition Database for Fish and Shellfish 
2) FAO/INFOODS Food Composition Database for Biodiversity 

#### Input data
* "binded.mod.AFCD.csv" - in the OutputsFromR subdirectory
* "fao.infoods.ufish1.csv" - FAO INFOODS v1
* "fao.infoods.biodiv3.csv" - FAO BIODIV v3

#### Output data
* "AFCD.w.FAO.USDA.csv" - synthesis dataset with FAO and USDA nutrient info, to OutputsFromR subdirectory.


### *3) finalize.AFCD.R*
Cleans updated dataset now including FAO and USDA food composition information. This script does a quick clean of USDA amino acid data, which reports amino acid concentrations in grams instead of micrograms.

This script also links species in the dataset with taxonomic names from the World Register of Marine Species or [WoRMS](http://www.marinespecies.org/). Scientific names between the datasets do not match perfectly, so a fuzzy merge was developed to link class/order/family/AFCD taxonomical information to species names in the updated nutrition database. 

*Note:* It took a really long time for my machine to excecute the fuzzy merge, so I created a shortcut spreadsheet that basically "matched.worms.AFCD.csv". The chunk of script that executes the fuzzy merge is commented out to save computation time. Uncommenting it will run the fuzzy merge, and create another copy of "matched.worms.AFCD.csv".

#### Input data
* "AFCD.w.FAO.USDA.csv" - synthesis dataset with FAO and USDA nutrient info. In OutputsFromR subdirectory. 
* "WoRMS.6.2017.csv" - WoRMS taxonomic database downloaded in June 2017. More recent taxononomic classifications may be available. 

#### Output data
* "matched.worms.AFCD.csv" - yep, that output csv I created so I didn't need to re-run the fuzzy merge over and over. To OutputsFromR subdirectory. 
* "AFCD.final.csv" - the final, cleaned synthesis document. To OutputsFromR subdirectory. 


### *3) clean_fct_aus.R*
Script cleans [Australia's 2019 food composition database](https://www.foodstandards.gov.au/science/monitoringnutrients/afcd/Pages/default.aspx) and subsets for aquatic foods, appended to AFCD in another script.
#### Input data
1) "Release 1 - Food details file.xlsx" - details on scientific names, also includes food category to subset from the actual FCT
2) "Release 1 - Food nutrient database.xlsx" - food composition table, which will be subset for aquatic foods
#### Output data
* "clean_fct_aus.csv" - cleaned and subset aquatic foods dataset with column names corrected for appending to AFCD.

### *4) extract_clean_fct_india.R*
Extracts nutrition tables from the PDF of India's FCTs, and cleans them for appending to AFCD.
#### Input data
1) "India_ifct-doc.pdf" - PDF that includes FCT information in tables. 
#### Output data
* "clean_fct_india.csv" - cleaned and subset aquatic foods dataset with column names corrected for appending to AFCD.

