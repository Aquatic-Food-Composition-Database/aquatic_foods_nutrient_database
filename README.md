# **Aquatic Food Composition Database**

Welcome to the Aquatic Foods Composition Database! This repository synthesizes existing nutrient composition data for aquatic food species. These data originate from disparate sources, including national food composition tables (FCT), international datasets from FAO, and other peer reviewed published sources of nutrient composition. New FCT are dynamically added to this comprehensive database. Authorship for this database includes Christopher D. Golden, J. Zachary Koehn, Bapu Vaitla, Camille DeSisto, Kathryn J. Fiorella, and Shakuntala H. Thilsted. 

Scripts included in this repository have different functional roles, including: extracting data from the raw FCT data, cleaning of FCT data, merging of FCT data and results of the literature review, adding taxonomic information, and finally cleaning/finalizing the overall database. 


## *Extracting and cleaning FCT data*

These scripts extracted and/or cleaned the FCT data from national and internatioanl databases as needed. Databases included here include all FCT databases that were available online in machine-readeable formats (e.g., .csv). We intend to continue to build on this set as more databases become available. Note that a number of FCTs (data from Bangladesh, Canada, Mozambique, Japan, Argentina, Gambia, United Kingdom, Chile, Malaysia, Italy and Turkey) were previously compiled as part of a previous project (**Chris: what should we cite for the original 'GENuS-FISH' data, the 2016 nature paper??**), so the scripts and descriptions included in the table below pertain only to the tables added as part of this project. 

| Script | Description |
| --- | --- |
| USDA.scrape.R | Uses an API to query online USDA database (no data required), and cleans that USDA data for merge with existing AFCD dataset. |
| add.INFOODSandUSDA.R | Cleans and adds FAO/INFOODS Global Food Composition Database for Fish and Shellfish and FAO/INFOODS Food Composition Database for Biodiversity  |
| clean_fao_west_africa.R | Cleans FAO's West Africa Food Composition Table dataset |
| clean_latinfoods.R | Cleans data from the international LATINFOODS dataset |
| clean_fct_pndb.R | Cleans data from the Pacific Nutrient Database by Pacific Community, University of Wollongong and the Food and Agriculture Organization of the United Nations |
| clean_fct_aus | Script cleans [Australia's 2019 food composition database](https://www.foodstandards.gov.au/science/monitoringnutrients/afcd/Pages/default.aspx) and subsets for aquatic foods, appended to AFCD in another script. |
| extract_clean_fct_india | Extracts nutrition tables from the PDF of India's 2017 FCTs, and cleans them for appending to AFCD. |
| clean_fct_norway.R | Cleans data from the Norwegian Food Composition Database |
| clean_smiling_cambodia.R | Cleans Cambodia data from the EU-SMILING project |
| clean_smiling_indonesia.R | Cleans Indonesia data from the EU-SMILING project |
| clean_smiling_laos.R | Cleans Laos data from the EU-SMILING project |
| clean_smiling_thailand.R | Cleans Thailand data from the EU-SMILING project |
| clean_smiling_vietnam.R | Cleans Vietnam data from the EU-SMILING project |


## *Peer review data extraction and cleaning*
Describes how peer review data is pulled together and cleaned.

## *AFCD synthesis and cleanings*
Describes scripts that are used to merge FCT and peer review data. This section can also include an overview of parts/processing. 