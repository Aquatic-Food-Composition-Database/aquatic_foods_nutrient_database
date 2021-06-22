# **Aquatic Food Composition Database**

Welcome to the Aquatic Foods Composition Database! This repository synthesizes existing nutrient composition data for aquatic food species. These data originate from disparate sources, including national food composition tables (FCT), international datasets from FAO, and other peer reviewed published sources of nutrient composition. New FCT are dynamically added to this comprehensive database. Authorship for this database includes Christopher D. Golden, J. Zachary Koehn, Bapu Vaitla, Camille DeSisto, Kathryn J. Fiorella, Kathryn J. Fiorella, Marian Kjellevold and Shakuntala H. Thilsted. 

Scripts included in this repository have different functional roles, including: extracting data from the raw FCT data, cleaning of FCT data, merging of FCT data and results of the literature review, adding taxonomic information, and finally cleaning/finalizing the overall database. 

The resulting database can be found at in (this repository)[https://dataverse.harvard.edu/dataset.xhtml?persistentId=doi:10.7910/DVN/KI0NYM].

## *Extracting and cleaning FCT data*

These scripts extracted and/or cleaned the FCT data from national and internatioanl databases as needed. Databases included here include all FCT databases that were available online in machine-readeable formats (e.g., .csv). We intend to continue to build on this set as more databases become available. Note that a number of FCTs (data from Bangladesh, Canada, Mozambique, Japan, Argentina, Gambia, United Kingdom, Chile, Malaysia, Italy and Turkey) were previously compiled as part of a previous project (**Chris: what should we cite for the original 'GENuS-FISH' data, the 2016 nature paper??**), so the scripts and descriptions included in the table below pertain only to the tables added as part of this project. 

| Script | Description |
| --- | --- |
| USDA.scrape.R | Uses an API to query online USDA database (no data required), and cleans that USDA data for merge with existing AFCD dataset. |
| add.INFOODSandUSDA.R | Cleans and adds FAO/INFOODS Global Food Composition Database for Fish and Shellfish and FAO/INFOODS Food Composition Database for Biodiversity  |
| clean_fao_west_africa.R | Cleans FAO's West Africa Food Composition Table dataset |
| clean_latinfoods.R | Cleans data from the international LATINFOODS dataset |
| clean_fct_pndb.R | Cleans data from the Pacific Nutrient Database by Pacific Community, University of Wollongong and the Food and Agriculture Organization of the United Nations |
| clean_fct_aus | Script cleans Australia's 2019 food composition database and subsets for aquatic foods, appended to AFCD in another script. |
| extract_clean_fct_india | Extracts nutrition tables from the PDF of India's 2017 FCTs, and cleans them for appending to AFCD. |
| clean_fct_norway.R | Cleans data from the Norwegian Food Composition Database |
| clean_smiling_cambodia.R | Cleans Cambodia data from the EU-SMILING project |
| clean_smiling_indonesia.R | Cleans Indonesia data from the EU-SMILING project |
| clean_smiling_laos.R | Cleans Laos data from the EU-SMILING project |
| clean_smiling_thailand.R | Cleans Thailand data from the EU-SMILING project |
| clean_smiling_vietnam.R | Cleans Vietnam data from the EU-SMILING project |


## *Peer review data extraction and cleaning*
Describes how peer review data is pulled together and cleaned.

| Script | Description |
| --- | --- |
| clean_peer_review.R | This script brings together a number of different spreadsheets that include, and joins them by the metadata associated with each study. Where relative values of nutrients were reported, the script calculates the absolute value (e.g., the relative value of a) |

## *AFCD synthesis and cleaning*
Describes scripts that are used to merge FCT and peer review data. This section can also include an overview of parts/processing. 

| Script | Description |
| --- | --- |
| afcd_merge.R |  |
| quality_check_outliers.R |  |
| afcd_clean_categories.R | Includes all the  |
| taxonomy_afcd.R |  |
| afcd_finalize.R |  |


## *Categories of aquatic foods*
Reporting on the parts of aquatic foods (*e.g.,* fillet, whole) and how they were prepared (*e.g.,* raw, cooked) prior to nutrient analysis varied by peer review paper and by national and international database. We created a simplified coding scheme for the original names of the categories in the parts and preparation of aquatic food samples. The recategorizations are implememnted in the script *afcd_finalize.R*, using the code scheme defined below (and included in *afcd_clean_categories*)

The AFCD column named "prepration_of_food" is the coded version of the column with the original names "Preparation", and the AFCD column named "part_of_food" is the coded version of the column with the original names "Parts". The tables below have a full explanation of what original names are included in the codes. 


| Code | Original names for preparation included in code |
| --- | --- |
| frozen | "frozen","Frozen" |
| raw | "raw","Raw","r","wet","crudo","cruda","cru","Fresh","flesh","flesh","sa","raw (Alaska Native)","Raw","crudo","fridge","minced" |
| freezedried | "freeze-dried", "freeze dried","Freeze dried ", "freezedried","Freeze-dried","freeze-dried, dried", "freeze fried", "Freezedried" |
| rehydrated | "rehydrated" |
|  |  |
|  |  |
|  |  |
| **to fill in an additional 18 codes |  |


| Code | Original names for parts included in code |
| --- | --- |
| muscle_tissue | "f","meat","muscle","fillet","Muscle ","carne","muscle ","dorsal muscle","epaxial muscle","Muscle","soft tissue", "filete","flesh","Meat","Fillet and skinned","pulpa","meat ","fillet ","muscle, dark meat", "flesh and skin","fs","Muscle and skin","filé","músculo dorsal ","d","adductor muscle","foot","foot muscle","Adductor muscle","Foot","adductor","abdominal muscle","middle cut","muscle + skin","muscles","juvenile muscle","breast","f, skin","Fillet ","Fillet with skin","light meat","meat + subcutaneous fat","Muscles","v","músculo ventral","tail muscle" |
| whole | "w","whole","fb","whole body","Whole","Whole fish","juvenile whole",
	"cuerpo","fingerlings","entero","entera","fhb","Whole body","animal entero","fbhe","Larvae" |
| reproductive_tissue | "gonads","gonad","Ovaries","Gonad","ovary","Gonads" |
|  |  |
|  |  |
|  |  |
| **to fill in an additional 35 codes |  |
