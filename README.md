# **Aquatic Food Composition Database**

Welcome to the Aquatic Food Composition Database! 

This repository synthesizes existing nutrient composition data for aquatic food species. These data originate from disparate sources, including national food composition tables (FCT), international datasets from FAO, and other peer reviewed published sources of nutrient composition. New FCT are dynamically added to this comprehensive database. Authorship for this database includes Christopher D. Golden, J. Zachary Koehn, Bapu Vaitla, Camille DeSisto, Kathryn J. Fiorella, Marian Kjellevold and Shakuntala H. Thilsted. 

Scripts included in this repository have different functional roles, including: extracting data from the raw FCT data, cleaning of FCT data, merging of FCT data and results of the literature review, adding taxonomic information, and finally cleaning/finalizing the overall database. References for this literature can be found [will add link here]()

The resulting database can be found at in [this repository](https://doi.org/10.7910/DVN/KI0NYM).

## Extracting and cleaning FCT data

These scripts extracted and/or cleaned the FCT data from national and international databases as needed. Databases included here include all FCT databases that were available online in machine-readeable formats (e.g., .csv). We intend to continue to build on this set as more databases become available. Note that a number of FCTs (data from Bangladesh, Canada, Mozambique, Japan, Argentina, Gambia, United Kingdom, Chile, Malaysia, Italy and Turkey) were already compiled as part of a previous project (**Chris: what should we cite for the original 'GENuS-FISH' data, the 2016 nature paper??**). As such, the scripts and descriptions included in the table below pertain only to the tables added as part of this project. 


 
 - **clean_fao_west_africa.R** - Cleans FAO's West Africa Food Composition Table dataset 
 - **clean_fct_aus**  Script cleans Australia's 2019 food composition database and subsets for aquatic foods, appended to AFCD in another script.
 - **clean_fct_india_2017**  Extracts nutrition tables from the PDF of India's 2017 FCTs, and cleans them for appending to AFCD. 
 - **clean_fct_norway.R** - Cleans data from the Norwegian Food Composition Database 
 - **clean_fct_pndb.R** - Cleans data from the Pacific Nutrient Database by Pacific Community, University of Wollongong and the Food and Agriculture Organization of the United Nations 
 - **clean_infoods_usda.R** - Cleans and adds FAO/INFOODS Global Food Composition Database for Fish and Shellfish and FAO/INFOODS Food Composition Database for Biodiversity  
 - **clean_latinfoods.R** - Cleans data from the international LATINFOODS dataset 
 - **clean_smiling_cambodia.R** - Cleans Cambodia data from the EU-SMILING project 
 - **clean_smiling_indonesia.R** - Cleans Indonesia data from the EU-SMILING project 
 - **clean_smiling_laos.R** - Cleans Laos data from the EU-SMILING project 
 - **clean_smiling_thailand.R** - Cleans Thailand data from the EU-SMILING project 
 - **clean_smiling_vietnam.R** - Cleans Vietnam data from the EU-SMILING project 
 - **usda_api.R** - Uses an API to query online USDA database (no data required), and cleans that USDA data for merge with existing AFCD dataset. 

## Peer review data extraction and cleaning
One script, **clean_peer_review.R**, brings together a number of different spreadsheets that include, and joins them by the metadata associated with each study. Where relative values of nutrients were reported, the script calculates the absolute value (e.g., the  value of a fatty acid relative to total fat content in the sample)

## AFCD synthesis and cleaning
Describes scripts that are used to merge FCT and peer review data. This section can also include an overview of parts/processing. Scripts are run in order from 1 to 4, with *afcd_finalize.R* outputing the final csv file. 

1. **afcd_merge.R** - Merges together cleaned national and international FCT data (output from each script below) as well as the cleaned peer review data (from *clean_peer_review.R*).
2. **taxonomy_afcd.R** - Adds taxononomic information to each of the products in AFCD.
3. **afcd_clean_categories.R** - Includes the codes and categories used to clean the parts and preparation columns in *afcd_finalize.R*
4. **afcd_finalize.R** - This script cleans the merged AFCD data with taxonomic information. 

## Categories of aquatic foods
Reporting on the parts of aquatic foods (*e.g.,* fillet, whole) and how they were prepared (*e.g.,* raw, cooked) prior to nutrient analysis varied by peer review paper and by national and international database. We created a simplified coding scheme for the original names of the categories in the parts and preparation of aquatic food samples. The recategorizations are implememnted in the script **afcd_finalize.R**, using the code scheme defined below (and included in *afcd_clean_categories*)

The AFCD column named "preparation_of_food" is the coded version of the column with the original names "Preparation", and the AFCD column named "part_of_food" is the coded version of the column with the original names "Parts". The tables below have a full explanation of what original names are included in the codes. 


| Preparation re-code | Original names for preparation included in code |
| --- | --- |
| frozen | "frozen","Frozen" |
| raw | "raw","Raw","r","wet","crudo","cruda","cru","Fresh","flesh","flesh","sa","raw (Alaska Native),"Raw","crudo","fridge","minced" |
| freezedried | "freeze-dried", "freeze dried","Freeze dried ", "freezedried","Freeze-dried","freeze-dried, dried", "freeze fried", "Freezedried" |
| rehydrated | "rehydrated" |
| unknown_preparation | "unknown","NA","b","sl","not specified","conserva","0","Control","wet","fz","fe","e","Experiment" |
| dried | "dried","dry","d","Dry","dried and homogenized","air-dried","Dried","seco" |
| baked | "baked","baked","baked / broiled","al horno","broiled","bake","broiled/baked" |
| boiled_steamed | "boil","boiled","boiled ","boiled/steamed","steamed","st","hervido","poached","precocido","cozido" |
| fried | "fried","f","frito","frita" |
| canned | "canned","c","enlatado","canned, drained","enlatada" |
| smoked | "smoked","sm" |
| microwaved | "microwave","microwaved" |
| oil | oil |
| cooked | "cooked","cooked, dry heat","cooked, moist heat","sancochado","cocida","cocido","roasted" |
| brined | brined |
| grilled | "grilled","assado/40 min" |
| acid_digestion | "acid digestion" |
| salted | "salted","s" |
| aged | aged |
| curried | curried |
| combination | "freeze-dried, aged","boiled, frozen","freeze-dried, boiled","m","ds","salted \\+ dried","boild, dried","kippered, canned","d; sm","kippered","kippered","dried \\+ smoked","smoked/dried","b;d","salted \\+ dried","freeze-dried, aged","freeze-dried, boiled","boiled, frozen","canned in oil","m","canned in oil, drained","boild, dried","kippered, canned","cooked in oil","cooked in rice","dried + smoked","smoked/dried","smoked, canned","salted \\+ rehydrated","smoked/baked","d; sm","canned with salt","salted \\+ fermented","b;d","dried, salted","salted \\+ smoked" |



| Parts re-code | Original names for parts included in code |
| --- | --- |
| muscle_tissue | "f","meat","muscle","fillet","Muscle ","carne","muscle ","dorsal muscle","epaxial muscle","Muscle","soft tissue", "filete","flesh","Meat","Fillet and skinned","pulpa","meat ","fillet ","muscle, dark meat", "flesh and skin","fs","Muscle and skin","filé","músculo dorsal ","d","adductor muscle","foot","foot muscle","Adductor muscle","Foot","adductor","abdominal muscle","middle cut","muscle + skin","muscles","juvenile muscle","breast","f, skin","Fillet ","Fillet with skin","light meat","meat + subcutaneous fat","Muscles","v","músculo ventral","tail muscle" |
| whole | "w","whole","fb","whole body","Whole","Whole fish","juvenile whole","cuerpo","fingerlings","entero","entera","fhb","Whole body","animal entero","fbhe","Larvae" |
| reproductive_tissue | "gonads","gonad","Ovaries","Gonad","ovary","Gonads" |
| body_wall | "thalli","body wall" |
| gills | "gills","gill","Gills" |
| roots | "Meristematic tip","Meristem" |
| unknown_part | "unknown","not specified","tunic coat","m","l","soma","","solids + liquid","s","Mid","solids + liquids","drained solids","fhe","smoked","meat, bone + liquid","solids, liquid, bones","trunk","carne con aparato digestivo","no holdings","con espinas","contenido total","composite sample for each species" |
| mantle | "mantle","Mantle" |
| cephalothorax | "cephalothorax" |
| blade | "blade","Blade" |
| stipe | "stipe","Stipe","stipes" |
| holdfast | "Holdfast","holdfast" |
| eyes | "eyes" |
| edible | "e","edible" |
| raw | "r","raw" |
| scale | "scale","scales" |
| seeds | "seeds" |
| skin | "skin","skein","Skin" |
| liver | "liver","juvenile liver","Liver" |
| viscera | "viscera","hepatopancreas","intestine","stomach","visceralmass","digestive gland","offal","Midgut gland","digestive tract","inteiro","g","head and viscera" |
| leg | "leg","legs" |
| tail | "tail","tail end" |
| shell | "Shell","shell" |
| roe | "eggs","roe","egg","eggs ","Fertalized eggs" |
| blubber | "blubber","mattak","muktuk","mattack" |
| frond | "frond","leaf" |
| bone | "bone","vertebrate","bones","vertebrae","frame" |
| claw | "claw" |
| flippers | "flippers","fin","flipper (w/o skin + bone)" |
| heart | "heart" |
| gutted | "gutted","excluding viscera","head, rear and the middle","cleaned" |
| fats | "oil","grease","fat","liver oil" |
| larvae | "Larvae","larvae" |
| head | "head","brain","head end","head, eyes, cheeks \\+ soft bones" |
| kidney | "kidney","kidneys" |
| combination | "muscle and liver","solids \\+ bones","eggs on kelp","whole, no liver" |
| gelatin | gelatin |
