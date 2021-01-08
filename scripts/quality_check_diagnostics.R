# ===============================================================================
# Assessing nutrition information quality in AFCD
#
#
# Name: J. Zachary (Zach) Koehn
# Email: zkoehn@gmail.com
# For: Marian Kjellevold & Shakuntala Thilsted
# Date started: 08/17/2017
# Revised: 09/14/2020
# ===============================================================================

# Initial instructions:
# Sources of data on nutrient composition of aquatic foods
# - National /regional / international (INFOODS)
# - Peer-reviewed publications
# - Grey literature and reports

# Starting with some peer-reviewed articles. 

# Use protein content in raw samples (also frozen and dried (need to know if dried in the lab as part of analytical process or dried through processing) as a check for the quality of the values of other nutrients.
# (As protein content does not vary much.) 

# If we see that there are large variations (values not realistic), then to check the publications to see if errors were made in entry - and which kinds of errors (e.g. some values may have been on dried weight basis).  

# There are many kinds of samples which are analysed: fresh weight (from frozen samples), processed aquatic foods (e.g. sun dried, smoked), freeze dried; products (e.g. fish cake, soup)  
# Very important that we ensure that the correct units are presented in the data set (e.g. mg nutrient x/ 100 g raw, edible parts aquatic food; mg nutrient x / 100 g sun dried aquatic food). 



#_____________________________________________________________________________________________
# read data and load libraries directory defaults
# ________________________________________________________________________________________________
library(tidyverse)
library(calecopal)
library(data.table)
library(pbapply)
library(ggridges)

# filenames
afcd_file <- "AFCD_live.csv"
peer_review_file <- "clean_peer_review.csv" #need this to extract ONLY the peer review data we complied for the AFCD
study_info <- "study information - Sheet1.csv"
macro_peer_file <- "macro nutrients.csv"
# work directory, set yours using wk_dir here!
wk_dir <- "/Volumes/GoogleDrive/My Drive/BFA_Papers/BFA_Nutrition/Separate/aquatic_foods_nutrient_database"

afcd_dat <- read.csv(
  file.path(wk_dir,
  	"data",
  	"OutputsFromR",
  	"aquatic_food_composition_database",
  	afcd_file),
  header = TRUE)

peer_review_dat <- read.csv(
  file.path(wk_dir,
  	"data",
  	"OutputsFromR",
  	"cleaned_fcts",
  	peer_review_file)
)

peer_review_study_info <- read.csv(
  file.path(wk_dir,
  	"data",
  	"afcd_peer_review_data",
  	"Seafood nutrients",
  	study_info),
  header=TRUE
)
macro_peer_studies <- read.csv(
  file.path(wk_dir,
  	"data",
  	"afcd_peer_review_data",
  	"Seafood nutrients",
  	macro_peer_file),
  header=TRUE
)

# first extract the rows representing studies we compiled for AFCD
peer_review_study_ids <- unique(as.character(peer_review_dat$Study.ID.number))
peer_review_study_ids <- peer_review_study_ids[is.na(peer_review_study_ids)==FALSE]

peer_sub <- merge(peer_review_study_info,peer_sub,all.y=TRUE,by="Study.ID.number")

peer_sub <- afcd_dat %>%
	filter(
		Study.ID.number %in% peer_review_study_ids
		)



weird_parts <- peer_sub %>%
	filter(Parts %in% c(
		"body wall",
		"gills","gill",
		"viscera",
		"gonads","gonad",
		"scale",
		"hepatopancreas",
		"intestine",
		"foot",
		"stomach",
		"heart",
		"digestive gland",
		"gelatin",
		"brain",
		"Meristematic tip",
		"digestive tract"
		)
	)


acid_freeze_dried_prep_lit <- peer_sub %>%
	filter(Preparation %in% c(
		"freeze-dried",
		"acid digestion"
		)
	) %>% 
	select(Preparation,Study.DOI,Study.APA.citation) %>%
	distinct()


species_protein_lit <- merge(peer_review_study_info,peer_sub,all.y=TRUE,by.x="Study.ID.number",by.y="Study.ID.number") %>%
	filter(
		order %in% ray_fins_high_variance,
		Preparation %in% c("dried","frozen","unknown","freeze-dried","raw"),
		Parts %in% c("w","f","whole","flesh and skin","e")
		) %>%
	select(species, order,
		Study.DOI,Study.APA.citation,
		Protein.total.calculated.from.total.nitrogen,
		Nitrogen.nonprotein,
		Protein.total.calculated.from.protein.nitrogen,
		Protein.total.method.of.determination.unknown.or.variable,
		Conversion.factor.to.calculate.total.protein.from.nitrogen,
		Nitrogen.protein,
		Protein.soluble
		) %>%
	# filter(is.na(Protein.total.calculated.from.total.nitrogen)==FALSE) %>%
	distinct()


species_omega3_lit <- merge(peer_review_study_info,peer_sub,all.y=TRUE,by.x="Study.ID.number",by.y="Study.ID.number") %>%
	filter(
		# order %in% ray_fins_high_variance,
		Preparation %in% c("dried","frozen","unknown","freeze-dried","raw"),
		Parts %in% c("w","f","whole","flesh and skin","e"),
		is.na(Fatty.acids.total.n3.polyunsaturated)==FALSE
		) %>%
	select(species, order,
		Study.DOI,Study.APA.citation,
		Fatty.acids.total.n3.polyunsaturated
		) %>%
	# filter(is.na(Protein.total.calculated.from.total.nitrogen)==FALSE) %>%
	distinct()

peer_protein_dat <- merge(peer_review_study_info,macro_peer_studies,all.y=TRUE,by.x="Study.ID.number",by.y="X...Study.ID.number")
	peer_protein_dat %>% filter(
		!is.na(Protein.total.calculated.from.total.nitrogen_est) &
		!is.na(Protein.total.calculated.from.protein.nitrogen_est) &
		!is.na(Protein.total.method.of.determination.unknown.or.variable_est) &
		!is.na(Conversion.factor.to.calculate.total.protein.from.nitrogen_est) &
		!is.na(Nitrogen.protein_est) &
		!is.na(Protein.soluble_est)
		) %>%
	filter(
		order %in% ray_fins_high_variance,
		Preparation %in% c("dried","frozen","unknown","freeze-dried","raw"),
		Parts %in% c("w","f","whole","flesh and skin","e")
		)
dim(peer_protein_dat)

weird_parts_lit <- select(weird_parts,Study.DOI,Study.APA.citation)

write.csv(
	unique(weird_parts_lit),
	file.path(wk_dir,
	  	"data",
	  	"OutputsFromR",
	  	"quality_control",
	  	"weird_food_parts_lit.csv"),
	row.names=FALSE
	)

write.csv(
	acid_freeze_dried_prep_lit,
	file.path(wk_dir,
	  	"data",
	  	"OutputsFromR",
	  	"quality_control",
	  	"acid_freeze_dried_prep_lit.csv"),
	row.names=FALSE
	)

write.csv(
	species_omega3_lit,
	file.path(wk_dir,
	  	"data",
	  	"OutputsFromR",
	  	"quality_control",
	  	"species_omega3_studies.csv"),
	row.names=FALSE
	)


sort(summary(as.factor(peer_sub$Parts)),decreasing=TRUE)
sort(summary(as.factor(peer_sub$Preparation)),decreasing=TRUE)


prep_counts <- summary(as.factor(peer_sub$Preparation))
prep_counts <- names(prep_counts[prep_counts>10])

part_counts <- summary(as.factor(peer_sub$Parts))
part_counts <- names(part_counts[part_counts>30])

summary(peer_sub$Edible.portion.coefficient)


genus_to_export <- c(
	"Engraulis",
	"Trachurus",
	"Oreochromis",
	"Gadus",
	"Merluccius"
	)

genus_export_lit <- peer_sub %>%
	filter(genus %in% genus_to_export) %>% 
	select(genus,Study.DOI,Study.APA.citation) %>%
	distinct()

write.csv(
	genus_export_lit,
	file.path(wk_dir,
	  	"data",
	  	"OutputsFromR",
	  	"quality_control",
	  	"genus_lit_to_check.csv"),
	row.names=FALSE
	)

########################
# plots of every kind!!
########################
give_n <- function(x){
  return(c(y = -0.2, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}


nutrient_name <- sym("Protein.total.calculated.from.total.nitrogen")
# nutrient_name <- "Protein.total.calculated.from.protein.nitrogen"
# nutrient_name <- "Protein.total.method.of.determination.unknown.or.variable"
category <- sym("phylum")
classification <- sym("Parts")


peer_sub %>%
	filter(
		Preparation %in% prep_counts,
		Parts %in% part_counts,
		!!classification != "NA",
		is.na(!!nutrient_name)==FALSE,
		
		) %>%
	mutate(
		kingdom=str_replace(kingdom,"Metazoa","Animalia"),
		kingdom=str_replace(kingdom,"Viridiplantae","Plantae")

		) %>%
	ggplot(
		aes(x=reorder(!!classification,!!nutrient_name,FUN=median,na.rm=TRUE),
			y=!!nutrient_name)
		) +
	geom_boxplot(
		size=0.5,
		outlier.shape=NA,
		aes(
			# fill=category,
			color=!!category)
		) +
		geom_jitter(
			aes(fill=!!category,color=!!category),
			alpha=0.2,shape=16,width = 0.15
			) +
	# scale_fill_manual(values = cal_palette("kelp1")) +
	# scale_color_manual(values = cal_palette("kelp1")) +

	stat_summary(fun=median, geom="point",size=2,shape=15,color="gray10") +
	stat_summary(fun.data = give_n, geom = "text", size=3,hjust = 1,vjust=0.5) +
	coord_flip() +
	ylab("Protein calculated from total nitrogen") +
	xlab(NULL) +
	theme_bw()



nutrient_name <- sym("Protein.total.calculated.from.total.nitrogen")
# nutrient_name <- "Protein.total.calculated.from.protein.nitrogen"
# nutrient_name <- "Protein.total.method.of.determination.unknown.or.variable"
category <- sym("Preparation")
classification <- sym("phylum")

peer_sub %>%
	filter(
		Preparation %in% c("dried","frozen","unknown","freeze-dried","raw"),
		Parts %in% c("w","f","whole","flesh and skin","e"),
		!!classification != "NA",
		is.na(!!nutrient_name)==FALSE
		
		) %>%
	mutate(
		kingdom=str_replace(kingdom,"Metazoa","Animalia"),
		kingdom=str_replace(kingdom,"Viridiplantae","Plantae")

		) %>%
	ggplot(
		aes(
			x=reorder(!!classification,!!nutrient_name,FUN=median,na.rm=TRUE),
			y=!!nutrient_name,
			color=!!category,
			fill=NULL
			)
		) +
	geom_boxplot(
		size=0.5,
		outlier.shape=NA
		) +
	# geom_jitter(
	# 	alpha=0.2,shape=16,width = 0.2
	# 	) +
	geom_point(
		shape=16,position = position_jitterdodge(jitter.width=0.1),alpha=0.2
		) +
	# scale_fill_manual(values = cal_palette("superbloom3")) +
	# scale_color_manual(values = cal_palette("superbloom3")) +
	scale_fill_brewer(palette = "Set1") +
	scale_color_brewer(palette = "Set1") +

	stat_summary(fun=median, geom="point",size=6,shape=8,color="gray10") +
	stat_summary(fun.data = give_n, geom = "text", size=3,hjust = 1,vjust=0.5,color="gray10") +
	coord_flip() +
	ylab("Protein concentration (g/100g product form)") +
	xlab(NULL) +
	labs(
		title="Protein calculated from total nitrogen",
		subtitle="Only includes dried, frozen, freeze-dried,raw and 'unknown' product forms"
		)+
	theme(
		legend.position="top"
		)+
	theme_bw()



nutrient_name <- sym("Protein.total.calculated.from.total.nitrogen")
# nutrient_name <- "Protein.total.calculated.from.protein.nitrogen"
# nutrient_name <- "Protein.total.method.of.determination.unknown.or.variable"
category <- sym("phylum")
classification <- sym("Parts")

weird_parts %>%
	filter(
		Preparation %in% c("dried","frozen","unknown","freeze-dried","raw"),
		!!classification != "NA",
		!!category != "NA",
		is.na(!!nutrient_name)==FALSE
		
		) %>%
	mutate(
		kingdom=str_replace(kingdom,"Metazoa","Animalia"),
		kingdom=str_replace(kingdom,"Viridiplantae","Plantae")

		) %>%
	ggplot(
		aes(
			x=reorder(!!classification,!!nutrient_name,FUN=median,na.rm=TRUE),
			y=!!nutrient_name,
			color=!!category,
			fill=NULL
			)
		) +
	geom_boxplot(
		size=0.5,
		outlier.shape=NA
		) +
	# geom_jitter(
	# 	alpha=0.2,shape=16,width = 0.2
	# 	) +
	geom_point(
		shape=16,position = position_jitterdodge(jitter.width=0.1),alpha=0.2
		) +
	# scale_fill_manual(values = cal_palette("superbloom3")) +
	# scale_color_manual(values = cal_palette("superbloom3")) +
	scale_fill_brewer(palette = "Set1") +
	scale_color_brewer(palette = "Set1") +

	stat_summary(fun=median, geom="point",size=6,shape=8,color="gray10") +
	stat_summary(fun.data = give_n, geom = "text", size=3,hjust = 1,vjust=0.5,color="gray10") +
	coord_flip() +
	ylab("Protein concentration (g/100g product form)") +
	xlab(NULL) +
	labs(
		title="Weird parts of aquatic foods in Protein calculated from total nitrogen",
		subtitle="Only includes dried, frozen, freeze-dried,raw and 'unknown' product forms"
		)+
	theme(
		legend.position="top"
		)+
	theme_bw()

unique(peer_sub$class)


nutrient_name <- sym("Protein.total.calculated.from.total.nitrogen")
# nutrient_name <- "Protein.total.calculated.from.protein.nitrogen"
# nutrient_name <- "Protein.total.method.of.determination.unknown.or.variable"
category <- sym("Parts")
classification <- sym("order")

ray_finned_class_plot <- peer_sub %>%
	filter(
		Preparation %in% c("dried","frozen","unknown","freeze-dried","raw"),
		Parts %in% c("w","f","whole","flesh and skin","e"),
		!!classification != "NA",
		is.na(!!nutrient_name)==FALSE,
		class == "actinopteri"
		) %>%
	mutate(
		kingdom=str_replace(kingdom,"Metazoa","Animalia"),
		kingdom=str_replace(kingdom,"Viridiplantae","Plantae")
		) %>%
	ggplot(
		aes(
			x=reorder(!!classification,!!nutrient_name,FUN=median,na.rm=TRUE),
			y=!!nutrient_name,
			color=!!category,
			fill=NULL
			)
		) +
	geom_boxplot(
		size=0.5,
		outlier.shape=NA
		) +
	# geom_jitter(
	# 	alpha=0.2,shape=16,width = 0.2
	# 	) +
	geom_point(
		shape=16,position = position_jitterdodge(jitter.width=0.1),alpha=0.2
		) +
	# scale_fill_manual(values = cal_palette("superbloom3")) +
	# scale_color_manual(values = cal_palette("superbloom3")) +
	scale_fill_brewer(palette = "Set1") +
	scale_color_brewer(palette = "Set1") +

	stat_summary(fun=median, geom="point",size=6,shape=8,color="gray10") +
	stat_summary(fun.data = give_n, geom = "text", size=3,hjust = 1,vjust=0.5,color="gray10") +
	coord_flip() +
	ylab("Protein concentration (g/100g product form)") +
	xlab(NULL) +
	labs(
		title="Actinopteri (Ray-finned fishes) - Protein calculated from total nitrogen",
		subtitle="Only includes dried, frozen, freeze-dried,raw and 'unknown' product forms"
		)+
	theme(
		legend.position="top"
		)+
	theme_bw()


nutrient_name <- sym("Protein.total.calculated.from.total.nitrogen")
# nutrient_name <- "Protein.total.calculated.from.protein.nitrogen"
# nutrient_name <- "Protein.total.method.of.determination.unknown.or.variable"
category <- sym("Parts")
classification <- sym("genus") 

clupeiformes_family_plot <- peer_sub %>%
	filter(
		Preparation %in% c("dried","frozen","unknown","freeze-dried","raw"),
		Parts %in% c("w","f","whole","flesh and skin","e"),
		!!classification != "NA",
		is.na(!!nutrient_name)==FALSE,
		class == "Actinopteri",
		order=="Clupeiformes"
		) %>%
	mutate(
		kingdom=str_replace(kingdom,"Metazoa","Animalia"),
		kingdom=str_replace(kingdom,"Viridiplantae","Plantae")

		) %>%
	ggplot(
		aes(
			x=reorder(!!classification,!!nutrient_name,FUN=median,na.rm=TRUE),
			y=!!nutrient_name,
			color=!!category,
			fill=NULL
			)
		) +
	geom_boxplot(
		size=0.5,
		outlier.shape=NA
		) +
	# geom_jitter(
	# 	alpha=0.2,shape=16,width = 0.2
	# 	) +
	geom_point(
		shape=16,position = position_jitterdodge(jitter.width=0.1),alpha=0.2
		) +
	# scale_fill_manual(values = cal_palette("superbloom3")) +
	# scale_color_manual(values = cal_palette("superbloom3")) +
	scale_fill_brewer(palette = "Set1") +
	scale_color_brewer(palette = "Set1") +

	stat_summary(fun=median, geom="point",size=6,shape=8,color="gray10") +
	stat_summary(fun.data = give_n, geom = "text", size=3,hjust = 1,vjust=0.5,color="gray10") +
	coord_flip() +
	ylab("Protein concentration (g/100g product form)") +
	xlab(NULL) +
	labs(
		title="Clupeiformes - Protein calculated from total nitrogen",
		subtitle="Only includes dried, frozen, freeze-dried,raw and 'unknown' product forms"
		)+
	theme(
		legend.position="top"
		)+
	theme_bw()


salmoniformes_family_plot <- peer_sub %>%
	filter(
		Preparation %in% c("dried","frozen","unknown","freeze-dried","raw"),
		Parts %in% c("w","f","whole","flesh and skin","e"),
		!!classification != "NA",
		is.na(!!nutrient_name)==FALSE,
		class == "Actinopteri",
		order=="Salmoniformes"
		) %>%
	mutate(
		kingdom=str_replace(kingdom,"Metazoa","Animalia"),
		kingdom=str_replace(kingdom,"Viridiplantae","Plantae")

		) %>%
	ggplot(
		aes(
			x=reorder(!!classification,!!nutrient_name,FUN=median,na.rm=TRUE),
			y=!!nutrient_name,
			color=!!category,
			fill=NULL
			)
		) +
	geom_boxplot(
		size=0.5,
		outlier.shape=NA
		) +
	# geom_jitter(
	# 	alpha=0.2,shape=16,width = 0.2
	# 	) +
	geom_point(
		shape=16,position = position_jitterdodge(jitter.width=0.1),alpha=0.2
		) +
	# scale_fill_manual(values = cal_palette("superbloom3")) +
	# scale_color_manual(values = cal_palette("superbloom3")) +
	scale_fill_brewer(palette = "Set1") +
	scale_color_brewer(palette = "Set1") +

	stat_summary(fun=median, geom="point",size=6,shape=8,color="gray10") +
	stat_summary(fun.data = give_n, geom = "text", size=3,hjust = 1,vjust=0.5,color="gray10") +
	coord_flip() +
	ylab("Protein concentration (g/100g product form)") +
	xlab(NULL) +
	labs(
		title="Salmoniformes - Protein calculated from total nitrogen",
		subtitle="Only includes dried, frozen, freeze-dried,raw and 'unknown' product forms"
		)+
	theme(
		legend.position="top"
		)+
	theme_bw()

carangiformes_family_plot <- peer_sub %>%
	filter(
		Preparation %in% c("dried","frozen","unknown","freeze-dried","raw"),
		Parts %in% c("w","f","whole","flesh and skin","e"),
		!!classification != "NA",
		is.na(!!nutrient_name)==FALSE,
		class == "Actinopteri",
		order=="Carangiformes"
		) %>%
	mutate(
		kingdom=str_replace(kingdom,"Metazoa","Animalia"),
		kingdom=str_replace(kingdom,"Viridiplantae","Plantae")

		) %>%
	ggplot(
		aes(
			x=reorder(!!classification,!!nutrient_name,FUN=median,na.rm=TRUE),
			y=!!nutrient_name,
			color=!!category,
			fill=NULL
			)
		) +
	geom_boxplot(
		size=0.5,
		outlier.shape=NA
		) +
	# geom_jitter(
	# 	alpha=0.2,shape=16,width = 0.2
	# 	) +
	geom_point(
		shape=16,position = position_jitterdodge(jitter.width=0.1),alpha=0.2
		) +
	# scale_fill_manual(values = cal_palette("superbloom3")) +
	# scale_color_manual(values = cal_palette("superbloom3")) +
	scale_fill_brewer(palette = "Set1") +
	scale_color_brewer(palette = "Set1") +

	stat_summary(fun=median, geom="point",size=6,shape=8,color="gray10") +
	stat_summary(fun.data = give_n, geom = "text", size=3,hjust = 1,vjust=0.5,color="gray10") +
	coord_flip() +
	ylab("Protein concentration (g/100g product form)") +
	xlab(NULL) +
	labs(
		title="Carangiformes - Protein calculated from total nitrogen",
		subtitle="Only includes dried, frozen, freeze-dried,raw and 'unknown' product forms"
		)+
	theme(
		legend.position="top"
		)+
	theme_bw()


cichliformes_family_plot <- peer_sub %>%
	filter(
		Preparation %in% c("dried","frozen","unknown","freeze-dried","raw"),
		Parts %in% c("w","f","whole","flesh and skin","e"),
		!!classification != "NA",
		is.na(!!nutrient_name)==FALSE,
		class == "Actinopteri",
		order=="Cichliformes"
		) %>%
	mutate(
		kingdom=str_replace(kingdom,"Metazoa","Animalia"),
		kingdom=str_replace(kingdom,"Viridiplantae","Plantae")

		) %>%
	ggplot(
		aes(
			x=reorder(!!classification,!!nutrient_name,FUN=median,na.rm=TRUE),
			y=!!nutrient_name,
			color=!!category,
			fill=NULL
			)
		) +
	geom_boxplot(
		size=0.5,
		outlier.shape=NA
		) +
	# geom_jitter(
	# 	alpha=0.2,shape=16,width = 0.2
	# 	) +
	geom_point(
		shape=16,position = position_jitterdodge(jitter.width=0.1),alpha=0.2
		) +
	# scale_fill_manual(values = cal_palette("superbloom3")) +
	# scale_color_manual(values = cal_palette("superbloom3")) +
	scale_fill_brewer(palette = "Set1") +
	scale_color_brewer(palette = "Set1") +

	stat_summary(fun=median, geom="point",size=6,shape=8,color="gray10") +
	stat_summary(fun.data = give_n, geom = "text", size=3,hjust = 1,vjust=0.5,color="gray10") +
	coord_flip() +
	ylab("Protein concentration (g/100g product form)") +
	xlab(NULL) +
	labs(
		title="Cichliformes - Protein calculated from total nitrogen",
		subtitle="Only includes dried, frozen, freeze-dried,raw and 'unknown' product forms"
		)+
	theme(
		legend.position="top"
		)+
	theme_bw()


gadiformes_family_plot <- peer_sub %>%
	filter(
		Preparation %in% c("dried","frozen","unknown","freeze-dried","raw"),
		Parts %in% c("w","f","whole","flesh and skin","e"),
		!!classification != "NA",
		is.na(!!nutrient_name)==FALSE,
		class == "Actinopteri",
		order=="Gadiformes"
		) %>%
	mutate(
		kingdom=str_replace(kingdom,"Metazoa","Animalia"),
		kingdom=str_replace(kingdom,"Viridiplantae","Plantae")

		) %>%
	ggplot(
		aes(
			x=reorder(!!classification,!!nutrient_name,FUN=median,na.rm=TRUE),
			y=!!nutrient_name,
			color=!!category,
			fill=NULL
			)
		) +
	geom_boxplot(
		size=0.5,
		outlier.shape=NA
		) +
	# geom_jitter(
	# 	alpha=0.2,shape=16,width = 0.2
	# 	) +
	geom_point(
		shape=16,position = position_jitterdodge(jitter.width=0.1),alpha=0.2
		) +
	# scale_fill_manual(values = cal_palette("superbloom3")) +
	# scale_color_manual(values = cal_palette("superbloom3")) +
	scale_fill_brewer(palette = "Set1") +
	scale_color_brewer(palette = "Set1") +

	stat_summary(fun=median, geom="point",size=6,shape=8,color="gray10") +
	stat_summary(fun.data = give_n, geom = "text", size=3,hjust = 1,vjust=0.5,color="gray10") +
	coord_flip() +
	ylab("Protein concentration (g/100g product form)") +
	xlab(NULL) +
	labs(
		title="Gadiformes - Protein calculated from total nitrogen",
		subtitle="Only includes dried, frozen, freeze-dried,raw and 'unknown' product forms"
		)+
	theme(
		legend.position="top"
		)+
	theme_bw()

pdf(
	file.path(wk_dir,
	  	"data",
	  	"OutputsFromR",
	  	"quality_control",
	  	"plots",
	  	"Actinopteri_plots.pdf"
	  	)
	)
ray_finned_class_plot
clupeiformes_family_plot
salmoniformes_family_plot
carangiformes_family_plot
cichliformes_family_plot
gadiformes_family_plot
dev.off()
