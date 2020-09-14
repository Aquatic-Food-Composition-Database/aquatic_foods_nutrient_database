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


# first extract the rows representing studies we compiled for AFCD
peer_review_study_ids <- unique(as.character(peer_review_dat$Study.ID.number))
peer_review_study_ids <- peer_review_study_ids[is.na(peer_review_study_ids)==FALSE]

peer_sub <- afcd_dat %>%
	filter(
		Study.ID.number %in% peer_review_study_ids
		)



sort(summary(as.factor(peer_sub$Parts)),decreasing=TRUE)
sort(summary(as.factor(peer_sub$Preparation)),decreasing=TRUE)

sort(summary(as.factor(peer_sub$kingdom)),decreasing=TRUE)
sort(summary(as.factor(peer_sub$phylum)),decreasing=TRUE)
sort(summary(as.factor(peer_sub$class)),decreasing=TRUE)
sort(summary(as.factor(peer_sub$order)),decreasing=TRUE)



prep_counts <- summary(as.factor(peer_sub$Preparation))
prep_counts <- names(prep_counts[prep_counts>10])

part_counts <- summary(as.factor(peer_sub$Parts))
part_counts <- names(part_counts[part_counts>30])

summary(peer_sub$Edible.portion.coefficient)

give_n <- function(x){
  return(c(y = -0.2, label = length(x))) 
  # experiment with the multiplier to find the perfect position
}

function(dat,nutrient_name,process_type) {

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




