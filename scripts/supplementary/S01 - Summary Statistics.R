## ---------------------------
##
## Project: Colonial history and global economics distort our understanding of deep-time biodiversity
##
## Purpose of script: Calculate some summary figures and language distribution
##
## Author: Nussaïbah B. Raja
## Copyright (c) N. Raja, 2021
## Email: nussaibah.raja.schoob@fau.de
##
## Date Created: 2021-03-13
## Last Modified: 2021-12-30
##
## ---------------------------
##
## Notes:
##   
##
## ---------------------------
library(tidyverse)

# Load data ---------------------------------------------------------------

load(file.path("data", "refs.RData"))
unknowns <- read.csv(file.path("data", "unknown.csv"))

all_refs <- all_refs[all_refs$pubyr >= 1990 & all_refs$pubyr < 2021,]
nrow(all_refs)
completed_refs <- completed_refs[completed_refs$reference_no %in% all_refs$reference_no,]
unknowns <- unknowns[unknowns$reference_no %in% all_refs$reference_no,]

# Number of publications
length(unique(completed_refs$reference_no))
length(unique(completed_refs$reference_no)) - 11037
length(unique(completed_refs$reference_no)) + length(unique(unknowns$reference_no))


# Reference details -------------------------------------------------------
all_refs <- subset(all_refs, reference_no %in% completed_refs$reference_no)

single <- all_refs[!is.na(all_refs$author2last),]
nrow(single) #non-single author pubs
nrow(single)/nrow(all_refs)

#number of countries per reference 
ncount <- completed_refs[completed_refs$reference_no %in% single$reference_no,]
ncount <- unique(ncount)
ncount <- table(ncount$reference_no)
length(ncount[ncount > 1])
length(ncount[ncount > 1])/nrow(all_refs)
table(ncount)

# Languages
languages <- setNames(data.frame(table(all_refs$language)), c("language", "freq"))
languages$language <- as.character(languages$language)

languages$language[!languages$language %in% c("English", "French", "Spanish", "German", "Chinese")] <- "Other"

nn <- sum(languages$freq)

data <- languages %>% group_by(language) %>% 
	summarise(freq=sum(freq)) %>% 
	arrange(freq)

data$language <- factor(data$language, levels=data$language)

svg(file.path("figs", "languages.svg"), w=4, h=4)
PieDonut(data,aes(language, count=freq),ratioByGroup=FALSE,
		 explode = 6,
		 showRatioThreshold = getOption("PieDonut.showRatioThreshold", 0.05)) +
	scale_fill_manual(values=RColorBrewer::brewer.pal(8, "Greens"))
dev.off()
