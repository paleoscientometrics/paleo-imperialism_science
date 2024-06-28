## ---------------------------
##
## Project: Colonial history and global economics distort our understanding of deep-time biodiversity
##
## Purpose of script: Reshape data to be used for analyses
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

library(sp)
library(spdep)
library(rworldmap)
library(dplyr)

# Load data -----------------------------------------------------------

pbdb <- readRDS("data/2024_data/pbdb_data.rds")
completed_refs <- read.csv("data/2024_data/aff-data-complete.csv")

# Find countries for those not in there -----------------------------------

no_countries <- pbdb[pbdb$cc == "",c("lng", "lat")]
no_countries <- no_countries[!(is.na(no_countries$lng)|is.na(no_countries$lat)),]
no_countries <- unique(no_countries)

spts = SpatialPoints(no_countries[,1:2])
w <- rworldmap::getMap()
proj4string(spts) <- proj4string(w)

#planar coordinates
spts <- spTransform( spts, CRS('+proj=moll') ) 
w2 <- spTransform( w, CRS('+proj=moll') ) 

#find nearest country
dist_cc <- as.data.frame(rgeos::gDistance(spts, w2,byid=TRUE))

new_dist <- rep(NA, ncol(dist_cc))
cc <- rownames(dist_cc)
cc[cc=="Indian Ocean Territories"] <- "Australia"
cc[cc=="Saint Martin"] <- "The Netherlands"

for(i in 1:ncol(dist_cc)){
	temp <- dist_cc[,i]
	new_dist[i] <- cc[which.min(temp)]
}

no_countries <- cbind(no_countries, name=new_dist)
no_countries$cc <-countrycode::countrycode(no_countries$name, origin = 'country.name', 
										   destination = 'iso2c')

for (i in 1:nrow(no_countries)){
	n <- which(pbdb$lng == no_countries$lng[i] & pbdb$lat == no_countries$lat[i])
	
	pbdb$cc[n] <- no_countries$cc[i]
}

pbdb$country <- countrycode::countrycode(pbdb$cc, origin="iso2c", destination = "country.name")
pbdb$country[pbdb$cc=="UK"] <- "United Kingdom"
pbdb$country[pbdb$cc=="AA"] <- "Antarctica"
# pbdb$country[pbdb$cc=="FA"] <- "Faroe Islands"
pbdb$country[pbdb$country=="United States Minor Outlying Islands (the)"] <- "United States"

# Merge pbdb
pbdb <- bind_rows(pbdb_old, pbdb[, names(pbdb_old)])

# Checking dups
pbdb %>% filter(reference_no ==  "43480") # 0
pbdb %>% filter(reference_no == "33387") # 0

pbdb <- pbdb[pbdb$reference_no %in% completed_refs$reference_no,]

# Make corrections --------------------------------------------

temp <- completed_refs
temp <- temp[!temp$samp_country %in% c("", " ", "ODP Site"),]
temp <- temp[!temp$aff_country %in% c("", " ", "ODP Site"),]

# add no countries data
temp <- left_join(completed_refs, 
                 pbdb[, c("reference_no", "country")], by = "reference_no") %>% 
  distinct()

temp <- temp %>% 
  mutate(samp_country = ifelse(is.na(samp_country), country, samp_country)) %>% 
  select(-country)

temp %>% filter(is.na(samp_country)) %>% 
  distinct(reference_no) # emma, can you check if these are not connected to occurrences? maybe choose a couple of them

temp <- temp %>% 
  filter(!is.na(samp_country)) # remove NAs

# Recode country to ISO3 --------------------------------------------------

temp$samp_code <- countrycode::countrycode(temp$samp_country, origin="country.name", destination="iso3c")
temp$aff_code <- countrycode::countrycode(temp$aff_country, origin="country.name", destination="iso3c")

temp$aff_country <- countrycode::countrycode(temp$aff_code, origin = "iso3c", destination = "country.name")
temp$samp_country <- countrycode::countrycode(temp$samp_code, origin = "iso3c", destination = "country.name")

new_refs <- temp

# Save data ---------------------------------------------------------------

saveRDS(pbdb[,c("collection_no", "lng", "lat", "reference_no", "country", "cc")], file="data/2024_data/2024_pbdb.rds")

# merge with old data

load("data/refs.RData")

completed_refs <- bind_rows(completed_refs, new_refs)
save(completed_refs, file="data/2024_data/2024_refs.RData")
