## **********************************
##
## Project: Science update
##
## Purpose of script: To get the dois of the publications that don't have them in the db
##
## Author: Nussaïbah B. Raja
## Copyright (c) N. Raja, 2024
## Email: nussaibah.raja.schoob@fau.de
##
## Date Created: 2024-06-08
## Last Modified:
##
## **********************************
##
## Notes:
##   
##
## **********************************


## ****************************************************
## Load libraries
## ****************************************************

library(dplyr)
library(rcrossref)

## ****************************************************
## Read data
## ****************************************************

dat_raw <- read.csv("data/2024_data/pbdb_since_2021.csv", skip = 20)

dat <- dat_raw %>% filter(doi == "") # replace with those that weren't found
dat <- dat %>% filter(pubyr > 1989 & publication_type == "journal article") # only looking for journal articles

## ****************************************************
## Grab data using the crossref API
## ****************************************************

for(i in 1:nrow(dat)){
  
  cat("\r", i)
  
  tmp <- cr_works(query = dat$reftitle[i])$data
  
  if("container.title" %in% names(tmp)){
    
    # now check if we have a match
    tmp <- tmp[intersect(
      which(adist(tolower(dat$pubtitle[i]), tolower(tmp$container.title)) < 5), # matching journal name
      which(adist(tolower(dat$reftitle[i]), tolower(tmp$title)) < 5)),] # matching title
    
    # now by publications
    if(nrow(tmp) > 0){
      
      dat$doi[i] <- tmp$doi
      
    }  
    
  }
  
}

## ****************************************************
## Merge data with the ones without doi
## ****************************************************

dat_doi <- anti_join(dat_raw, dat, by = "reference_no")
dat_new <- dat_doi %>% bind_rows(dat)

## ****************************************************
## Save data
## ****************************************************

write.csv(dat_new, file = "data/2024_data/pbdb_new_doi.csv", row.names = FALSE)

