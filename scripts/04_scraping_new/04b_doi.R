## **********************************
##
## Project: Update for science
##
## Purpose of script: To extract affiliations using the Scopus API (only pubs with doi)
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
library(rscopus)


## ****************************************************
## Read data
## ****************************************************

dat_raw <- read.csv("data/2024_data/pbdb_new_doi.csv") %>% 
  filter(pubyr > 1989)

dat_doi <- dat_raw %>% filter(doi != "")

# cleaning up some dois
dat_doi$doi <- gsub("^.+doi.org/", "", dat_doi$doi)

prob_doi <- grep("^10", dat_doi$doi, invert = TRUE)
# [1]  14  21  67 147

dat_doi$doi[prob_doi]
# [1] "0.1016/j.revpalbo.2012.03.015"   "ISBN 978-84-7840-707-1"          "doi:10.3732/ajb.1000461"        
# [4] "doi:10.1016/j.ympev.2018.08.011"

dat_doi$doi[prob_doi[1]] <- paste0("1", dat_doi$doi[prob_doi[1]])
dat_doi$doi[prob_doi[3:4]] <- gsub("doi:", "", dat_doi$doi[prob_doi[3:4]])

dat_doi <- dat_doi[-prob_doi[2],] # removing because it's a ISBN

grep("^10", dat_doi$doi, invert = TRUE) # 0


## ****************************************************
## Getting data from scopus
## ****************************************************

api_key <- get_api_key() # personal api key obtains from Scopus

res <- list()

for(i in 1:nrow(dat_doi)){
  
  tmp <- abstract_retrieval(id = dat_doi$doi[i], identifier = "doi") # retrieve abstract info
  
  if(length(tmp$content) > 0){ # get affiliations
    tmp <- tmp$content$`abstracts-retrieval-response`$affiliation
    
    if(any(grepl("affiliation", names(tmp)))) tmp <- list(tmp)
    
    aff <- purrr::map_chr(tmp, ~{
      
      .x$`affiliation-country`
    })
    
    aff <- aff[!duplicated(aff)] # remove duplicates to be consistent with previous data
    aff <- paste(aff, collapse = ";")
    
    res[[i]] <- aff} else {
      res[[i]] <- NA
    }  
  
}

## ****************************************************
## Save extracted data
## ****************************************************

dat_doi$aff <- do.call(rbind, res)

dat_doi <- dat_doi %>% 
  filter(aff != "" & !is.na(aff))

write.csv(dat_doi, "output/2024_data/aff_doi.csv", row.names = FALSE)

dat_todo <- anti_join(dat_raw, dat_doi, by = "reference_no")

write.csv(dat_todo, "output/data_nodoi.csv", row.names = FALSE)
