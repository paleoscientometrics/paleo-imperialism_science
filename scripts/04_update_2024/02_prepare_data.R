## **********************************
##
## Project: Science update
##
## Purpose of script: To prepare the data for the update
##
## Author: Nussaïbah B. Raja
## Copyright (c) N. Raja, 2024
## Email: nussaibah.raja.schoob@fau.de
##
## Date Created: 2024-06-26
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

## ****************************************************
## Read scraped data
## ****************************************************

dat1 <- read.csv("output/2024_data/aff_doi.csv")

dat1 <- dat1 %>% 
  select(reference_no, aff) %>% 
  mutate(aff_country = strsplit(aff, ";")) %>% 
  tidyr::unnest(cols = aff_country) %>% 
  select(-aff)

## ****************************************************
## Read manual data
## ****************************************************

dat2 <- read.csv("output/2024_data/data_nodoi_manual.csv")

dat2 <- dat2 %>% 
  select(reference_no, starts_with("aff")) %>% 
  mutate(across(starts_with("aff"), ~ ifelse(is.na(.x), "", .x))) %>% 
  rowwise() %>% 
  mutate(aff_country = paste(c_across(starts_with("aff")), collapse = ";"), 
         aff_country = gsub(";;+", "", aff_country)) %>% 
  ungroup() %>% 
  select(reference_no, aff_country)

dat2 <- dat2 %>% 
  select(reference_no, aff_country) %>% 
  mutate(aff_country = strsplit(trimws(aff_country), ";")) %>% 
  tidyr::unnest(cols = aff_country) 

## ****************************************************
## Merge data
## ****************************************************

dat <- bind_rows(dat1, dat2)
dat <- dat %>% filter(aff_country != "") # removing refs for which we have no data

# pbdb <- read.csv("data/2024_data/pbdb_data.csv") # not saved to GH
# pbdb <- pbdb %>% filter(reference_no %in% dat$reference_no)
# saveRDS(pbdb, "data/2024_data/pbdb_data.rds")

pbdb <- readRDS("data/2024_data/pbdb_data.rds")

pbdb <- pbdb %>% 
  select(samp_country = cc, reference_no) %>% 
  mutate(samp_country = countrycode::countrycode(samp_country, origin="iso2c", destination="country.name")) %>% 
  distinct()

df <- dat %>% 
  left_join(pbdb, by = "reference_no")

write.csv(df, "data/2024_data/aff-data-complete.csv", row.names = FALSE)
