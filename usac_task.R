## ---------------------------
##
## Script name: usac_task.R
##
## Purpose of script: Complete USAC Data Wrangling and Viz. Task
##
## Author: Nathan Lovin
##
## Date Created: 2019-08-30
##
## 
## Email: natelovin@gmail.com
##
## ---------------------------
##
## Notes: source.R script sets up directories and loads appropriate packages
##   
##
## ---------------------------



## ---------------------------
## Run Source File


source(setup.R)

## ---------------------------
## Access the data


task.token <- "" # token removed
FRN.status <- read.socrata("https://opendata.usac.org/resource/qdmp-ygft.csv?Form Version=Original&Window Status=In Window", 
                           app_token = task.token)

## --------------------------- 
## Save Data


## Save raw data locally to avoid hitting the API again later

save(FRN.status, file = "data/FRN_status_raw.Rdata") 

write_csv(FRN.status, path = "data/FRN_status_raw.csv")

## Rdata file is much more efficent at 41MB vs 200MB csv file



## ---------------------------
## Wrangling and Exploring

## Filter for years between 2016 and 2018

frn <- FRN.status %>% 
  filter(between(funding_year,2016,2018))

## Exploring main variables
frn %>% skimr::skim(funding_year, funding_request_number, funding_commitment_request)


## Check to see if there are any duplicate funding request ids
frn %>% 
  count(funding_request_number) %>% 
  filter(n > 1)

### Check for missing funding request ids
frn %>% 
  filter(is.na(funding_request_number))

## Request count by year
frn %>% 
  group_by(funding_year) %>% 
  summarise(requests = n())




## YoY Requests