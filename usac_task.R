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


source("setup.R")

## ---------------------------
## Access the data


task.token <- "" # token removed
FRN.status <- read.socrata("https://opendata.usac.org/resource/qdmp-ygft.csv?Form Version=Original&Window Status=In Window", 
                           app_token = task.token)

## --------------------------- 
## Save Data


## Save raw data locally to avoid hitting the API again later

save(FRN.status, file = "data/FRN_status_raw.Rdata") 

## Create subset file to explore data structure
write_csv(FRN.status, path = "data/FRN_status_raw.csv")
frn %>%
  head(500) %>% 
  write_csv(path = "data/subset.csv")

## Rdata file is much more efficent at 41MB vs 200MB csv file

## ---------------------------
## Load data

load("Data/FRN_status_raw.Rdata")


## ---------------------------
## Wrangling and Exploring

## Filter for years between 2016 and 2018

frn <- FRN.status %>% 
  filter(between(funding_year,2016,2018))

## Exploring main variables
frn %>% skimr::skim(funding_year, funding_request_number, funding_commitment_request)
frn %>% group_by(funding_year) %>% skimr::skim(funding_request_number, funding_commitment_request,avg_cost_per_ft_of_plant, state,bid_count)
frn %>% skimr::skim()

## Double check to see if there are any duplicate funding request ids
frn %>% 
  count(funding_request_number) %>% 
  filter(n > 1)

### Double check for missing funding request ids
frn %>% 
  filter(is.na(funding_request_number))

### Double check for missing funding request amounts
frn %>% 
  filter(is.na(funding_commitment_request)) %>% 
  select(funding_year, funding_request_number, funding_commitment_request)

## YoY Calculation
frn %>% 
  group_by(funding_year) %>% 
  summarise(requests = n(),
            ammount.req = sum(funding_commitment_request,na.rm = T)) %>% 
  mutate(yoy.req = requests - lag(requests),
         yoy_pct.req = 100*((requests - lag(requests))/requests),
         yoy.ammount = ammount.req - lag(ammount.req),
         yoy_pct.ammount = 100*((ammount.req - lag(ammount.req))/ammount.req))
  

## ---------------------------
## Total Requests & Request Ammounts by Service Type
service <- frn %>% 
  group_by(form_471_service_type_name,funding_year) %>% 
  summarise(requests = n(),
            dollars = sum(funding_commitment_request, na.rm = T)) %>% 
  mutate(yoy.dollars = dollars - lag(dollars),
         yoy_pct.dollars = ((dollars - lag(dollars))/dollars),
         yoy.requests = requests - lag(requests),
         yoy_pct.requests = ((requests - lag(requests))/requests))

## Total Requests & Request Ammounts by Entity
entity <- frn %>% 
  group_by(organization_entity_type_name,funding_year) %>% 
  summarise(requests = n(),
            dollars = sum(funding_commitment_request, na.rm = T)) %>% 
  mutate(yoy.dollars = dollars - lag(dollars),
         yoy_pct.dollars = ((dollars - lag(dollars))/dollars),
         yoy.requests = requests - lag(requests),
         yoy_pct.requests = ((requests - lag(requests))/requests))

## Total Requests & Request Ammounts by Entity, dropping Voice requests
entity.no_voice <- frn %>% 
  filter(form_471_service_type_name != "Voice") %>% 
  group_by(organization_entity_type_name,funding_year) %>% 
  summarise(requests = n(),
            dollars = sum(funding_commitment_request, na.rm = T)) %>% 
  mutate(yoy.dollars = dollars - lag(dollars),
         yoy_pct.dollars = ((dollars - lag(dollars))/dollars),
         yoy.requests = requests - lag(requests),
         yoy_pct.requests = ((requests - lag(requests))/requests))


## ---------------------------
#frn %>% 
  group_by(funding_year) %>% 
  summarise(a = prettyNum( sum(funding_commitment_request,na.rm = T), big.mark = "," ))

#bids <- frn %>% filter(bid_count > 100) %>% arrange(bid_count)

