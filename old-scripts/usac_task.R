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
## Wrangling

## Filter for years between 2016 and 2018

frn <- FRN.status %>% 
  filter(between(funding_year,2016,2018))

## YoY Calculation
yoy <- frn %>% 
  group_by(funding_year) %>% 
  summarise(requests = n(),
            ammount.req = sum(funding_commitment_request,na.rm = T)) %>% 
  mutate(yoy.req = requests - lag(requests),
         yoy_pct.req = round(100*((requests - lag(requests))/requests),1),
         yoy.ammount = ammount.req - lag(ammount.req),
         yoy_pct.ammount = round(100*((ammount.req - lag(ammount.req))/ammount.req),1))


## YoY Calculation, no voice
yoy.novoice <- frn %>% 
  filter(form_471_service_type_name != "Voice") %>% 
  group_by(funding_year) %>% 
  summarise(requests = n(),
            ammount.req = sum(funding_commitment_request,na.rm = T)) %>% 
  mutate(yoy.req = requests - lag(requests),
         yoy_pct.req = round(100*((requests - lag(requests))/requests),1),
         yoy.ammount = ammount.req - lag(ammount.req),
         yoy_pct.ammount = round(100*((ammount.req - lag(ammount.req))/ammount.req),1))


## YoY Calculation, no voice, no school
yoy.no.voice_school <- frn %>% 
  filter(organization_entity_type_name != "School" & organization_entity_type_name != "School District" & form_471_service_type_name != "Voice") %>% 
  group_by(funding_year) %>% 
  summarise(requests = n(),
            ammount.req = sum(funding_commitment_request,na.rm = T)) %>% 
  mutate(yoy.req = requests - lag(requests),
         yoy_pct.req = round(100*((requests - lag(requests))/requests),1),
         yoy.ammount = ammount.req - lag(ammount.req),
         yoy_pct.ammount = round(100*((ammount.req - lag(ammount.req))/ammount.req),1))

## YoY Calculation, no voice, no connections
yoy.no.voice.connections <- frn %>% 
  filter(form_471_service_type_name != "Voice",
         form_471_service_type_name != "Internal Connections") %>% 
  group_by(funding_year) %>% 
  summarise(requests = n(),
            ammount.req = sum(funding_commitment_request,na.rm = T)) %>% 
  mutate(yoy.req = requests - lag(requests),
         yoy_pct.req = round(100*((requests - lag(requests))/requests),1),
         yoy.ammount = ammount.req - lag(ammount.req),
         yoy_pct.ammount = round(100*((ammount.req - lag(ammount.req))/ammount.req),1))

## Load state pop data
data(states)  

yoy.state <- frn %>% 
  group_by(state,funding_year) %>% 
  summarise(requests = n(),
            ammount.req = sum(funding_commitment_request, na.rm = T)) %>% 
  mutate(yoy.req = requests - lag(requests),
         yoy_pct.req = round(100*((requests - lag(requests))/requests),1),
         yoy.ammount = ammount.req - lag(ammount.req),
         yoy_pct.ammount = round(100*((ammount.req - lag(ammount.req))/ammount.req),1)) %>% 
  ungroup() %>% 
  left_join(., states, by = "state") %>% 
  group_by(funding_year) %>% 
  mutate(total.requests_year = sum(requests),
         total.ammount_year = sum(ammount.req),
         population = as.numeric(population)) %>% 
  ungroup() %>% 
  group_by(state, funding_year) %>% 
  mutate(pct_of_total.req = 100*(requests/total.requests_year),
         pct_of_total.ammount = 100*(ammount.req/total.ammount_year),
         req.state.pct = 100*(requests/population),
         ammount.per.person = round(ammount.req/population,0))

## Remove state pop data after merging
rm(states)

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

## Total Requests & Request Ammounts by Entity, dropping Voice\Internal Connections requests
entity.no_voice.connections <- frn %>% 
  filter(form_471_service_type_name != "Voice",
         form_471_service_type_name != "Internal Connections") %>% 
  group_by(organization_entity_type_name,funding_year) %>% 
  summarise(requests = n(),
            dollars = sum(funding_commitment_request, na.rm = T)) %>% 
  mutate(yoy.dollars = dollars - lag(dollars),
         yoy_pct.dollars = ((dollars - lag(dollars))/dollars),
         yoy.requests = requests - lag(requests),
         yoy_pct.requests = ((requests - lag(requests))/requests))

## Group by entity and service
entity.service <- frn %>% 
  group_by(organization_entity_type_name,form_471_service_type_name,funding_year) %>% 
  summarise(requests = n(),
            dollars = sum(funding_commitment_request, na.rm = T)) %>% 
  mutate(yoy.dollars = dollars - lag(dollars),
         yoy_pct.dollars = ((dollars - lag(dollars))/dollars),
         yoy.requests = requests - lag(requests),
         yoy_pct.requests = ((requests - lag(requests))/requests))





## ---------------------------
## Exploratory

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




## ---------------------------
## Models

summary(lm(yoy.requests~factor(organization_entity_type_name) + factor(funding_year) + factor(form_471_service_type_name), data = entity.service))

summary(lm(requests~factor(organization_entity_type_name) + factor(funding_year) + factor(form_471_service_type_name), data = entity.service))



## ---------------------------
#frn %>% 
  group_by(funding_year) %>% 
  summarise(a = prettyNum( sum(funding_commitment_request,na.rm = T), big.mark = "," ))

#bids <- frn %>% filter(bid_count > 100) %>% arrange(bid_count)


# data.frame(
#   funding_year = c(2016, 2017, 2018),
#   requests = c(121073, 97525, 72757),
#   ammount.req = c(3597195535, 3231201425, 2854538263),
#   yoy.req = c(NA, -23548, -24768),
#   yoy_pct.req = c(NA, -24.1, -34),
#   yoy.ammount = c(NA, -365994110, -376663162)
# )

