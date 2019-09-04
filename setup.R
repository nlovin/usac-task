### Setup File



## ---------------------------
## check.packages function: install and load multiple R packages.
## Check to see if packages are installed. Install them if they are not, then load them into the R session.
check.packages <- function(pkg){
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if (length(new.pkg)) 
    install.packages(new.pkg, dependencies = TRUE)
  sapply(pkg, require, character.only = TRUE)
}

## install packages

packages<-c("rlang", "scales", "tidyverse","RSocrata", "fs", "hrbrthemes", "noncensus", "rnaturalearth", "sp", "sf", "tmap", "grid")
check.packages(packages)

rm(check.packages, packages)





## ---------------------------
## Check if the folder "Data" exists in the current directory, if not creates it


if(!dir_exists("Data")) {
  
  dir_create("Data")
  print("Data directory created")
  
} else {
  
  print("Data folder already exists")
  
}





## ---------------------------
## Check for FRN data and download if not already


if(!file_exists("Data/FRN_status_raw.Rdata")) {
  
  print("File Does Not Exist. Accessing SODA portal.")
  
  task.token <- "" # token removed
  FRN.status <- read.socrata("https://opendata.usac.org/resource/qdmp-ygft.csv?Form Version=Original&Window Status=In Window", 
                             app_token = task.token)
  
  save(FRN.status, file = "data/FRN_status_raw.Rdata") 
  
} else {
  
  print("The raw data already exists and has been loaded.")
  load("Data/FRN_status_raw.Rdata")
  
}





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
## Load state pop data for maps
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