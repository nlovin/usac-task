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
## Graphics for Report


## Fig 1:
### Number of Annual Requests
ggplot(yoy,
       aes(x = funding_year,
           y = requests)) +
  geom_bar(fill = "darkseagreen4",
           stat = 'identity',
           show.legend = FALSE) +
  geom_text(aes(label = ifelse(is.na(yoy.req), "", comma(yoy.req))),
            position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  scale_y_continuous(label=comma) +
  labs(title = "Fig. 1: Annual E-Rate Requests", 
       x = "", y = "Requests (N)",
       subtitle = "2016 - 2018",
       caption = "Values above bars represent Year-over-Year changes") +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.text=element_text(size=7),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        legend.title = element_text(size=8),
        plot.title = element_text(size=15),
        plot.caption = element_text(size=7))

## Fig 2: 
### Annual Dollar Amount Requested
ggplot(yoy,
       aes(x = funding_year,
           y = ammount.req)) +
  geom_bar(fill = "dodgerblue4",
           stat = 'identity',
           show.legend = FALSE) +
  geom_text(aes(label = ifelse(is.na(yoy.ammount), "", comma(yoy.ammount))),
            position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  scale_y_continuous(label=dollar_format()) +
  labs(title = "Fig. 2: Total Amount Requested for E-Rate Funding ($)", 
       x = "", y = "ammount requested ($)",
       subtitle = "2016 - 2018",
       caption = "Values above bars represent Year-over-Year changes") +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.text=element_text(size=7),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        legend.title = element_text(size=8),
        plot.title = element_text(size=15),
        plot.caption = element_text(size=7))



## Fig. 3:
### Annual Requests by Service Type
ggplot(service,
       aes(x = factor(form_471_service_type_name),
           y = requests,
           fill = factor(funding_year),
           group = factor(funding_year),
           label = percent(yoy_pct.requests))) +
  geom_bar(position = "dodge",
           stat = "identity") +
  geom_text(aes(label = ifelse(is.na(yoy_pct.requests), "", percent(yoy_pct.requests))),
            position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 2.5) +
  scale_fill_ipsum() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(title = "Fig. 3: Annual Requests by Service Type", 
       x = "Service Type", fill = "Funding Year", 
       subtitle = "2016-2018",
       caption = "Percentages represent Year-over-Year changes") +
  scale_y_continuous(label=comma) +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.text=element_text(size=7),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        legend.title = element_text(size=8),
        plot.title = element_text(size=16),
        plot.caption = element_text(size=7))

## Fig. 4: 
### Total Request Amounts by Service Type
ggplot(service,
       aes(x = factor(form_471_service_type_name),
           y = dollars,
           fill = factor(funding_year),
           group = factor(funding_year),
           label = percent(yoy_pct.dollars))) +
  geom_bar(position = "dodge",
           stat = "identity") +
  geom_text(aes(label = ifelse(is.na(yoy_pct.dollars), "", percent(yoy_pct.dollars))),
            position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 2.5) +
  scale_fill_ipsum() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 10)) +
  labs(title = "Fig. 4: Total Amount Requested by Service", 
       x = "Service Type", fill = "Funding Year", 
       subtitle = "2016-2018",
       caption = "Percentages represent Year-over-Year changes") +
  scale_y_continuous(label=dollar_format()) +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.text=element_text(size=7),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        legend.title = element_text(size=8),
        plot.title = element_text(size=16),
        plot.caption = element_text(size=7))


## Fig. 5:
### Requests by Year, dropping voice and connections
ggplot(yoy.no.voice.connections,
       aes(x = funding_year,
           y = requests)) +
  geom_bar(fill = "darkseagreen4",
           stat = 'identity',
           show.legend = FALSE) +
  geom_text(aes(label = ifelse(is.na(yoy.req), "", comma(yoy.req))),
            position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  scale_y_continuous(label=comma) +
  labs(title = "Fig. 5: Annual E-Rate Requests", 
       x = "", y = "Requests (N)",
       subtitle = "Voice and Internal Connection requests excluded | 2016 - 2018",
       caption = "Values above bars represent Year-over-Year changes") +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.text=element_text(size=7),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        legend.title = element_text(size=8),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12),
        plot.caption = element_text(size=7))


## Fig. 6:
### Amount by Year, dropping voice and connections
ggplot(yoy.no.voice.connections,
       aes(x = funding_year,
           y = ammount.req)) +
  geom_bar(fill = "dodgerblue4",
           stat = 'identity',
           show.legend = FALSE) +
  geom_text(aes(label = ifelse(is.na(yoy.ammount), "", comma(yoy.ammount))),
            position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  scale_y_continuous(label=dollar_format()) +
  labs(title = "Fig. 6: Total Amount Requested for E-Rate Funding ($)", 
       x = "", y = "ammount requested ($)",
       subtitle = "Voice and Internal Connection requests excluded | 2016 - 2018",
       caption = "Values above bars represent Year-over-Year changes") +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.text=element_text(size=7),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        legend.title = element_text(size=8),
        plot.title = element_text(size=15),
        plot.subtitle = element_text(size=11),
        plot.caption = element_text(size=7))


## Fig. 7:
### Entity type by Year
ggplot(entity,
       aes(x = factor(organization_entity_type_name),
           y = requests,
           fill = factor(funding_year),
           group = factor(funding_year),
           label = percent(yoy_pct.requests))) +
  geom_bar(position = "dodge",
           stat = "identity") +
  geom_text(aes(label = ifelse(is.na(yoy_pct.requests), "", percent(yoy_pct.requests))),
            position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 2.5) +
  scale_fill_ipsum() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(title = "Fig. 7: Requests by Organization Type", 
       x = "Organization Type", fill = "Funding Year", 
       subtitle = "2016-2018",
       caption = "Percentages represent Year-over-Year changes") +
  scale_y_continuous(label=comma) +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.text=element_text(size=7),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        legend.title = element_text(size=8),
        plot.title = element_text(size=16),
        plot.caption = element_text(size=7))

## Fig. 8:
### Entity type by Year, dropping voice and connections
ggplot(entity.no_voice.connections,
       aes(x = factor(organization_entity_type_name),
           y = requests,
           fill = factor(funding_year),
           group = factor(funding_year),
           label = percent(yoy_pct.requests))) +
  geom_bar(position = "dodge",
           stat = "identity") +
  geom_text(aes(label = ifelse(is.na(yoy_pct.requests), "", percent(yoy_pct.requests))),
            position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 2.5) +
  scale_fill_ipsum() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(title = "Fig. 8: Requests by Organization Type", 
       x = "Organization Type", fill = "Funding Year", 
       subtitle = "Voice and Internal Connection requests excluded | 2016-2018",
       caption = "Percentages represent Year-over-Year changes") +
  scale_y_continuous(label=comma) +
  ggthemes::theme_fivethirtyeight() +
  theme(legend.text=element_text(size=7),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        legend.title = element_text(size=8),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12),
        plot.caption = element_text(size=7))


## Fig. 9:
### Total Amount Requested by Service, dropping voice and connections
ggplot(entity.no_voice.connections,
       aes(x = factor(organization_entity_type_name),
           y = dollars,
           fill = factor(funding_year),
           group = factor(funding_year),
           label = percent(yoy_pct.dollars))) +
  geom_bar(position = "dodge",
           stat = "identity") +
  geom_text(aes(label = ifelse(is.na(yoy_pct.dollars), "", percent(yoy_pct.dollars))),
            position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 2.5) +
  scale_fill_ipsum(labels = function(x) str_wrap(x, width = 25)) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(title = "Fig. 9: Total Request Amount by Service ($)", 
       x = "Service Type", fill = "Funding Year", 
       subtitle = "Voice and Internal Connection requests excluded | 2016-2018",
       caption = "Percentages represent Year-over-Year changes") +
  scale_y_continuous(label= dollar_format()) + 
  ggthemes::theme_fivethirtyeight() +
  theme(legend.text=element_text(size=7),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        legend.title = element_text(size=8),
        plot.title = element_text(size=16),
        plot.subtitle = element_text(size=12),
        plot.caption = element_text(size=7))

## Fig. 10:
## REQUESTS by org and service
ggplot(data = entity.service) +
  geom_bar(aes(x = factor(funding_year),
               y = requests,
               fill = factor(form_471_service_type_name)),
           position = "dodge",
           stat = 'identity') +
  scale_colour_discrete(labels = function(x) str_wrap(x, width = 5)) +
  ggthemes::theme_fivethirtyeight() + 
  scale_fill_ipsum(labels = function(x) str_wrap(x, width = 25)) + 
  facet_wrap(organization_entity_type_name~.,scales = "free") +
  labs(title = "Fig. 10: Annual Requests by Service and Organization", 
       x = "", fill = "Service Requested", 
       subtitle = "2016-2018") +
  guides(fill=guide_legend(
    keywidth=0.2,
    keyheight=0.4,
    default.unit="inch")
  ) + 
  scale_y_continuous(label=comma) +
  theme(legend.text=element_text(size=7),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        legend.title = element_text(size=8),
        plot.title = element_text(size=14),
        plot.subtitle = element_text(size=11),
        plot.caption = element_text(size=7))

## Fig. 11:
## DOLLARS by org and service
ggplot(data = entity.service) +
  geom_bar(aes(x = factor(funding_year),
               y = dollars,
               fill = factor(form_471_service_type_name)),
           position = "dodge",
           stat = 'identity') +
  scale_colour_discrete(labels = function(x) str_wrap(x, width = 5)) +
  ggthemes::theme_fivethirtyeight() + 
  scale_fill_ipsum(labels = function(x) str_wrap(x, width = 25)) + 
  facet_wrap(organization_entity_type_name~.,scales = "free") +
  labs(title = "Fig. 11: Amount Requested by Service and Organization ($)", 
       x = "", fill = "Service Requested", 
       subtitle = "2016-2018") +
  guides(fill=guide_legend(
    keywidth=0.2,
    keyheight=0.4,
    default.unit="inch")
  ) + 
  scale_y_continuous(label=dollar_format()) +
  theme(legend.text=element_text(size=7),
        axis.text.x = element_text(size=7),
        axis.text.y = element_text(size=7),
        legend.title = element_text(size=8),
        plot.title = element_text(size=14),
        plot.subtitle = element_text(size=11),
        plot.caption = element_text(size=7))








## ---------------------------
# Maps


## Set up Map data
states_shp <- ne_states(returnclass = "sf",
                        country = "United States of America")

## Setup projections
US.proj <- "+proj=aea +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
#AK.proj <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
#HI.proj <- "+proj=aea +lat_1=8 +lat_2=18 +lat_0=13 +lon_0=-153 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


## Merge yoy and state state pop
yoy.state_geo <- left_join(states_shp,yoy.state, by = c("postal" = "state"))

## Drop Alaska and Hawaii to fit nicely
US_geo <- yoy.state_geo %>% 
  filter(name.y != "Alaska" & name.y != "Hawaii")

## Alaska and Hawaii Data
# AK_geo <- yoy.state_geo %>% 
#   filter(name.y == "Alaska")
# 
# HI_geo <- yoy.state_geo %>% 
#   filter(name.y == "Hawaii") %>% 
#   st_crop(., c(xmin=-162.62, xmax=-153.87, ymin=18.05, ymax=23.68))



US_geo %>% 
  filter() %>% 
  tm_shape(., projection = US.proj) +
  tm_layout(frame = FALSE) +
  tm_fill(col = "ammount.per.person",
          style = "quantile") +
  tm_borders() +
  tm_facets(by="funding_year") +
  tm_layout(legend.outside.position = "right" , 
            legend.outside.size = .1)