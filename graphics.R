dta <- frn %>% 
  select(funding_request_number, funding_year, state, funding_commitment_request, avg_cost_per_ft_of_plant,dis_pct,pricing_confidentiality,total_monthly_recurring_cost,old_funding_request_number,award_date,form_471_service_type_name,bid_count,months_of_service) %>% 
  mutate(old_funding_request_number = ifelse(old_funding_request_number=="",NA,old_funding_request_number),
         repeat_customer = ifelse(is.na(old_funding_request_number), 0, 1))

skimr::skim(dta)

dta %>% janitor::tabyl(funding_year, repeat_customer)
dta %>% janitor::tabyl(funding_year, form_471_service_type_name)

summary(lm(funding_commitment_request ~ funding_year + total_monthly_recurring_cost + months_of_service + factor(form_471_service_type_name) + relevel(factor(organization_entity_type_name),"School District"), data=frn))


### Service Type Requests by Year
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
            size = 3) +
  scale_fill_ipsum() +
  theme_ipsum() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(title = "Requests by Service Type", 
       x = "Service Type", fill = "Funding Year", 
       subtitle = "2016-2018",
       caption = "Percentages represent Year-over-Year changes") +
  scale_y_continuous(label=comma)

## ---------------------------
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
            size = 3) +
  scale_fill_ipsum() +
  theme_ipsum() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(title = "Requests by Organization Type", 
       x = "Organization Type", fill = "Funding Year", 
       subtitle = "2016-2018",
       caption = "Percentages represent Year-over-Year changes") +
  scale_y_continuous(label=comma)

### Entity type by Year, dropping Voice requests
ggplot(entity.no_voice,
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
            size = 3) +
  scale_fill_ipsum() +
  theme_ipsum() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(title = "Requests by Organization Type", 
       x = "Organization Type", fill = "Funding Year", 
       subtitle = "2016-2018",
       caption = "Percentages represent Year-over-Year changes") +
  scale_y_continuous(label=comma)

## ---------------------------
### Funding Requests by Entity type by Request type by Year
ggplot(data = frn) +
  geom_bar(aes(x = organization_entity_type_name, fill = factor(form_471_service_type_name)),position = "dodge") +
  theme_ipsum() + 
  scale_fill_ipsum(labels = function(x) str_wrap(x, width = 25)) + 
  facet_grid(funding_year~.) +
  labs(title = "Requests by Organization Type", 
       x = "Organization Type", fill = "Service Requested", 
       subtitle = "2016-2018") +
  guides(fill=guide_legend(
    keywidth=0.2,
    keyheight=0.4,
    default.unit="inch")
  )

### Funding Requests by Entity type by Request type by Year
## DROP SCHOOL AND DISTRICTS
ggplot(data = filter(frn, organization_entity_type_name != "School" & organization_entity_type_name != "School District")) +
  geom_bar(aes(x = organization_entity_type_name, fill = factor(form_471_service_type_name)),position = "dodge") +
  scale_colour_discrete(labels = function(x) str_wrap(x, width = 5)) +
  theme_ipsum() + 
  scale_fill_ipsum(labels = function(x) str_wrap(x, width = 25)) + 
  facet_grid(funding_year~.) +
  labs(title = "Requests by Organization Type", 
       x = "Organization Type (excluding schools and school districts)", fill = "Service Requested", 
       subtitle = "2016-2018") +
  guides(fill=guide_legend(
    keywidth=0.2,
    keyheight=0.4,
    default.unit="inch")
  )

## ---------------------------
## Total Request Ammounts by Service Type
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
            size = 3) +
  scale_fill_ipsum() +
  theme_ipsum() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(title = "Total Request Amount by Service", 
       x = "Service Type", fill = "Funding Year", 
       subtitle = "2016-2018",
       caption = "Percentages represent Year-over-Year changes") +
  scale_y_continuous(label=dollar_format())


## Total Request Ammounts by Entity
ggplot(entity,
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
            size = 3) +
  scale_fill_ipsum() +
  theme_ipsum() +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  labs(title = "Total Request Amount by Service", 
       x = "Service Type", fill = "Funding Year", 
       subtitle = "2016-2018",
       caption = "Percentages represent Year-over-Year changes") +
  scale_y_continuous(label= dollar_format())


## ---------------------------
### Mean dollars requested by entity type and year
frn %>% 
  group_by(organization_entity_type_name, funding_year) %>% 
  summarise(mean_request = mean(funding_commitment_request, na.rm = T)) %>% 
  mutate(yoy.ammount = mean_request - lag(mean_request),
         yoy_pct.ammount = ((mean_request - lag(mean_request))/mean_request)) %>% 
  ggplot(aes(x = organization_entity_type_name, ## --- ggplot begins here --- ##
             y = mean_request, 
             fill = factor(funding_year), 
             group = factor(funding_year),
             label = percent(yoy_pct.ammount))) + 
  geom_bar(stat = "identity", 
           position = "dodge") +
  labs(title = "Average Request Amount ($) by Organization", 
       x = "Organization Type", 
       y = "Avg. Request", 
       fill = "Funding Year", 
       subtitle = "2016-2018",
       caption = "Percentages represent Year-over-Year changes") +
  geom_text(aes(label = ifelse(is.na(yoy_pct.ammount), "", percent(yoy_pct.ammount))),
            position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  theme_ipsum() +
  scale_fill_ipsum() +
  scale_y_continuous(labels = dollar_format())

### Mean dollars requested by service type and year
frn %>% 
  group_by(form_471_service_type_name, funding_year) %>% 
  summarise(mean_request = mean(funding_commitment_request, na.rm = T)) %>% 
  mutate(yoy.ammount = mean_request - lag(mean_request),
         yoy_pct.ammount = ((mean_request - lag(mean_request))/mean_request)) %>% 
  ggplot(aes(x = form_471_service_type_name, ## --- ggplot begins here --- ##
             y = mean_request, fill = factor(funding_year), 
             group = factor(funding_year),
             label = percent(yoy_pct.ammount))) + 
  geom_bar(stat = "identity", 
           position = "dodge") +
  labs(title = "Average Request Amount ($) by Service", 
       x = "Service Type", 
       y = "Avg. Request", 
       fill = "Funding Year", 
       subtitle = "2016-2018",
       caption = "Percentages represent Year-over-Year changes") +
  geom_text(aes(label = ifelse(is.na(yoy_pct.ammount), "", percent(yoy_pct.ammount))),
            position = position_dodge(width = .9),    # move to center of bars
            vjust = -0.5,    # nudge above top of bar
            size = 3) +
  theme_ipsum() +
  scale_fill_ipsum() +
  scale_y_continuous(labels = dollar_format()) +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20))



## ---------------------------
## Extra Graphics


### States x Funding Requests
ggplot(data = frn) +
  geom_bar(mapping = aes(x = state)) +
  facet_grid(funding_year~.)

### Status -- Everything is pending
ggplot(data = frn) +
  geom_bar(mapping = aes(x = form_471_frn_status_name, 
                         group = factor(funding_year), 
                         fill = factor(funding_year)),
           position = "dodge")

### Contract by Year (dropped Voice)
frn %>% 
  filter(form_471_service_type_name != "Voice") %>% 
  ggplot() +
  geom_bar(aes(x = contract_type_name, group = factor(funding_year), fill = factor(funding_year)),position = "dodge") +
  theme_ipsum() + 
  scale_fill_viridis_d()


### Checking how many double codings there are for voice + internet
frn <- frn %>% 
  mutate(narrative2 = tolower(narrative),
         phone = 0,
         phone = ifelse(grepl('phone', narrative2),1,phone),
         phone = ifelse(form_471_service_type_name == "Voice", 0, phone))

table(frn$phone) # approx 0.5% requested voice and something else, but it was not coded as voice
