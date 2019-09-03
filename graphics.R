dta <- frn %>% 
  select(funding_request_number, funding_year, state, funding_commitment_request, avg_cost_per_ft_of_plant,dis_pct,pricing_confidentiality,total_monthly_recurring_cost,old_funding_request_number,award_date,form_471_service_type_name,bid_count,months_of_service) %>% 
  mutate(old_funding_request_number = ifelse(old_funding_request_number=="",NA,old_funding_request_number),
         repeat_customer = ifelse(is.na(old_funding_request_number), 0, 1))

skimr::skim(dta)

dta %>% janitor::tabyl(funding_year, repeat_customer)
dta %>% janitor::tabyl(funding_year, form_471_service_type_name)

summary(lm(funding_commitment_request ~ bid_count + dis_pct + funding_year + total_monthly_recurring_cost + months_of_service + factor(form_471_service_type_name), data=dta))

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

### Service Type Requests by Year
ggplot(data = frn) +
  geom_bar(aes(x = form_471_service_type_name, group = factor(funding_year), fill = factor(funding_year)),position = "dodge") +
  theme_ipsum() + 
  scale_fill_ipsum() +
  labs(title = "Service Type Requests by Year", 
       x = "Service Type", fill = "Funding Year", 
       subtitle = "by year")




### Entity type by Year
ggplot(data = frn) +
  geom_bar(aes(x = organization_entity_type_name, group = factor(funding_year), fill = factor(funding_year)),position = "dodge") +
  theme_ipsum() + 
  scale_fill_ipsum()

### Funding Requests by Entity type by Request type by Year
ggplot(data = frn) +
  geom_bar(aes(x = organization_entity_type_name, fill = factor(form_471_service_type_name)),position = "dodge") +
  theme_ipsum() + 
  scale_fill_ipsum() + 
  facet_grid(funding_year~.)

### Mean dollars requested by entity type
frn %>% 
  group_by(organization_entity_type_name) %>% 
  summarise(mean_request = mean(funding_commitment_request, na.rm = T))


### Contract by Year (dropped Voice)
frn %>% 
  filter(form_471_service_type_name != "Voice") %>% 
  ggplot() +
  geom_bar(aes(x = contract_type_name, group = factor(funding_year), fill = factor(funding_year)),position = "dodge") +
  theme_ipsum() + 
  scale_fill_viridis_d()

### Entity type by Year (dropped Voice)
frn %>% 
  filter(form_471_service_type_name != "Voice") %>% 
ggplot() +
  geom_bar(aes(x = organization_entity_type_name, group = factor(funding_year), fill = factor(funding_year)),position = "dodge") +
  theme_ipsum() + 
  scale_fill_ipsum()

str(frn$fcdl_letter_date)



### Checking how many double codings there are for voice + internet
frn <- frn %>% 
  mutate(narrative2 = tolower(narrative),
         phone = 0,
         phone = ifelse(grepl('phone', narrative2),1,phone),
         phone = ifelse(form_471_service_type_name == "Voice", 0, phone))

table(frn$phone) # approx 0.5% requested voice and something else, but it was not coded as voice
