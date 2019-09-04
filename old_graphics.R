##### old_graphics.R

## Graphs that have been updated and replaced

### Service Type Requests by Year
ggplot(data = frn) +
  geom_bar(aes(x = form_471_service_type_name, group = factor(funding_year), fill = factor(funding_year)),position = "dodge") +
  theme_ipsum() + 
  scale_fill_ipsum() +
  labs(title = "Requests by Service Type", 
       x = "Service Type", fill = "Funding Year", 
       subtitle = "2016-2018") +
  scale_x_discrete(labels = function(x) str_wrap(x, width = 20)) +
  scale_y_continuous(label=comma)


ggplot(data = frn) +
  geom_bar(aes(x = organization_entity_type_name, group = factor(funding_year), fill = factor(funding_year)),position = "dodge") +
  theme_ipsum() + 
  scale_fill_ipsum() +
  labs(title = "Requests by Organization Type", 
       x = "Organization Type", fill = "Funding Year", 
       subtitle = "2016-2018") +
  scale_y_continuous(label=comma)