knitr::kable(yoy,
             col.names = c("Funding Year",
                           "Requests (N)",
                           "Ammount Requested ($)",
                           "Y-o-Y Requests (N)",
                           "Y-o-Y Requests (%)",
                           "Y-o-Y Ammount Requested ($)",
                           "Y-o-Y Ammount Requested (%)"))



ggplot(yoy.state, aes(x = state, y = yoy.ammount, fill = factor(funding_year))) +
  geom_bar(position = "dodge",
           stat = "identity")

filter(yoy.state, state == "HI")



library(sp)
library(sf)
library(rnaturalearth)
library(tmap)
library(grid)

# Call world shapefile from the rnaturalearth package as a sf object
states_shp <- ne_states(returnclass = "sf",
                       country = "United States of America")


# Setup projections
US.proj <- "+proj=aea +lat_1=33 +lat_2=45 +lat_0=39 +lon_0=-96 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m"
AK.proj <- "+proj=aea +lat_1=55 +lat_2=65 +lat_0=50 +lon_0=-154 +x_0=0 +y_0=0 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs"
HI.proj <- "+proj=aea +lat_1=8 +lat_2=18 +lat_0=13 +lon_0=-153 +x_0=0 +y_0=0 +ellps=GRS80 +datum=NAD83 +units=m +no_defs"


yoy.state_geo <- left_join(states_shp,yoy.state, by = c("postal" = "state"))

US_geo <- yoy.state_geo %>% 
  filter(name.y != "Alaska" & name.y != "Hawaii")

AK_geo <- yoy.state_geo %>% 
  filter(name.y == "Alaska")

HI_geo <- yoy.state_geo %>% 
  filter(name.y == "Hawaii") %>% 
  st_crop(., c(xmin=-162.62, xmax=-153.87, ymin=18.05, ymax=23.68))

### ------------------------------------------------------------

us_states_range = st_bbox(US_geo)[4] - st_bbox(US_geo)[2]
hawaii_range = st_bbox(HI_geo)[4] - st_bbox(HI_geo)[2]
alaska_range = st_bbox(AK_geo)[4] - st_bbox(AK_geo)[2]

us_states_hawaii_ratio = hawaii_range / us_states_range
us_states_alaska_ratio = alaska_range / us_states_range

### ------------------------------------------------------------

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


m <- US_geo %>% 
  filter(funding_year==2018) %>% 
  select(ammount.per.person,everything()) %>% 
  tm_shape(., projection = US.proj) +
  tm_layout(frame = FALSE,
            main.title = "E-Fund Dollars Requested per Person, 2018") +
  tm_fill(col = "ammount.per.person",
          breaks = c(1,5,8,10,20,320),
          title = "Dollars Requested per Person") +
  tm_borders()
tmap_leaflet(m)

US_geo %>% 
  filter() %>% 
  tm_shape(., projection = US.proj) +
  tm_layout(frame = FALSE) +
  tm_fill(col = "req.state.pct") +
  tm_borders() +
  tm_facets(by="funding_year")


US_geo %>% 
  filter(funding_year != 2016) %>% 
  tm_shape(., projection = US.proj) +
  tm_layout(frame = FALSE) +
  tm_fill(col = "yoy_pct.ammount",
          style = "quantile",
          midpoint = NA) +
  tm_borders() +
  tm_facets(by="funding_year")

US_geo %>% 
  filter(funding_year != 2016) %>% 
  tm_shape(., projection = US.proj) +
  tm_fill(col = "yoy.ammount",
          style = "quantile",
          midpoint = NA) +
  tm_borders() +
  tm_text(text = "yoy_pct.ammount",
          size = .6,
          shadow = TRUE) +
  tm_facets(by="funding_year") +
  tm_layout(legend.outside.position = "right" , legend.outside.size = .1)



## ggplot testing
US_geo %>% 
  filter(funding_year == 2017) %>%
  ggplot() +
  geom_sf(aes(fill = yoy_pct.ammount)) +
  viridis::scale_fill_viridis() +
  theme(panel.background = element_rect(fill = "transparent"),
        panel.grid.major = element_line(colour = "transparent"),
        axis.text.x = element_blank(), 
        axis.text.y = element_blank(), 
        axis.ticks = element_blank()) +
  coord_sf(crs = US.proj)
  


summary(US_geo$yoy_pct.ammount)

AK_map <- tm_shape(AK_geo, projection = AK.proj) +
  tm_polygons() +
  tm_layout(title = "Alaska",
            frame = F,
            bg.color = NA,
            title.position = c("left","TOP"))

HI_map <- tm_shape(HI_geo, projection = HI.proj) +
  tm_polygons() +
  tm_layout(title = "Hawaii", frame = FALSE, bg.color = NA, 
            title.position = c("LEFT", "BOTTOM"))

### ------------------------------------------------------------

grid.newpage()
pushViewport(viewport(layout = grid.layout(2, 1,
                                           heights = unit(c(us_states_alaska_ratio, 1), "null"))))
print(AK_map, vp = viewport(layout.pos.row = 1))
print(US_map, vp = viewport(layout.pos.row = 2))
print(HI_map, vp = viewport(x = 0.3, y = 0.07,
                                height = us_states_hawaii_ratio / sum(c(us_states_alaska_ratio, 1))))
grid.lines(x = c(0, 1), y = c(0.55, 0.55), gp = gpar(lty = 2))
grid.lines(x = c(0, 0.5), y = c(0.33, 0), gp = gpar(lty = 2))






library(statebins)

statebins_continuous(state_data = filter(yoy.state_geo, funding_year == 2018), state_col = "postal",
                     text_color = "white", value_col = "yoy.req",
                     brewer_pal="RdYlBu", font_size = 3,
                     legend_title="yoy_pct.ammount")
