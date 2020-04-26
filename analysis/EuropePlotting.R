ggplot() +
  geom_sf(data=europe, fill="white", color="lightgrey") + 
  geom_sf(data=responses, bg='red', color="black", 
          pch=21, size=0.5, 
          stroke=1, alpha=0.3) +
  geom_sf(data=europe, fill=NA, color="lightgrey") + 
  geom_sf(data=germ_buff, fill=NA, lwd=2, color="black") +
  geom_sf(data=germany, fill=NA, lwd=2, color="black") +
  geom_sf_text(data=europe, aes(label=admin), color="black")

europe <- NaturalEarthGlobalAdmin1_10_sf %>%
  filter(geonunit %in% c('Germany', 'France', 'Luxembourg',
                         'Switzerland', 'Austria', 'Poland', 
                         'Czech Republic') )%>%
  st_transform(crs_etrs) %>%
  group_by(admin) %>% 
  summarize() %>%
  rbind(BgNl)

NaturalEarthGlobalAdmin1_10_sf %>%
  select(admin, postal) %>%
  group_by(admin) %>%
  slice(1) %>%
  as_tibble %>%
  select(-geometry) %>%
  edit 

Bg <- NaturalEarthGlobalAdmin1_10_sf %>%
  filter(admin %in% c("Belgium") ) %>%
  st_transform(crs_etrs) %>%
  group_by(admin) %>% 
  summarize() 

Nl <- NaturalEarthGlobalAdmin1_10_sf %>%
  filter(admin %in% c( "Netherlands"), 
         type_en == "Province" )  %>%
  st_transform(crs_etrs) %>%
  group_by(admin) %>% 
  summarize() 

BgNl <- rbind(Bg, Nl)

ggplot(BgNl) +
  geom_sf() 