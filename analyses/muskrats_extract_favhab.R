library(tidyverse)
library(lubridate)
library(sf)

# study area

departements <- st_read("shp/departements-d-occitanie.shp")
mask <- departements$nom_officie.2 == "HERAULT" 
herault <- departements[mask, ]
communes <- st_read("shp/georef-france-commune-millesime.shp")
communes_herault <- communes %>% st_intersection(herault)

unique(communes_herault$com_name_lo)
communes_herault %>% st_area()

# read in CLC data
clc2012 <- st_read("shp/CLC12_FR_RGF.shp")

# extract zones humides et surfaces en eau
# https://wiki.openstreetmap.org/wiki/FR:Corine_Land_Cover/Nomenclature
zh_raw <- clc2012 %>%
  filter(CODE_12 == '411' | 
           CODE_12 == '412' | 
           CODE_12 == '421' | 
           CODE_12 == '422' | 
           CODE_12 == '423' | 
           CODE_12 == '511' | 
           CODE_12 == '512' | 
           CODE_12 == '521' | 
           CODE_12 == '522' | 
           CODE_12 == '523') #<<
zh_raw

# zones humides dans les communes de l'hérault
zh <- zh_raw %>%
  st_transform(crs = st_crs(communes_herault)) %>%
  st_intersection(communes_herault)
#saveRDS(zh, file = "data/zh.rds")
#zh <- readRDS("data/zh.rds")

zh %>% st_area()
zh %>% group_by(com_name_lo) %>% st_area()

# river data (BDtopo at https://geoservices.ign.fr/bdtopo)
Herault <- sf::st_read("data/BDTOPO/BDTOPOHERAULT/1_DONNEES_LIVRAISON_2022-12-00159/BDT_3-3_SHP_LAMB93_D034-ED2022-12-15/HYDROGRAPHIE/COURS_D_EAU.shp")

# bind river
river_lines <- Herault %>%
  sf::st_transform(crs = st_crs(communes_herault)) %>%
  sf::st_simplify()

# checks
nrow(river_lines %>%
       filter(IMPORTANCE %in% c(3,4)))

# rivières des communes de l'hérault
rivers34 <- river_lines %>% 
  st_intersection(communes_herault)
#st_write(rivers34, "shp/rivers.shp")
#st_read("shp/rivers.shp")

# viz rivers
ggplot() +
  geom_sf(data = herault, color = "black", lwd = 1) +
  geom_sf(data = communes_herault, lwd = .3, color = "black", fill = "white") +
  geom_sf(data = rivers34, color = "lightblue") + 
  theme_light()

# viz zh
ggplot() +
  geom_sf(data = herault, color = "black", lwd = 1) +
  geom_sf(data = communes_herault, lwd = .3, color = "black", fill = "white") +
  geom_sf(data = zh) + 
  theme_light()

# sum zh over all study sites
zhherault <- zh %>% 
  mutate(area = st_area(.)) %>%
  group_by(com_name_lo) %>% # groups a data frame by variables #<< 
  summarise(areazh = sum(area)) %>% # perform group-wise summaries #<<
  as_tibble() %>%
  select(-geometry)
zhherault

# sum rivers over all study sites
riversherault <- rivers34 %>% 
  st_buffer(dist = units::set_units(50,  m)) # mettre un buffer de 50m autour
sf_use_s2(FALSE) # https://github.com/r-spatial/sf/issues/1881
riversherault <- riversherault %>%
  mutate(area = st_area(.)) %>%
  group_by(com_name_lo) %>% # groups a data frame by variables #<< 
  summarise(arearivers = sum(area)) %>% # perform group-wise summaries #<<
  as_tibble() %>%
  select(-geometry)
riversherault

# calculate cover
habfav <- communes_herault %>% 
  mutate(area = st_area(.)) %>%
  left_join(riversherault, by = 'com_name_lo') %>% 
  left_join(zhherault, by = 'com_name_lo') %>%
  mutate(area = as.numeric(area),
         areazh = as.numeric(areazh),
         arearivers = as.numeric(arearivers)
         ) %>%
  mutate(areazh = if_else(is.na(areazh), 0, areazh)) %>%
  mutate(arearivers = if_else(is.na(arearivers), 0, arearivers)) %>%
  mutate(.before = 1,
         cover = (areazh + arearivers) / area) %>%
  mutate(cover = if_else(cover > 1, 1, cover))

st_write(habfav, "shp/habfav.shp")

