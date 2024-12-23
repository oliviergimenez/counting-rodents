# get rainfall through kriging

library(tidyverse)
library(tidyterra)
library(lubridate)
library(sf)
library(KrigR) # https://www.erikkusch.com/courses/krigr/

Dir.Base <- getwd() # identifying the current directory
Dir.Data <- file.path(Dir.Base, "data") # folder path for data

# départements de la région
dpts_occitanie <- st_read("shp/departements-d-occitanie.shp")

# contours de la région
loc_site <- dpts_occitanie %>% 
  st_union() %>%
  st_transform(crs = 4326)

st_bbox(loc_site)

occitanie_raster <- st_bbox(c(xmin = -0.35, 
                              xmax = 5, 
                              ymax = 45.1, 
                              ymin = 42), 
                            crs = st_crs(4326)) %>%
  st_as_sfc() %>%
  terra::vect() %>%
  terra::rast()

poccitanie <- CDownloadS(
  Variable = "total_precipitation",
  DataSet = "reanalysis-era5-land-monthly-means",
  Type = "monthly_averaged_reanalysis",
  DateStart = "2021-01-12 00:00",
  DateStop = "2022-03-31 23:00",
  TZone = "Europe/Paris",
  TResolution = "month",
  TStep = 1,
  Extent = occitanie_raster, # our data.frame with Lat and Lon columns
  Dir = Dir.Data,
  FileName = "Poccitanie",
  API_User = "olivier.gimenez@cefe.cnrs.fr",
  API_Key = "ccb31c25-7603-4cd7-8e88-97c8eb6e9cbd"
)

terra::writeRaster(x = poccitanie,
                  filename = "shp/pluvio.tif",
                  overwrite = TRUE)

poccitanie <- terra::rast("shp/pluvio.tif")

Plot.SpatRast(poccitanie$Poccitanie_11) + 
  geom_sf(data = dpts_occitanie %>% 
            st_union() %>%
            st_transform(crs = 4326), 
             fill = NA)

Plot.SpatRast(poccitanie$Poccitanie_12) + 
  geom_sf(data = dpts_occitanie %>% 
            st_union() %>%
            st_transform(crs = 4326), 
          fill = NA)

Plot.SpatRast(poccitanie$Poccitanie_13) + 
  geom_sf(data = dpts_occitanie %>% 
            st_union() %>%
            st_transform(crs = 4326), 
          fill = NA)

Plot.SpatRast(poccitanie$Poccitanie_14) + 
  geom_sf(data = dpts_occitanie %>% 
            st_union() %>%
            st_transform(crs = 4326), 
          fill = NA)

# extract rainfall per commune
departements <- st_read("shp/departements-d-occitanie.shp")
mask <- departements$nom_officie.2 == "HERAULT" 
herault <- departements[mask, ]
communes <- st_read("shp/georef-france-commune-millesime.shp")
communes_herault <- communes %>% st_intersection(herault)
noms_communes_piegeage <- c("BAILLARGUES",
                            "CANDILLARGUES",
                            "LA GRANDE-MOTTE",
                            "LANSARGUES",
                            "MARSILLARGUES",
                            "MAUGUIO",
                            "SAINT-NAZAIRE-DE-PÉZAN",
                            "TEYRAN",
                            "ENTRE-VIGNES", #"SAINT-CHRISTOL",
                            "SAINT-AUNÈS",
                            "VALERGUES",
                            "SAINT-JUST",
                            "SAINT-GENIÈS-DES-MOURGUES",
                            "PÉROLS",
                            "LUNEL-VIEL",
                            "LUNEL",
                            "SAINT-VINCENT-DE-BARBEYRARGUES")
communes_piegeage <- communes_herault[communes_herault$com_name_up %in% noms_communes_piegeage,]

pluvio_communes <- terra::extract(poccitanie, communes_piegeage) %>%
  group_by(ID) %>%
  summarise(raindec2021 = mean(Poccitanie_11, na.rm = T),
            rainjan2022 = mean(Poccitanie_12, na.rm = T),
            rainfeb2022 = mean(Poccitanie_13, na.rm = T),
            rainmar2022 = mean(Poccitanie_14, na.rm = T),
  )

saveRDS(pluvio_communes, "data/rainfall.rds")
