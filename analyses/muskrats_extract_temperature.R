# get temperature through kriging

library(tidyverse)
library(tidyterra)
library(lubridate)
library(sf)
library(KrigR) # https://www.erikkusch.com/courses/krigr/
library(cbsodataR)


Dir.Base <- getwd() # identifying the current directory
Dir.Data <- file.path(Dir.Base, "data") # folder path for data

# map of the netherlands
cbs_maps <- cbs_get_maps()
netherlands <- cbs_get_sf("gemeente", 2023)

netherlands <- netherlands %>% 
  st_union() %>%
  st_transform(crs = 4326)

plot(netherlands)

st_bbox(netherlands)

netherlands_raster <- st_bbox(c(xmin = 3.3, 
                              xmax = 7.3, 
                              ymax = 54, 
                              ymin = 50), 
                            crs = st_crs(4326)) %>%
  st_as_sfc() %>%
  terra::vect() %>%
  terra::rast()

tnetherlands <- CDownloadS(
  Variable = "2m_temperature",
  DataSet = "reanalysis-era5-land-monthly-means",
  Type = "monthly_averaged_reanalysis",
  # time-window, default set to range of dataset-type
  DateStart = "2013-12-31 00:00",
  DateStop = "2014-03-31 23:00",
  TZone = "CET",
  # temporal aggregation
  TResolution = "month",
  TStep = 1,
  # file storing
  Extent = netherlands_raster,
  FileName = "Tnetherlands",
  # API credentials
  API_User = "olivier.gimenez@cefe.cnrs.fr",
  API_Key = "ccb31c25-7603-4cd7-8e88-97c8eb6e9cbd"
)

Plot.SpatRast(tnetherlands$Tnetherlands_1)
Plot.SpatRast(tnetherlands$Tnetherlands_2)
Plot.SpatRast(tnetherlands$Tnetherlands_3)

terra::writeRaster(x = tnetherlands,
                  filename = "analyses/shp/tempn.tif",
                  overwrite = TRUE)

tnetherlands <- terra::rast("analyses/shp/tempn.tif")

Plot.SpatRast(tnetherlands$Tnetherlands_1) + 
  geom_sf(data = netherlands, 
          fill = NA)

Plot.SpatRast(tnetherlands$Tnetherlands_2) + 
  geom_sf(data = netherlands, 
          fill = NA)

Plot.SpatRast(tnetherlands$Tnetherlands_3) + 
  geom_sf(data = netherlands, 
          fill = NA)

# extract temperature per cities
cbs_maps <- cbs_get_maps()
netherlands <- cbs_get_sf("gemeente", 2023)
netherlands <- netherlands %>%
  st_transform(crs = 4326)

temp_cities <- terra::extract(tnetherlands, netherlands) 
temp_cities <- temp_cities %>%
  left_join(data.frame(ID = 1:nrow(netherlands), statcode = netherlands[["statcode"]]), by = "ID")
temp_cities <- temp_cities %>%
  group_by(statcode) %>%
  summarise(tjan2014 = mean(Tnetherlands_1, na.rm = T),
            tfeb2014 = mean(Tnetherlands_2, na.rm = T),
            tmar2014 = mean(Tnetherlands_3, na.rm = T),
  )

temp_cities <- temp_cities %>% 
  mutate(tjan2014 = weathermetrics::kelvin.to.celsius(tjan2014),
         tfeb2014 = weathermetrics::kelvin.to.celsius(tfeb2014),
         tmar2014 = weathermetrics::kelvin.to.celsius(tmar2014))

saveRDS(temp_cities, "analyses/data/temperature_netherlands.rds")
