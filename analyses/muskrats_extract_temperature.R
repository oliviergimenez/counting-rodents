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

netherlands_raster <- netherlands %>% 
  terra::vect() %>%
  terra::rast()

tnetherlands <- CDownloadS(
  Variable = "2m_temperature",
  DataSet = "reanalysis-era5-land-monthly-means",
  Type = "monthly_averaged_reanalysis",
  # time-window, default set to range of dataset-type
  DateStart = "1987-01-01 00:00",
  DateStop = "2014-03-31 24:00",
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
Plot.SpatRast(tnetherlands$Tnetherlands_327)

terra::writeRaster(x = tnetherlands,
                  filename = "analyses/shp/tempnetherlands.tif",
                  overwrite = TRUE)

tnetherlands <- terra::rast("analyses/shp/tempnetherlands.tif")

Plot.SpatRast(tnetherlands$Tnetherlands_1) + 
  geom_sf(data = netherlands, 
          fill = NA)

Plot.SpatRast(tnetherlands$Tnetherlands_327) + 
  geom_sf(data = netherlands, 
          fill = NA)

# extract temperature per cities
cbs_maps <- cbs_get_maps()
netherlands <- cbs_get_sf("gemeente", 2023)
netherlands <- netherlands %>%
  st_transform(crs = 4326)

# Define total years and months per year
years <- length(1987:2014)
months_per_year <- 12
# Generate indices for the first three months of each year
indices <- unlist(lapply(0:(years - 1), function(i) {
  # Offset by year (12 * i) and add the first three months (1, 2, 3)
  1:3 + (i * months_per_year)
}))

temp_cities <- terra::extract(tnetherlands, netherlands) 
temp_cities <- temp_cities %>%
  left_join(data.frame(ID = 1:nrow(netherlands), 
                       statcode = netherlands[["statcode"]]), by = "ID")

# Generate the sequence of years
start_year <- 1987
end_year <- 2014
years <- seq(start_year, end_year)

# Assuming temp_cities has monthly columns like Tnetherlands_1, Tnetherlands_2, ..., Tnetherlands_336
temp_cities_long <- temp_cities %>%
  pivot_longer(
    cols = starts_with("Tnetherlands_"),  # Select all temperature columns
    names_to = "month_index",
    names_prefix = "Tnetherlands_",
    values_to = "temperature"
  ) %>%
  mutate(month_index = as.numeric(month_index)) 

# Filter for indices of interest and calculate the mean
result <- temp_cities_long %>%
  filter(month_index %in% indices) %>%  # Filter for relevant months
  mutate(
    year = years[(month_index - 1) %/% 12 + 1]  # Map month_index to calendar year
  ) %>%
  group_by(statcode, year) %>%  # Group by year and statcode
  summarise(
    mean_temperature = mean(temperature, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(mean_temperature = weathermetrics::kelvin.to.celsius(mean_temperature))

saveRDS(result, "analyses/data/temperature_netherlands_allperiod.rds")
