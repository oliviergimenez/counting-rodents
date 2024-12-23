library(tidyverse)
library(sf)
library(cbsodataR)
library(ubms)

# faire un stacked model with site random effect
# faire un modèle avec RSR

# read in muskrat data
muskrats <- read_delim("data/occurrence-muskrats.txt")
head(muskrats)

# extract data for January, February and March
muskrats_3month <-  muskrats %>%
mutate(
  Start_Date = as.Date(sub("/.*", "", eventDate)),  # extract start date
  End_Date = as.Date(sub(".*/", "", eventDate))    # extract end date
) %>%
  rowwise() %>%  # apply operations row-wise
  # calculate the month and year with maximum overlap
  mutate(
    interval_dates = list(seq(Start_Date, End_Date, by = "day")),  # generate sequence of dates in the interval
    Overlapping_Month = {
      # find the month for each date
      month_names <- format(interval_dates, "%B")
      
      # determine the month with the maximum count
      max_month <- month_names %>%
        table() %>%
        which.max() %>%
        names()
      
      max_month
    },
    Overlapping_Year = {
      # find the year for each date
      year_values <- format(interval_dates, "%Y")
      
      # determine the year with the maximum count
      max_year <- year_values %>%
        table() %>%
        which.max() %>%
        names()
      
      max_year
    }
  ) %>%
  ungroup() %>%
  select(-interval_dates)

# create sf object
muskrats_sf <- muskrats_3month %>%
  filter(Overlapping_Month %in% c("January", "February", "March")) %>%
  group_by(year, Overlapping_Month, decimalLatitude, decimalLongitude) %>%
  summarise(n = sum(individualCount)) %>%
  mutate(month = Overlapping_Month) %>%
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), 
                        crs = 4326, 
                        agr = "constant")

# map by year after 2010
muskrats_sf %>% 
  filter(year > 2010) %>%
  ggplot() +
  geom_sf(aes(color = log(n))) + 
  scale_color_viridis_c() +
  labs(color = "number of removed \nmuskrats (log)") +
  facet_wrap(~year) +
  theme_void() 

# map by month for year 2010
muskrats_sf %>% 
  filter(year == 2010) %>%
  ggplot() +
  geom_sf(aes(color = log(n))) + 
  scale_color_viridis_c() +
  labs(color = "number of removed \nmuskrats (log)") +
  facet_wrap(~month) +
  theme_void() 

# get map of netherlands w/ municipalities
# https://cran.r-project.org/web/packages/cbsodataR/vignettes/maps.html
cbs_maps <- cbs_get_maps()
netherlands <- cbs_get_sf("gemeente", 2023)

netherlands <- netherlands %>% 
  st_transform(crs = st_crs(muskrats_sf))

# spatial join to associate points with polygons
points_with_polygons <- st_join(muskrats_sf, 
                                netherlands, 
                                join = st_within)

# summarize counts
netherlands_with_counts <- points_with_polygons %>%
  group_by(statcode, month, year) %>% 
  summarize(
    total_n = sum(n, na.rm = TRUE),  
    .groups = "drop"
  )

# create all perm of month, year and municipalities
all_months_years <- expand.grid(
  month = unique(muskrats_sf$month), 
  year = unique(muskrats_sf$year),
  statcode = netherlands$statcode
)

# fill in missing combinations of `statcode`, `month`, and `year`
netherlands_with_counts <- all_months_years %>%
  left_join(netherlands_with_counts %>% 
              st_drop_geometry(), # temporarily drop geometry to avoid mismatches
            by = c("statcode", "month", "year")) %>%
  mutate(total_n = ifelse(is.na(total_n), 0, total_n)) %>%
  right_join(netherlands, by = "statcode") %>%  # join back to netherlands
  st_as_sf()

# save files
st_write(, "shp/netherland_muskrats.shp")

# read file
netherlands_with_counts <- st_read("shp/netherland_muskrats.shp")

# map by month
netherlands_with_counts %>% 
  ggplot() + 
  geom_sf(aes(fill=log(total_n)), color="#FFFFFF99") +
  scale_fill_viridis_c(direction = -1, option = "A")+ 
  labs(fill = "number of removed \nmuskrats (log)") +
  facet_wrap(~month) +
  theme_void()

# map by year (sum over months)
netherlands_with_counts %>% 
  group_by(year, statcode) %>%
  summarise(n = sum(total_n)) %>%
  ggplot() + 
  geom_sf(aes(fill=log(n)), color="#FFFFFF99") +
  scale_fill_viridis_c(direction = -1)+ 
  labs(fill = "number of removed \nmuskrats (log)") +
  facet_wrap(~year) +
  theme_void()


netherlands

# hydroriver data from https://www.hydrosheds.org/
rivers <- st_read("shp/HydroRIVERS_v10_eu.shp")

# Find intersections
intersect_indices <- st_intersects(rivers, netherlands, sparse = TRUE)

# Subset rivers based on intersecting indices
rivers_netherlands <- rivers[lengths(intersect_indices) > 0, ]

ggplot() +
  geom_sf(data = netherlands, fill = "lightgray", color = "black") +
  geom_sf(data = rivers_netherlands, color = "blue", size = 0.5) +
  coord_sf() +
  theme_minimal() +
  labs(
    title = "River Network in the Netherlands",
    caption = "Data: HydroSHEDS"
  ) +
  ggspatial::annotation_scale(location = "bl", width_hint = 0.5) +
  ggspatial::annotation_north_arrow(location = "br", which_north = "true", style = ggspatial::north_arrow_fancy_orienteering)


muskrats_ubms <- netherlands_with_counts %>%
  as_tibble %>%
  select(-geometry, - statnaam) %>%
  mutate(site = dense_rank(statcode)) %>% # renumber cities
  pivot_wider(
    names_from = month,
    values_from = total_n) %>% 
  select(January, February, March, site, year) %>%
  filter(year > 2010)



y <- as.matrix(muskrats_ubms[,c(1,2,3)])

# replace 0 by NAs or rows with only 0s by NAs

# site.covs <- data.frame(temp = (temp - mean(temp))/sd(temp),
#                         rain = (rain - mean(rain))/sd(rain),
#                         offset = fh)

site.covs <- data.frame(site = as_factor(muskrats_ubms$site))

time <- matrix(c('january','february','march'),
               nrow = nrow(y),
               ncol = ncol(y), 
               byrow = TRUE)

umf <- unmarkedFrameMPois(y = y, 
                          siteCovs = site.covs,
                   #     obsCovs = list(time = time),
                        type = "removal")

head(umf)

fit <- stan_multinomPois(~1 + (1|site) ~1 + (1|site), 
                         data = umf, 
                         chains = 2, 
                         iter = 1000,
                         prior_intercept_state = normal(0, 1.5),
                         prior_coef_state = normal(0, 1.5))
fit

traceplot(fit, pars = c("beta_state", "beta_det"))

plot_posteriors(fit)#, pars=c("beta_state[SistemaSilvopastoril]"))

#lambda <- extract(fit,"beta_state")[[1]]
#hist(lambda, freq = FALSE)

plot_marginal(fit, submodel = "state")

plot_marginal(fit, submodel = "det")

# abundance
exp(coef(fit)[1])

# detection
plogis(coef(fit)[3])

# faire courbe avec 
extract(fit, "beta_state")

predict(fit, submodel="state")
apply(y, 1, sum)

ran <- ranef(fit, submodel="state")
head(ran$site[[1]])

ran <- ranef(fit, submodel="state", summary=TRUE)
head(ran$site[[1]])

#-------- spatial

# select a year and do https://cran.r-project.org/web/packages/ubms/vignettes/spatial-models.html#ref-Broms_2014

#muskrats: mettre covariables temp, mois et fav hab (choisir qqs années avec des 0) ; faire modèle spatial

