

#Panama Land Cover: https://stridata-si.opendata.arcgis.com/maps/SI::forest-cover-and-land-use-2021-for-panama-1/about
#DEM: https://stridata-si.opendata.arcgis.com/maps/0815a202e591481c88b3037ff3ab4df4/about
#CBOTB: https://www.sinia.gob.pa/index.php/extensions/datos-abiertos-y-geoservicios


library(terra)
library(tidyterra)
library(tidyverse)
library(here)
library(sf)

gps <- read.csv(here("processedData", "GPS_allbats20240418.csv"))
gps_sf <- st_as_sf(gps, coords = c("location.lon", "location.lat"), crs = 4326)
gps_utm_sf <- st_transform(gps_sf, crs = 32617) #transform to UTM

panama_sf <- st_read(here("rawData", "LandCoverEtc", "Panama_Province_Boundaries-shp", "Province_Boundaries.shp")) #ESPG 32617  WGS 84 / UTM zone 17N
bocas_sf <- panama_sf %>% dplyr::filter(NOMBRE == "Bocas del Toro")#UTM zone 17 NAD84 32617
ggplot(bocas_sf)+
  geom_sf(fill = "white")+
  theme_classic()

dem <- rast(here("rawData", "LandCoverEtc", "Pma_DEM_30m", "Pma_DEM_30m.tif")) #EPSG 4326
lc <- rast(here("rawData", "LandCoverEtc", "ForestCoverLandUse_2021_25k", "ForestCoverLandUse_2021_25k.tif")) #WGS 84 / UTM zone 17N (EPSG:32617) 
plot(lc)
plot(dem)

#landcover names:
# Code	Name_Eng
# 1	Mature Broadleaf Forest
# 2	Secondary Mixed Broadleaf Forest
# 3	Mangrove Forest
# 4	Orey Forest
# 5	Cativo Forest
# 6	Rafia Forest
# 7	Coniferous Forest
# 8	Broadleaf Forest
# 9	Shrubs and Bushes
# 10	Herbaceous Vegetation
# 11	Flooded Vegetation
# 12	Rocks and Bare Soils
# 13	Sand Beaches
# 14	Coffee
# 15	Citrus
# 16	Oil Palm
# 17	Banana
# 18	Permanent Crops
# 19	Rice
# 20	Sugar Cane
# 21	Mixed Horticulture
# 22	Corn
# 23	Pineapple
# 24	Other Annual Crops
# 25	Heterogeneous Area Agricultural Production
# 26	Pasture
# 27	Water Bodies
# 28	Urban Areas
# 29	Infrastructure
# 30	Mining
# 31	Aquaculture Ponds
# 32	Salt Mine
# 33	Albinas


#I will transform the DEM to UTM projection. In general, transforming rasters is bad practice, but we need to bring everything into the same projection and alignment. This may take some time since it's for the entire country sampled at 30 m.
dem_utm <- terra::project(dem, crs(lc))


#Crop rasters to Bocas
lc_bocas <- terra::crop(lc, st_bbox(bocas_sf))
dem_bocas <- terra::crop(dem_utm, st_bbox(bocas_sf))

ggplot()+
  geom_spatraster(dem_bocas, mapping = aes())+
  geom_sf(gps_utm_sf, mapping = aes(color = batID), alpha = 0.2)


#Crop down to the GPS tracks + a bit. 

lc_region <- terra::crop(lc_bocas, terra::ext(325376, 380000, 1000000, 1050000))
dem_region <- terra::crop(dem_bocas, terra::ext(325376, 385000, 1000000, 1050000))
plot(dem_region)
ggplot()+
  geom_spatraster(lc_region, mapping = aes(), inherit.aes = FALSE)+
  geom_sf(gps_utm_sf, mapping = aes(), alpha = 0.2, inherit.aes = FALSE)

ggplot()+
  geom_spatraster(dem_region, mapping = aes(), inherit.aes = FALSE)+
  geom_sf(gps_utm_sf, mapping = aes(), color = "white", alpha = 0.2, inherit.aes = FALSE)

#Annotate GPS with DEM elevation & Landcover
gps_lc <- extract(lc_region, gps_utm_sf)


