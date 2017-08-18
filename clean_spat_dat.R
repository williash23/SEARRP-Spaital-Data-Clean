#  Sara Williams
#  June 6, 2017
#  Importing availble Sabah spatial data and species distribution data, 
#  transforming to UTM and croping to only Sabah.

library(rgdal)
library(raster)
library(dplyr) 
library(maptools)
library(rgeos)
library(sf)
## install issue use: install.packages('xxxx', repos = 'http://cran.cnr.berkeley.edu/')



#  General map and extent layers

#  Basic information on the bounding box of Sabah (could be used for cropping) and the CRS that can be used 
#   to link spatial data layers.
sabah_bb_latlong <- extent(115.1531, 119.6081, 3.981738, 7.857259)
sabah_bb_latlong_p <- as(sabah_bb_latlong, 'SpatialPolygons')
sabah_bb_latlong_sf <- st_as_sf(sabah_bb_latlong_p)
crs(sabah_bb_latlong_p) <- "+proj=longlat +datum=WGS84 +no_defs"
sabah_bb_UTM <- extent(440338.32, 869413.14, 294954.50, 787611.81)
sabah_UTM <- CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
# OR epsg = 32650

#  Generate base map of Sabah polygons
border_my <- shapefile("C:/Users/saraw/Documents/SEARRP/raw_spat_data/country_borders/MYS_adm2.shp")
border_my$NAME_1 <- as.factor(border_my$NAME_1)
border_sabah <- border_my[border_my@data$NAME_1 == "Sabah",]
border_sabah_t <- spTransform(border_sabah, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
border_sabah_d <- unionSpatialPolygons(border_sabah_t, border_sabah_t$ID_0)
border_sabah_sf <-  st_as_sf(border_sabah_d)

#  Generate base map of Sarawak (for mapping)
border_sarawak <- border_my[border_my@data$NAME_1 == "Sarawak",]
border_sarawak_t <- spTransform(border_sarawak, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
border_sarawak_d <- unionSpatialPolygons(border_sarawak_t, border_sarawak_t$ID_0)
border_sarawak_sf <-  st_as_sf(border_sarawak_d)

#  Generate base map of Indonesia (for mapping)
border_in <- shapefile("C:/Users/saraw/Documents/SEARRP/raw_spat_data/country_borders/IDN_adm2.shp")
border_in$NAME_1 <- as.factor(border_in$NAME_1)
border_kali <- border_in[border_in@data$NAME_1 == "Kalimantan Utara",]
border_kali_t <- spTransform(border_kali, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
border_kali_d <- unionSpatialPolygons(border_kali_t, border_kali_t$ID_0)



#  Development layers

#  Logging road data from Gaveau et al. 2014, accessed at:
#   https://data.cifor.org/dataset.xhtml?persistentId=doi:10.17528/CIFOR/DATA.00049
log_rds <- shapefile("C:/Users/saraw/Documents/SEARRP/raw_spat_data/roads/REGIONBorneo_LoggingRoad_1970to2010_CIFOR.shp")
log_rds_t <- spTransform(log_rds, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
log_rds_c <- crop(log_rds_t, border_sabah_d)
log_rds_sf <- st_as_sf(log_rds_c) %>%
	st_intersection(st_as_sf(border_sabah_d))

### PA file does not currently work
#  Protected area and other land ownership data from M. Strimas via email or Gaveau et al. 2014, accessed at:
#   https://data.cifor.org/dataset.xhtml?persistentId=doi:10.17528/CIFOR/DATA.00049
#  This (obtained from M. Strimas) appears to be a plantation land ownership shapefile, not protected areas
pa <- shapefile("C:/Users/saraw/Documents/SEARRP/raw_spat_data/land_ownership/#####.shp")
pa_c_tmp <- crop(pa, sabah_bb_latlong)
pa_t <- spTransform(pa_c_tmp, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
pa_c <- crop(pa_t, border_sabah_d)

#  This file (obtained from M. Strimas via email) is from 2010
land_own <- shapefile("C:/Users/saraw/Documents/SEARRP/raw_spat_data/land_ownership/REGIONBorneo_IndustrialPlantation_2010_CIFOR.shp")
land_own_t <- spTransform(land_own, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
land_own_c <- crop(land_own_t, border_sabah_d)

#  This file (downloaded from Gaveau et al. 2016) has error when trying to crop to just the Sabah region
orig_land_cov <- shapefile("C:/Users/saraw/Documents/SEARRP/raw_spat_data/land_ownership/REGBorneo_OriginOfLandConvertedToITPAndIOPPComplexTrajectory_1973to2016_CIFOR/REGBorneo_OriginOfLandConvertedToITPAndIOPPComplexTrajectory_1973to2016_CIFOR.shp")
orig_land_cov_b <- gBuffer(land_own, byid=TRUE, width=0)
orig_land_cov_t <- spTransform(orig_land_cov_b, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
orig_land_cov_c <- crop(orig_land_cov_t, border_sabah_d)



#  Environmental layers

#  Forest cover data from Gaveau et al. 2016, accessed at: 
#   https://data.cifor.org/dataset.xhtml?persistentId=doi:10.17528/CIFOR/DATA.00049
for_cov <- raster("C:/Users/saraw/Documents/SEARRP/raw_spat_data/forest_cover/REGBorneo_ForestCover_2016_CIFOR.tif")
for_cov_c <- mask(for_cov, border_sabah_d)

#  HydroSHEDS river and basin data, accessed at: http://hydrosheds.org/page/overview
hydro_vec <- shapefile("C:/Users/saraw/Documents/SEARRP/raw_spat_data/hydro/hydroSHEDS/as_riv_15s/as_riv_15s.shp")
hydro_vec_t <- spTransform(hydro_vec, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
hydro_vec_c <- crop(hydro_vec_t, border_sabah_d)

hybas <- shapefile("C:/Users/saraw/Documents/SEARRP/raw_spat_data/hydro/hybas_lake_as_lev12_v1c/hybas_lake_as_lev12_v1c.shp")
hybas_t <- spTransform(hybas, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
hybas_c <- crop(hybas_t, border_sabah_d)


#  These rasters are from "Global Multi-resolution Terrain Elevation Data 2010", which replaces GTOPO30
#   accessed at: https://lta.cr.usgs.gov/GMTED2010
elev_250m <- raster("C:/Users/saraw/Documents/SEARRP/raw_spat_data/dem/GMTED2010/7_5_arc_sec/10s090e_20101117_gmted_mea075.tif")
elev_250m_t <- projectRaster(elev_250m, crs = "+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
elev_250m_c <- crop(elev_250m_t, border_sabah_d)

#   Sabah coastline
#   Create box of area of Sabah where border is not coastline
y <- c(4.968642, 4.166413, 3.317313, 4.337937, 4.968642)
x <- c(115.425960, 117.898442, 117.811867, 114.821043, 115.425960)
coast_border_latlong <- SpatialLines(list(Lines(Line(cbind(x,y)), ID="a")))
crs(coast_border_latlong) <- "+proj=longlat +datum=WGS84 +no_defs"
coast_border <- spTransform(coast_border_latlong, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
coast_border_sf <- st_as_sf(coast_border)
coast_border_p_sf <- st_polygonize(coast_border_sf)
#   Determine the difference between the entire Sabah border and border without coastline
coast_diff <- st_difference(border_sabah_sf, coast_border_p_sf)
#   Create a strip of land that is the coastline ONLY
border_sabah_l <- as(border_sabah_d, 'SpatialLines')
border_sabah_b <- buffer(border_sabah_l, 10)
border_sabah_sf_b <- st_as_sf(border_sabah_b)
coast_str <- st_intersection(border_sabah_sf_b, coast_diff)



#  Species distribution layers
#   Distinguished only by spatial location - coarse distributions from IUCN

#  Mammals from IUCN spatail data download center (http://www.iucnredlist.org/technical-documents/spatial-data)
mammals <- shapefile("C:/Users/Sara/Desktop/SEARRP/spat_dat/TERRESTRIAL_MAMMALS/TERRESTRIAL_MAMMALS.shp")
mammals_c_tmp <- crop(mammals, sabah_bb_latlong)
mammals_t <- spTransform(mammals_c_tmp, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
mammals_c <- crop(mammals_t, border_sabah_d)

#  Amphibians from IUCN spatail data download center (http://www.iucnredlist.org/technical-documents/spatial-data)
amphs <- shapefile("C:/Users/Sara/Desktop/SEARRP/spat_dat/AMPHIBIANS/AMPHIBIANS.shp")
amphs_c_tmp <- crop(amphs, sabah_bb_latlong)
amphs_t <- spTransform(amphs_c_tmp, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
amphs_c <- crop(amphs_t, border_sabah_d)

#  Birds from BirdLife (Birds of the World)
rl_birds <- st_read(dsn="C:/Users/saraw/Documents/SEARRP/raw_spat_dat/BOTW.gdb", layer = "All_Species") 
#  Join spatial data set (which contains birds from all countries) to data frame of MY species (
#   all taxa, all spp other than least concern) only, to obtain a data frame of bird species that only occur in MY 
#   Must do this first step because the spatial object is SO large.
birds_j <- rl_birds %>%
	inner_join(my_species_rl, by = c("SCINAME" = "result.scientific_name"))
birds_c_tmp <- st_intersection(birds_j, sabah_bb_latlong_sf)
birds_t <- st_transform(birds_c_tmp, crs = 32650)
birds_c_tmp <- st_intersection(birds_t, border_sabah_d_sf)
birds_c <-as(birds_c_tmp, "Spatial")



#  Save these files to work with later

save(hydro_vec_c, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/hydro_vec_c.Rdata")
save(coast_str, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/coast_str.Rdata")
save(log_rds_sf, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/log_rds_sf.Rdata")
save(for_cov_c, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/for_cov_c.Rdata")
save(border_sabah_d, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/border_sabah_d.Rdata")
save(border_sarawak_d, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/border_sarawak_d.Rdata")
save(border_kali_d, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/border_kali_d.Rdata")

save(mammals_c, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/mammals_c.Rdata")
save(amphs_c, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/amphs_c.Rdata")
save(birds_c, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/birds_c.Rdata")
writeOGR(mammals_c,"C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj", "mammals_c", driver="ESRI Shapefile")
writeOGR(amphs_c,"C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj", "amphs_c", driver="ESRI Shapefile")
writeOGR(birds_c,"C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj", "birds_sp", driver="ESRI Shapefile")







#  Other potential spatial data sources
#  More coarse resolution GMTED2010 data
# elev_1k <- raster("C:/Users/sara.williams/Desktop/SEARRP/spat_dat/dem/GMTED2010/30_arc_sec/10s090e_20101117_gmted_mea300.tif")
# elev_1k_t <- projectRaster(elev_1k, crs = "+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
# elev_1k_c <- crop(elev_1k_t, border_sabah_d)

# #  Hydro1k elevation data: This is fewer rivers and streams than represented by hydroSHEDS
# #   Convert the e00 to SpatialLines
# e00 <- ("C:/Users/saraw/Documents/SEARRP/raw_spat_data/hydro/hydro1k/au_str.e00")
# e00toavc(e00, "au_str")
# arc <- get.arcdata(".", "au_str")
# hydro1k <- st_as_sf(ArcObj2SLDF(arc))
# st_crs(hydro1k) = "+proj=laea +lat_0=-15 +lon_0=135 +x_0=0 +y_0=0 +a=6371007.181 +b=6371007.181 +units=m +no_defs "
# hydro1k_t_tmp <- st_transform(hydro1k, "+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
# hydro1k_t_tmp_sp <- as(hydro1k_t_tmp, "Spatial")
# hydro1k_c_tmp <- crop(hydro1k_t_tmp_sp, border_sabah_d)

# #  Surrounding ocean polygon
# ocean <- shapefile("C:/Users/saraw/Documents/SEARRP/raw_spat_data/ocean/ne_10m_ocean.shp")
# ocean_c_tmp <- crop(ocean, sabah_bb_latlong)
# ocean_t <- spTransform(ocean_c_tmp, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
# ocean_sf <- st_as_sf(ocean_t)