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
border_sabah_d_sf <-  st_as_sf(border_sabah_d)

#  Development layers

#  Logging road data from Gaveau et al. 2014, accessed at:
#   https://data.cifor.org/dataset.xhtml?persistentId=doi:10.17528/CIFOR/DATA.00049
log_rds <- shapefile("C:/Users/saraw/Documents/SEARRP/raw_spat_data/roads/REGIONBorneo_LoggingRoad_1970to2010_CIFOR.shp")
log_rds_t <- spTransform(log_rds, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
log_rds_c <- crop(log_rds_t, border_sabah_d)

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
for_cov_c <- crop(for_cov,border_sabah_d)

#  HydroSHEDS river and basin data, accessed at: http://hydrosheds.org/page/overview
hydro_vec <- shapefile("C:/Users/saraw/Documents/SEARRP/raw_spat_data/hydro/hydroSHEDS/as_riv_15s/as_riv_15s.shp")
hydro_vec_t <- spTransform(hydro_vec, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
hydro_vec_c <- crop(hydro_vec_t, border_sabah_d)

#  These rasters are from "Global Multi-resolution Terrain Elevation Data 2010", which replaces GTOPO30
#   accessed at: https://lta.cr.usgs.gov/GMTED2010
elev_250m <- raster("C:/Users/saraw/Documents/SEARRP/raw_spat_data/dem/GMTED2010/7_5_arc_sec/10s090e_20101117_gmted_mea075.tif")
elev_250m_t <- projectRaster(elev_250m, crs = "+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
elev_250m_c <- crop(elev_250m_t, border_sabah_d)


#   Generate buffer around sabah coastline and water features
#   Buffer around coastline
border_sabah_b <- buffer(border_sabah_d, -2000)
#   Remove inside and leave polygon only as strip of buffer near coastline
coast_str <- raster::erase(border_sabah_d, border_sabah_b)
#   Buffer around water features
hydro_vec_b <- st_as_sf(hydro_vec_c) %>%
	st_buffer(dist = 2000)	
hydro_vec_b_sp <- as(hydro_vec_b, "Spatial")
#   Combine coastal strip and water features
water_b <- raster::union(coast_str, hydro_vec_b_sp)
water_d <- raster::aggregate(water_b)

#   Get ocean area to select only sabah border that is coastline
ocean <- shapefile("C:/Users/saraw/Documents/SEARRP/raw_spat_data/ocean/ne_10m_ocean.shp")
ocean_c_tmp <- crop(ocean, sabah_bb_latlong)
ocean_t <- spTransform(ocean_c_tmp, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
ocean_c <- crop(ocean_t, border_sabah_d)
ocean_d <- aggregate(ocean_c)

coast_sf <- st_as_sf(coast_str)
coast_bb_latlong <- extent(115.426438, 117.587149, 4.177596, 4.968712)
coast_bb_latlong <- extent(115.426438, 117.587149, 7.177596, 7.968712)
coast_bb_latlong_p <- as(coast_bb_latlong, 'SpatialPolygons')






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
save(hydro_vec_b, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/hydro_vec_b.Rdata")
save(log_rds_c, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/log_rds_c.Rdata")
save(for_cov_c, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/for_cov_c.Rdata")
save(border_sabah_d, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/border_sabah_d.Rdata")

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

#  Hydro1k elevation data 
# hydro_dem <- raster("C:/Users/sara.williams/Desktop/SEARRP/spat_dat/hydro/as_dem.bil")
# hydro_dem_t <- projectRaster(hydro_dem, crs = "+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
# hydro_dem_c <- crop(hydro_dem_t, border_sabah_d)
# save(hydro_dem_c, file="C:/Users/Sara/Desktop/SEARRP/spat_dat/trans_crop_dat/hydro_dem_c .Rdata")