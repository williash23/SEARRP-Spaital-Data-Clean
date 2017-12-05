#  Sara Williams
#  June 6, 2017; last updated Septemeber 18, 2017
#  Importing availble Sabah spatial data and species distribution data, 
#  transforming to UTM and croping to only Sabah.

library(rgdal)
library(raster)
library(dplyr) 
library(maptools)
library(rgeos)
library(sf)
## install issue use: install.packages('xxxx', repos = 'http://cran.cnr.berkeley.edu/')



####  NOTE: From the Rainforest Trust project:
####   Datum: Timbalai 1948 Boreno
####   Projection: Rectified Skew Orthomorphic Borneo Grid
####   EPSG: 29873
####   "+proj=omerc +lat_0=4 +lonc=115 +alpha=53.31582047222222 +k=0.99984 +x_0=590476.87 +y_0=442857.65 +gamma=53.13010236111111 +ellps=evrstSS +towgs84=-679,669,-48,0,0,0,0 +units=m +no_defs" 



path <- "C:/Users/saraw/Documents/SEARRP"
path_spat_dat_raw <- paste(path, "raw_spat_data", sep = "/")
path_spat_dat_proc <- paste(path, "processed_spat_data", sep = "/")
path_df_dat <- paste(path, "processed_excel_data", sep = "/")
setwd(path)





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
border_my <- shapefile(paste(path_spat_dat_raw, "country_borders/MYS_adm2.shp", sep = "/"))
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
border_in <- shapefile(paste(path_spat_dat_raw, "coutry_borders/IDN_adm2.shp", sep = "/")
border_in$NAME_1 <- as.factor(border_in$NAME_1)
border_kali <- border_in[border_in@data$NAME_1 == "Kalimantan Utara",]
border_kali_t <- spTransform(border_kali, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
border_kali_d <- unionSpatialPolygons(border_kali_t, border_kali_t$ID_0)


#  Load border data for use in other processing
load(paste(path_spat_dat_proc, "trans_crop_proj/border_sabah_d.Rdata", sep = "/"))


#  Development layers

#  Logging road data from Gaveau et al. 2014, accessed at:
#   https://data.cifor.org/dataset.xhtml?persistentId=doi:10.17528/CIFOR/DATA.00049
log_rds <- shapefile(paste(path_spat_dat_raw, "roads/REGIONBorneo_LoggingRoad_1970to2010_CIFOR.shp", sep = "/"))
log_rds_t <- spTransform(log_rds, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
log_rds_c <- crop(log_rds_t, border_sabah_d)
log_rds_sf <- st_as_sf(log_rds_c) %>%
	st_intersection(st_as_sf(border_sabah_d))

base_rds_rt <- shapefile(paste(path_spat_dat_raw, "from_RT/Existing_roads/Base_roads_(BRSO).shp", sep = "/"))
crs(base_rds_rt) <- "+proj=omerc +lat_0=4 +lonc=115 +alpha=53.31582047222222 +k=0.99984 +x_0=590476.87 +y_0=442857.65 +gamma=53.13010236111111 +ellps=evrstSS +towgs84=-679,669,-48,0,0,0,0 +units=m +no_defs" 
base_rds_rt_t <- spTransform(base_rds_rt, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
base_rds_rt_c <- crop(base_rds_rt_t, border_sabah_d)
base_rds_rt_sf <- st_as_sf(base_rds_rt_c) %>%
	st_intersection(st_as_sf(border_sabah_d))

main_rds_rt <- shapefile(paste(path_spat_dat_raw, "from_RT/Existing_roads/Main_Roads.shp", sep = "/"))
crs(main_rds_rt) <- "+proj=omerc +lat_0=4 +lonc=115 +alpha=53.31582047222222 +k=0.99984 +x_0=590476.87 +y_0=442857.65 +gamma=53.13010236111111 +ellps=evrstSS +towgs84=-679,669,-48,0,0,0,0 +units=m +no_defs" 
main_rds_rt_t <- spTransform(main_rds_rt, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
main_rds_rt_c <- crop(main_rds_rt_t, border_sabah_d)
main_rds_rt_sf <- st_as_sf(main_rds_rt_c) %>%
	st_intersection(st_as_sf(border_sabah_d))

	
	
	
#  Protected areas, information from:
#   http://services.arcgis.com/P8Cok4qAP1sTVE59/ArcGIS/rest/services/Protected_Area_of_Borneo/FeatureServer/0
#   Generated GeoJSON data from a query on the arcGIS services website. 
pa <- readOGR(dsn = paste(path_spat_dat_raw, "land_ownership/pa_geojson.GeoJSON", sep = "/"), layer = "OGRGeoJSON")
pa_t <- spTransform(pa, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
pa_sf <- st_as_sf(pa_t)
pa_sabah_sf <- pa_sf %>%
	dplyr::filter(COUNTRY == "Sabah")
pa_sabah_sf$DESIG[pa_sabah_sf$DESIG == "Wildlife Sancturary"] <- "Wildlife Sanctuary"
pa_sabah_sf$DESIG[pa_sabah_sf$DESIG == "Protection Forest"] <- "Protected Forest"
pa_sabah_sf$DESIG <- droplevels(pa_sabah_sf$DESIG)
pa_sabah_sf$COUNTRY <- droplevels(pa_sabah_sf$COUNTRY)



#  New parks and wildlife sanctuary data from RT project data
pa_rt <- shapefile(paste(path_spat_dat_raw, "from_RT/Parks_wildlife_sanctuaries/Sabah Parks (BRSO).shp", sep = "/"))
crs(pa_rt) <- "+proj=omerc +lat_0=4 +lonc=115 +alpha=53.31582047222222 +k=0.99984 +x_0=590476.87 +y_0=442857.65 +gamma=53.13010236111111 +ellps=evrstSS +towgs84=-679,669,-48,0,0,0,0 +units=m +no_defs" 
pa_rt_t <- spTransform(pa_rt, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
pa_rt_c <- crop(pa_rt_t, border_sabah_d)
pa_rt_sf <- st_as_sf(pa_rt_c)

wl_sanc_rt <- shapefile(paste(path_spat_dat_raw, "from_RT/Parks_wildlife_sanctuaries/Wildlife Sanctuaries (BRSO).shp", sep = "/"))
crs(wl_sanc_rt) <- "+proj=omerc +lat_0=4 +lonc=115 +alpha=53.31582047222222 +k=0.99984 +x_0=590476.87 +y_0=442857.65 +gamma=53.13010236111111 +ellps=evrstSS +towgs84=-679,669,-48,0,0,0,0 +units=m +no_defs" 
wl_sanc_rt_t <- spTransform(wl_sanc_rt, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
wl_sanc_rt_c <- crop(wl_sanc_rt_t, border_sabah_d)
wl_sanc_rt_sf <- st_as_sf(wl_sanc_rt_c)


#  New forest reserve and FMU data from RT project data
fmu_rt <- shapefile(paste(path_spat_dat_raw, "from_RT/Forest_reserves_FMUs/Forest Management Units (FMU) boundaries (BRSO).shp", sep = "/"))
crs(fmu_rt) <- "+proj=omerc +lat_0=4 +lonc=115 +alpha=53.31582047222222 +k=0.99984 +x_0=590476.87 +y_0=442857.65 +gamma=53.13010236111111 +ellps=evrstSS +towgs84=-679,669,-48,0,0,0,0 +units=m +no_defs" 
fmu_rt_t <- spTransform(fmu_rt , CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
fmu_rt_c <- crop(fmu_rt_t, border_sabah_d)
fmu_rt_sf <- st_as_sf(fmu_rt_c)

for_res_rt <- shapefile(paste(path_spat_dat_raw, "from_RT/Forest_reserves_FMUs/Forest Reserves classes 1-7 (BRSO).shp", sep = "/"))
crs(for_res_rt) <- "+proj=omerc +lat_0=4 +lonc=115 +alpha=53.31582047222222 +k=0.99984 +x_0=590476.87 +y_0=442857.65 +gamma=53.13010236111111 +ellps=evrstSS +towgs84=-679,669,-48,0,0,0,0 +units=m +no_defs" 
for_res_rt_t <- spTransform(for_res_rt , CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
for_res_rt_c <- crop(for_res_rt_t, border_sabah_d)
for_res_rt_sf <- st_as_sf(for_res_rt_c)


#  Environmental layers

#  Forest cover data from Gaveau et al. 2016, accessed at: 
#   https://data.cifor.org/dataset.xhtml?persistentId=doi:10.17528/CIFOR/DATA.00049
for_cov <- raster(paste(path_spat_dat_raw, "forest_cover/REGBorneo_ForestCover_2016_CIFOR.tif", sep = "/"))
for_cov_c <- raster::crop(for_cov, border_sabah_d)
for_cov_m <- raster::mask(for_cov_c, border_sabah_d)


#  These rasters are from "Global Multi-resolution Terrain Elevation Data 2010", which replaces GTOPO30
#   accessed at: https://lta.cr.usgs.gov/GMTED2010
elev_250m <- raster(paste(path_spat_dat_raw, "dem/GMTED2010/7_5_arc_sec/10s090e_20101117_gmted_mea075.tif", sep = "/"))
elev_250m_t <- projectRaster(elev_250m, crs = "+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0")
elev_250m_c <- crop(elev_250m_t, border_sabah_d)
elev_250m_m <- raster::mask(elev_250m_c, border_sabah_d)
elev_rs <- resample(elev_250m, for_cov, "bilinear")

#  Calculate slope from resampled elevation raster
elev_rs <- raster(paste(path_spat_dat, "trans_crop_proj/elev_rs.grd", sep = "/"))
slp <- terrain(elev_rs, opt = 'slope', unit='degrees')


#  HydroSHEDS river data, accessed at: http://hydrosheds.org/page/overview
hydro_vec <- shapefile(paste(path_spat_dat_raw, "hydro/hydroSHEDS/as_riv_15s/as_riv_15s.shp", sep = "/"))
hydro_vec_t <- spTransform(hydro_vec, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
hydro_vec_c <- crop(hydro_vec_t, border_sabah_d)


#  New river data from RT project data
rivers_vec <- shapefile(paste(path_spat_dat_raw, "from_RT/Rivers_catchments/detail_river_sabah.shp", sep = "/"))
crs(rivers_vec) <- "+proj=omerc +lat_0=4 +lonc=115 +alpha=53.31582047222222 +k=0.99984 +x_0=590476.87 +y_0=442857.65 +gamma=53.13010236111111 +ellps=evrstSS +towgs84=-679,669,-48,0,0,0,0 +units=m +no_defs" 
rivers_vec_t <- spTransform(rivers_vec, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
rivers_vec_c <- crop(rivers_vec_t, border_sabah_d)
rivers_sf <- st_as_sf(rivers_vec_c)


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


#  Buffers around water features
buff_dist <- 50
rivers_sf_b <- st_buffer(rivers_sf, buff dist)
coast_b <- st_buffer(coast_str, budd_dist - 10) # coast line already includes a 10m buffer from coast line creation
all_water <- st_union(rivers_b, coast_b)
all_water_sp <- as(all_water, "Spatial")
rivers_sp <- as(rivers_b, "Spatial")



#  Species distribution layers
#   Distinguished only by spatial location - coarse distributions from IUCN

#  Mammals from IUCN spatail data download center (http://www.iucnredlist.org/technical-documents/spatial-data)
mammals <- shapefile(paste(path_spat_dat_raw, "TERRESTRIAL_MAMMALS/TERRESTRIAL_MAMMALS.shp", sep = "/"))
mammals_c_tmp <- crop(mammals, sabah_bb_latlong)
mammals_t <- spTransform(mammals_c_tmp, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
mammals_c <- crop(mammals_t, border_sabah_d)


#  Amphibians from IUCN spatail data download center (http://www.iucnredlist.org/technical-documents/spatial-data)
amphs <- shapefile(paste(path_spat_dat_raw, "AMPHIBIANS/AMPHIBIANS.shp", sep = "/"))
amphs_c_tmp <- crop(amphs, sabah_bb_latlong)
amphs_t <- spTransform(amphs_c_tmp, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
amphibians_c <- crop(amphs_t, border_sabah_d)


#  Birds from BirdLife (Birds of the World)
rl_birds <- st_read(dsn=paste(path_spat_dat_raw, "BOTW.gdb", sep = "/"), layer = "All_Species") 
#  Join spatial data set (which contains birds from all countries) to data frame of MY species (
#   all taxa, all spp other than least concern) only, to obtain a data frame of bird species that only occur in MY 
#   Must do this first step because the spatial object is SO large.
birds_j <- rl_birds %>%
	inner_join(my_species_rl, by = c("SCINAME" = "result.scientific_name"))
birds_c_tmp <- st_intersection(birds_j, sabah_bb_latlong_sf)
birds_t <- st_transform(birds_c_tmp, crs = 32650)
birds_c_tmp <- st_intersection(birds_t, border_sabah_d_sf)
birds_c <- as(birds_c_tmp, "Spatial")



	

#  Use function below to import, project, crop, and save many development files from RT.
#tcp_fun <- function(sp_f, save_f, sp_n){
	sp_tmp <- shapefile(sp_f)
	crs(sp_tmp) <- "+proj=omerc +lat_0=4 +lonc=115 +alpha=53.31582047222222 +k=0.99984 +x_0=590476.87 +y_0=442857.65 +gamma=53.13010236111111 +ellps=evrstSS +towgs84=-679,669,-48,0,0,0,0 +units=m +no_defs" 
	sp_tmp_t <- spTransform(sp_tmp, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
	sp_tmp_c <- crop(sp_tmp_t, border_sabah_d)
	sp_tmp_sf <- st_as_sf(sp_tmp_c) %>%
		st_intersection(st_as_sf(border_sabah_d))
	assign(sp_n, sp_tmp_sf)
	#}
		
sp_f = paste(path_spat_dat_raw, "from_RT/Existing_proposed_infrastructure/New_Port.shp", sep = "/")
sp_n = "new_seaport" # had to do manually because seaport locations in ocean
save(new_seaport, file = paste(path_spat_dat_proc, "trans_crop_proj/dev_areas/new_seaport.Rdata", sep = "/"))

sp_f = paste(path_spat_dat_raw, "from_RT/Existing_proposed_infrastructure/Existing_Port.shp", sep = "/")
sp_n = "exist_seaport" # had to do manually because seaport locations in ocean
save(exist_seaport, file = paste(path_spat_dat_proc, "trans_crop_proj/dev_areas/exist_seaport.Rdata", sep = "/"))
	
sp_f = paste(path_spat_dat_raw, "from_RT/Existing_proposed_infrastructure/Existing_Airport.shp", sep = "/")
sp_n = "exist_airport"
save(exist_airport, file = paste(path_spat_dat_proc, "trans_crop_proj/dev_areas/exist_airport.Rdata", sep = "/"))
	
sp_f = paste(path_spat_dat_raw, "from_RT/Existing_proposed_infrastructure/Major_Upgraded_Transportation_Hub.shp", sep = "/")
sp_n = "hub_upgrade"
save(hub_upgrade, file = paste(path_spat_dat_proc, "trans_crop_proj/dev_areas/hub_upgrade.Rdata", sep = "/"))	

sp_f = paste(path_spat_dat_raw, "from_RT/Existing_proposed_infrastructure/Proposed_Airfield.shp", sep = "/")
sp_n = "prop_airfield"
save(prop_airfield, file = paste(path_spat_dat_proc, "trans_crop_proj/dev_areas/prop_airfield.Rdata", sep = "/"))	

sp_f = paste(path_spat_dat_raw, "from_RT/Existing_proposed_infrastructure/Search_Area_for_New_Airport.shp", sep = "/")
sp_n = "new_airport_search_area"
save(new_airport_search_area, file = paste(path_spat_dat_proc, "trans_crop_proj/dev_areas/new_airport_search_area.Rdata", sep = "/"))	

sp_f = paste(path_spat_dat_raw, "from_RT/Existing_proposed_infrastructure/Existing_Road.shp", sep = "/")
sp_n = "exist_rd"
save(exist_rd, file = paste(path_spat_dat_proc, "trans_crop_proj/dev_areas/exist_rd.Rdata", sep = "/"))	

sp_f = paste(path_spat_dat_raw, "from_RT/Existing_proposed_infrastructure/Proposed_New_Upgraded_Airport.shp", sep = "/")
sp_n = "prop_new_upgraded_airport"	
save(prop_new_upgraded_airport, file = paste(path_spat_dat_proc, "trans_crop_proj/dev_areas/prop_new_upgraded_airport.Rdata", sep = "/"))	

sp_f = paste(path_spat_dat_raw, "from_RT/Existing_proposed_infrastructure/Upgraded_Road.shp", sep = "/")
sp_n = "rd_upgraded"
save(rd_upgraded, file = paste(path_spat_dat_proc, "trans_crop_proj/dev_areas/rd_upgraded.Rdata", sep = "/"))	

sp_f = paste(path_spat_dat_raw, "from_RT/Existing_proposed_infrastructure/Proposed_Existing_Road_to_be_Upgraded_as_Highway_(4_lanes_-_80M).shp", sep = "/")
sp_n = "prop_rd_upgrade_hwy"			
save(prop_rd_upgrade_hwy, file = paste(path_spat_dat_proc, "trans_crop_proj/dev_areas/prop_rd_upgrade_hwy.Rdata", sep = "/"))	

sp_f = paste(path_spat_dat_raw, "from_RT/Existing_proposed_infrastructure/On-going_New_Road_Project.shp", sep = "/")
sp_n = "new_rd"		
save(new_rd, file = paste(path_spat_dat_proc, "trans_crop_proj/dev_areas/new_rd.Rdata", sep = "/"))	

sp_f = paste(path_spat_dat_raw, "from_RT/Existing_proposed_infrastructure/Proposed_New_Highway_(80M).shp", sep = "/")
sp_n = "prop_new_hwy"			
save(prop_new_hwy, file = paste(path_spat_dat_proc, "trans_crop_proj/dev_areas/prop_new_hwy.Rdata", sep = "/"))	

sp_f = paste(path_spat_dat_raw, "from_RT/Existing_proposed_infrastructure/Proposed_New_Road_(40M).shp", sep = "/")
sp_n = "prop_new_rd"	
save(prop_new_rd, file = paste(path_spat_dat_proc, "trans_crop_proj/dev_areas/prop_new_rd.Rdata", sep = "/"))	

sp_f = paste(path_spat_dat_raw, "from_RT/Existing_proposed_infrastructure/Proposed_New_Railway_Line.shp", sep = "/")
sp_n = "prop_new_rail"			
save(prop_new_rail, file = paste(path_spat_dat_proc, "trans_crop_proj/dev_areas/prop_new_rail.Rdata", sep = "/"))	

sp_f = paste(path_spat_dat_raw, "from_RT/Existing_proposed_infrastructure/Proposed_Upgraded_Railway_Line.shp", sep = "/")
sp_n = "prop_upgrade_rail"		
save(prop_upgrade_rail, file = paste(path_spat_dat_proc, "trans_crop_proj/dev_areas/prop_upgrade_rail.Rdata", sep = "/"))	
	



#  Save these files to work with later

save(for_res_rt_sf, file = paste(path_spat_dat_proc, "trans_crop_proj/for_res_rt_sf.Rdata", sep = "/"))
save(fmu_rt_sf, file = paste(path_spat_dat_proc, "trans_crop_proj/fmu_rt_sf.Rdata", sep = "/"))
save(wl_sanc_rt_sf, file = paste(path_spat_dat_proc, "trans_crop_proj/wl_sanc_rt_sf.Rdata", sep = "/"))
save(pa_rt_sf, file = paste(path_spat_dat_proc, "trans_crop_proj/pa_rt_sf.Rdata", sep = "/"))
save(pa_sabah_sf, file = paste(path_spat_dat_proc, "trans_crop_proj/pa_sabah_sf.Rdata", sep = "/"))
save(log_rds_sf, file = paste(path_spat_dat_proc, "trans_crop_proj/log_rds_sf.Rdata", sep = "/"))
save(base_rds_rt_sf, file = paste(path_spat_dat_proc, "trans_crop_proj/base_rds_rt_sf.Rdata", sep = "/"))
save(main_rds_rt_sf, file = paste(path_spat_dat_proc, "trans_crop_proj/main_rds_rt_sf.Rdata", sep = "/"))

save(coast_str, file = paste(path_spat_dat_proc, "trans_crop_proj/coast_str.Rdata", sep = "/"))
save(hydro_vec_c, file = paste(path_spat_dat_proc, "trans_crop_proj/hydro_vec_c.Rdata", sep = "/"))
writeRaster(for_cov_m, paste(path_spat_dat_proc, "trans_crop_proj/for_cov_m.grd", sep = "/"))
writeRaster(elev_250m_m, paste(path_spat_dat_proc, "trans_crop_proj/elev_250m_m.grd", sep = "/"))
writeRaster(elev_rs, paste(path_spat_dat_proc, "trans_crop_proj/elev_rs.grd", sep = "/"))

save(border_sabah_d, file = paste(path_spat_dat_proc, "trans_crop_proj/border_sabah_d.Rdata", sep = "/"))
save(border_sabah_sf, file = paste(path_spat_dat_proc, "trans_crop_proj/border_sabah_sf.Rdata", sep = "/"))
save(border_sarawak_d, file = paste(path_spat_dat_proc, "trans_crop_proj/border_sarawak_d.Rdata", sep = "/"))
save(border_kali_d, file = paste(path_spat_dat_proc, "trans_crop_proj/border_kali_d.Rdata", sep = "/"))

save(mammals_c, file = paste(path_spat_dat_proc, "trans_crop_proj/mammals_c.Rdata", sep = "/"))
save(amphibians_c, file = paste(path_spat_dat_proc, "trans_crop_proj/amphibians_c.Rdata", sep = "/"))
save(birds_c, file = paste(path_spat_dat_proc, "trans_crop_proj/birds_c.Rdata", sep = "/"))
writeOGR(mammals_c, paste(path_spat_dat_proc, "trans_crop_proj", sep = "/"), "mammals_c", driver="ESRI Shapefile")
writeOGR(amphibians_c, paste(path_spat_dat_proc, "trans_crop_proj", sep = "/"), "amphibians_c" , driver="ESRI Shapefile")
writeOGR(birds_c, paste(path_spat_dat_proc, "trans_crop_proj", sep = "/"), "birds_sp", driver="ESRI Shapefile")







#  Other potential spatial data sources

#  Industrial plantations, from: M. Strimas (via email), is from 2010
land_own <- shapefile("C:/Users/saraw/Documents/SEARRP/raw_spat_data/land_ownership/REGIONBorneo_IndustrialPlantation_2010_CIFOR.shp")
land_own_t <- spTransform(land_own, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
land_own_c <- crop(land_own_t, border_sabah_d)


#  This file (downloaded from Gaveau et al. 2016) has error when trying to crop to just the Sabah region
orig_land_cov <- shapefile("C:/Users/saraw/Documents/SEARRP/raw_spat_data/land_ownership/REGBorneo_OriginOfLandConvertedToITPAndIOPPComplexTrajectory_1973to2016_CIFOR/REGBorneo_OriginOfLandConvertedToITPAndIOPPComplexTrajectory_1973to2016_CIFOR.shp")
orig_land_cov_b <- gBuffer(land_own, byid=TRUE, width=0)
orig_land_cov_t <- spTransform(orig_land_cov_b, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
orig_land_cov_c <- crop(orig_land_cov_t, border_sabah_d)


### These PA files do not currently work
#  Protected area and other land ownership data from M. Strimas via email or Gaveau et al. 2014, accessed at:
#   https://data.cifor.org/dataset.xhtml?persistentId=doi:10.17528/CIFOR/DATA.00049
#  This (obtained from M. Strimas) appears to be a plantation land ownership shapefile, not protected areas
pa <- shapefile("C:/Users/saraw/Documents/SEARRP/raw_spat_data/land_ownership/REGIONBorneo_ProtectedArea_2010_CIFOR/REGIONBorneo_ProtectedArea_2010_CIFOR.shp")
pa_c_tmp <- crop(pa, sabah_bb_latlong)
pa_t <- spTransform(pa_c_tmp, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
pa_c <- crop(pa_t, border_sabah_d)


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