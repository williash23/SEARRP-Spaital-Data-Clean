#  Sara Williams
#  Novemeber 17, 2017
#  Script to import species range spatial data from IUCN, project to UTM, 
#   crop to extent of Sabah, aggregate multiple records of a single spp and
#   save as sf objects.



#  Load packages
library(rgdal)
library(raster)
library(dplyr) 
library(maptools)
library(rgeos)
library(sf)



#  Set directory paths
path <- "C:/Users/saraw/Documents/SEARRP"
path_spat_dat_raw <- paste(path, "raw_spat_data", sep = "/")
path_spat_dat_proc <- paste(path, "processed_spat_data", sep = "/")
path_df_dat <- paste(path, "processed_excel_data", sep = "/")
setwd(path)



#  Generate base map and extent layers

#  Basic information on the bounding box of Sabah (to be used for cropping) and the CRS that can be used 
#   to link spatial data layers.
sabah_bb_latlong <- extent(115.1531, 119.6081, 3.981738, 7.857259)
sabah_bb_latlong_p <- as(sabah_bb_latlong, 'SpatialPolygons')
crs(sabah_bb_latlong_p) <- "+proj=longlat +datum=WGS84 +no_defs"
sabah_bb_latlong_sf <- st_as_sf(sabah_bb_latlong_p)



#  Generate base map of Sabah
border_my <- shapefile(paste(path_spat_dat_raw, "country_borders/MYS_adm2.shp", sep = "/"))
border_my$NAME_1 <- as.factor(border_my$NAME_1)
border_sabah <- border_my[border_my@data$NAME_1 == "Sabah",]
border_sabah_t <- spTransform(border_sabah, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
border_sabah_d <- unionSpatialPolygons(border_sabah_t, border_sabah_t$ID_0)
border_sabah_sf <-  st_as_sf(border_sabah_d)



#  Species distribution layers

#####  MAMMALS  #####
#  Mammals from IUCN spatail data download center (http://www.iucnredlist.org/technical-documents/spatial-data)

mammals <- shapefile(paste(path_spat_dat_raw, "TERRESTRIAL_MAMMALS/TERRESTRIAL_MAMMALS.shp", sep = "/"))
mammals_c_tmp <- crop(mammals, sabah_bb_latlong)
mammals_t <- spTransform(mammals_c_tmp, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
mammals_c <- crop(mammals_t, border_sabah_d) 
#   Leads to 240 records of 208 unique spp
	####  NOTE: the shapefile from the IUCN spatial data for Melogale everetti is incorrect.

#  Convert to sf object and select spp that are any category except Least Concern and Data Deficient. Also remove polygons
#   for species that show extinct area of occurence (so only keep "presence" column categories 
#   1 and 2. Also remove the citation column (showing how to cite IUCN for use of the maps) as it
#   is available in the metadata PDF. Also removing bats.

sf_mammals <- st_as_sf(mammals_c) %>%
	dplyr::filter(code == "CR" | code == "EN" | code == "VU"| code == "NT") %>%   
	dplyr::filter(presence == 1 | presence == 2)  %>%
	dplyr::filter(order_name != "CHIROPTERA")  %>%
	dplyr::select(-citation) %>% 
	arrange(binomial, shape_Leng)
sf_mammals$binomial <- as.factor(sf_mammals$binomial)
#   Leads to 51 records of 46 unique spp for CR, EN, VU, NT
#   Leads to 40 records of 35 unique spp for CR, EN, VU

#  Aggregate thse sf object so that there is only 1 record (multipolygon) per spp.
#  The result is an sf object*without* all of the associated data.
sf_mammals_agg_tmp1 <- sf_mammals 
sf_mammals_agg_tmp1$group_no <- group_indices(sf_mammals_agg_tmp1, binomial)
groups = sf_mammals_agg_tmp1$group_no
sf_mammals_agg_tmp2 = st_sf(
	geom = do.call(c,
		lapply(1:max(groups), function(g){
			st_union(sf_mammals_agg_tmp1[groups==g,])
			})
		)
	)
#   Leads to 46 records of 46 unique spp for CR, EN, VU, NT
#   Leads to 35 records of 35 unique spp for CR, EN, VU

#  Reattach data from sf_mammals to newly aggregated sf object. Remove columns that are 
#   variable per record, as these should not be assigned to the species as a whole.
sf_mammals_unique <- sf_mammals %>%
	dplyr::select(-presence, - origin, - seasonal, - subspecies, -subpop, - legend, -island,
		-dist_comm, -tax_comm) %>% 
	group_by(binomial) %>%
	slice(1) %>%
	as.data.frame() %>%
	dplyr::select(-geometry)

sf_mammals_agg <- bind_cols(sf_mammals_agg_tmp2, sf_mammals_unique) %>%
	dplyr::select(id_no, binomial, RL_code = code, class = class_name, order = order_name,
	family = family_nam, genus = genus_name, RL_year = year, compiler, source, 
	shape_length = shape_Leng, shape_area = shape_Area)



#####  AMPHIBIANS  #####
#  Amphibians from IUCN spatail data download center (http://www.iucnredlist.org/technical-documents/spatial-data)

amphibians <- shapefile(paste(path_spat_dat_raw, "AMPHIBIANS/AMPHIBIANS.shp", sep = "/"))
amphibians_c_tmp <- crop(amphibians, sabah_bb_latlong)
amphibians_t <- spTransform(amphibians_c_tmp, CRS("+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
amphibians_c <- crop(amphibians_t, border_sabah_d)
#   Leads to 112  records of 112 unique spp
	####  NOTE: the shapefile from the IUCN spatial data for Limnonectes ibanorum, Limnonectes rhacodus, 
	####   Rhacophorus fasciatus, and Pelophryne signata shows that these species ranges barely overlap Sabah 
	####   and thus does not have any part that falls within the elevational constraints. 
	
#  Convert to sf object and select spp that are any category except Least Concern and Data Deficient. Also remove polygons
#   for species that show extinct area of occurence (so only keep "presence" column categories 
#   1 and 2. Also remove the citation column (showing how to cite IUCN for use of the maps) as it
#   is available in the metadata PDF.

sf_amphibians <- st_as_sf(amphibians_c) %>%
	dplyr::filter(code == "CR" | code == "EN" | code == "VU") %>% #| code == "NT") %>%
	dplyr::filter(presence == 1 | presence == 2)  %>%
	dplyr::select(-citation) %>% 
	arrange(binomial, shape_Leng)
sf_amphibians$binomial <- as.factor(sf_amphibians$binomial)
#   Leads to 59 records of 59 unique spp for CR, EN, VU, NT
#   Leads to 59 records of 59 unique spp for CR, EN, VU

#  Do not need to do aggregation step for amphibians because there is only a single record per spp. 
#   Instead, just rename the sf object and remove columns so that it matches the other sf obejcts
#   and remove unwanted columns.
sf_amphibians_agg <- sf_amphibians %>%
	dplyr::select(-presence, - origin, - seasonal, - subspecies, -subpop, - legend, -island,
		-dist_comm, -tax_comm) %>%
	dplyr::select(id_no, binomial, RL_code = code, class = class_name, order = order_name,
	family = family_nam, genus = genus_name, RL_year = year, compiler, source, 
	shape_length = shape_Leng, shape_area = shape_Area)
	

	
#####  BIRDS  #####
#  Birds from BirdLife (Birds of the World)

rl_birds <- st_read(dsn=paste(path_spat_dat_raw, "BOTW.gdb", sep = "/"), layer = "All_Species") 

#  Join spatial data set loaded above (which contains birds from all countries) to data frame of 
#   Malaysian species names (my_species_rl - which includes all spp found in Malaysia other than 
#   Least Concern). This results in a data frame of bird  species that only occur in MY. 
#   Must do this first step because the spatial object is SO large.

birds_j <- rl_birds %>%
	inner_join(my_species_rl, by = c("SCINAME" = "result.scientific_name"))
birds_c_tmp <- st_intersection(birds_j, sabah_bb_latlong_sf)
birds_t <- st_transform(birds_c_tmp, crs = 32650)
birds_c_tmp <- st_intersection(birds_t, border_sabah_d_sf)
birds_c <- as(birds_c_tmp, "Spatial")
#   Leads to 120 records of 119 unique spp

#  Convert to sf object and select spp that are any category except Least Concern and Data Deficient. Also remove polygons
#   for species that show extinct area of occurence (so only keep "presence" column categories 
#   1 and 2. Also remove the citation column (showing how to cite IUCN for use of the maps) as it
#   is available in the metadata PDF.

sf_birds <- st_as_sf(birds_c) %>%
	dplyr::select(id_no = SISID, binomial = SCINAME, RL_code = result.category, 
		presence = PRESENCE, origin = ORIGIN, seasonal = SEASONAL, RL_year = DATE_, 
		compiler = COMPILER, source = SOURCE, shape_length = Shape_Length, 
		shape_area = Shape_Area, geometry) %>%
	dplyr::filter(RL_code == "CR" | RL_code == "EN" | RL_code == "VU") %>% #| RL_code == "NT") %>%
	dplyr::filter(presence == 1 | presence == 2)  %>%
	arrange(binomial, shape_length)
sf_birds$binomial <- as.factor(sf_birds$binomial)
#   Leads to 119 records of 119 unique spp for CR, EN, VU, NT
#   Leads to 26 records of 26 unique spp for CR, EN, VU

#  Do not need to do aggregation step for birds because there is only a single record per spp. 
#   Instead, just rename the sf object and remove columns so that it matches the other sf obejcts
#   and remove unwanted columns.
sf_birds_agg <- sf_birds %>%
	dplyr::select(-presence, - origin, - seasonal) 
	







# #  Save these files to work with later
# save(border_sabah_d, file = paste(path_spat_dat_proc, "trans_crop_proj/border_sabah_d.Rdata", sep = "/"))
# save(border_sabah_sf, file = paste(path_spat_dat_proc, "trans_crop_proj/border_sabah_sf.Rdata", sep = "/"))

# save(sf_mammals_agg, file = paste(path_spat_dat_proc, "spp_ranges_no_constraints/sf_mammals_CR_EN_VU_NT.Rdata", sep = "/"))
# save(sf_amphibians_agg, file = paste(path_spat_dat_proc, "spp_ranges_no_constraints/sf_amphibians_CR_EN_VU_NT.Rdata", sep = "/"))
# save(sf_birds_agg, file = paste(path_spat_dat_proc, "spp_ranges_no_constraints/sf_birds_CR_EN_VU_NT.Rdata", sep = "/"))

# save(sf_mammals_agg, file = paste(path_spat_dat_proc, "spp_ranges_no_constraints/sf_mammals_CR_EN_VU.Rdata", sep = "/"))
# save(sf_amphibians_agg, file = paste(path_spat_dat_proc, "spp_ranges_no_constraints/sf_amphibians_CR_EN_VU.Rdata", sep = "/"))
# save(sf_birds_agg, file = paste(path_spat_dat_proc, "spp_ranges_no_constraints/sf_birds_CR_EN_VU.Rdata", sep = "/"))


