#  Sara Williams
#  August 7, 2017
#  Function to adjust species distribution polygons with information on
#   water dependency constraint.

####  NEED TO MERGE SPECIES POLYGONS THAT HAVE MORE THAN ONE
####  ALSO NEED TO INLCUDE COASTLINE IN BUFFER OF WATER FEAR+TURES

####  Currently, I generate a static buffer around the water features to be used for all spp (1000m).
####   However, buffer size could be nested within this function and vary based on conditions of
####   other variables. I.e., for river dependent kingfishers, condition could be if(family  == "Coraciiformes"), 
####   hydro_vec_b <- st_as_sf(hydro_vec_c) %>%
####       st_buffer(dist = 1000)

adj_spp_poly_water_fun <- function(dat = NULL, 
	sf = NULL, 
	new_spp_sp = NULL,
	water_feature = NULL){ 
	#, new_spp_sf = NULL){
												 
	# dat: file path to edited non-spatial data from manual input of constraint information from IUCN pages to species
	# sf: file path to simple feature object of species spatial data
	# new_spp_sp: shapefile name to save new ranges to (e.g., mammals, amphibians, birds, etc)
	# new_spp_sf: simple feature object name to save new ranges to 
	
	#  Read in non-spatial, manually edited data and select only one record per spp
	#   Some spp have multiple polygons of ranges but all records for a sinple spp
	#   contain the same info in the csv.
	df_spp_threat <- read.csv(dat) %>%
		group_by(binomial) %>%
		slice(1) %>%
		as.data.frame()
		
	#  Read in simple feature object with spatial data, dissolve all polygons belonging to same spp, and 
	#   arrange to match order of non-spatial data
	sf_spp_threat <- readRDS(sf) %>%
		dplyr::select(binomial, id_no, geometry, shape_Leng) %>% 
		arrange(binomial, shape_Leng)
	sf_spp_threat$binomial <- as.factor(sf_spp_threat$binomial)
	sp_spp_threat <- as(sf_spp_threat, "Spatial")
	sp_spp_threat_a <- aggregate(sp_spp_threat, by = "binomial")
	sf_spp_threat_a <- st_as_sf(sp_spp_threat_a)
		
	#  Join to original spatial sf object to reapply polygon/geometry/spatial info
	spp_df_sf_join <- bind_cols(sf_spp_threat_a, df_spp_threat) 	
	
	#  Loop over all species distributions and remove pixels that are outside of water dependency 
	#   constraint
	new_range_list <- list()
	for(i in 1:nrow(spp_df_sf_join)){	
		sf_tmp <- spp_df_sf_join %>%	
			dplyr::filter(row_number() == i)
		sp_tmp <- as(sf_tmp, "Spatial")
		if(sf_tmp$water_dep == 1) {
			  new_range_sp_tmp <- crop(sp_tmp, water_feature)
			} else {
			  new_range_sp_tmp <- sp_tmp
			}
		new_range_sf <- st_as_sf(new_range_sp_tmp)
		new_range_sf_u <- st_union(new_range_sf)
		new_range_sp <- as(new_range_sf_u, "Spatial")
		new_range_sp@polygons[[1]]@ID <-  paste0("ID", i)
		id <- paste0("poly_", i)
		new_range_list[[id]] <- new_range_sp
	}

	#  Generate new spatial object that contains all new range polygons and convert to 
	#   sf object
	new_spp_ranges_sp <- SpatialPolygons(lapply(new_range_list, function(x){x@polygons[[1]]}))
	new_spp_ranges_sf <- st_as_sf(new_spp_ranges_sp)
	st_crs(new_spp_ranges_sf) <- st_crs(sf_spp_threat)

	#  Write to shapefile for later use
	st_write(new_spp_ranges_sf, 
		dsn = paste("C:/Users/saraw/Documents/SEARRP/processed_spat_data/new_ranges", new_spp_sp, sep="/"), 
		layer = new_spp_sp, 
		driver = "ESRI Shapefile")
}


#	#  Combine new spatial ranges with information from data frame
#	new_spp_ranges_w_dat <- st_join(new_spp_ranges_sf, spp_df_sf_join)
#	save(new_spp_ranges_w_dat, 
#		file = paste("C:/Users/saraw/Documents/SEARRP/processed_spat_data", new_spp_sf, sep = "/"))
#	}