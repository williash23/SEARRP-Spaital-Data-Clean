#  Sara Williams
#  July 19, 2017
#  Function to adjust species distribution polygons with information on
#   constraints such as elevation and forest and water dependency.

adj_spp_poly_fun <- function(dat = NULL, sf = NULL, new_spp_sp = NULL, new_spp_sf = NULL){
												 
	# dat: file path to edited non-spatial data from manual input of constraint information from IUCN pages to species
	# sf: file path to simple feature object of species spatial data
	# new_spp_sp: shapefile name to save new ranges to (e.g., mammals, amphibians, birds, etc)
	# new_spp_sf: simple feature object name to save new ranges to 
	
	#  Read in non-spatial, manually edited data
	df_spp_threat <- read.csv(dat)
	#  Read in simple feature object with spatial data and arrange to match order of non-spatial data
	sf_spp_threat <- readRDS(sf) %>%
		dplyr::select(binomial, id_no, geometry, shape_Leng) %>%
		arrange(binomial, shape_Leng)
	sf_spp_threat$binomial <- as.factor(sf_spp_threat$binomial)

	#  Join to original spatial data object to reapply polygon/geometry/spatial info
	spp_df_sf_join <- bind_cols(sf_spp_threat, df_spp_threat) 

	#  Loop over all species distributions and remove pixels that are outside of elevational 
	#   constraint
	new_range_list <- list()
	for(i in 1:nrow(spp_df_sf_join)){	
		sf_tmp <- spp_df_sf_join %>%	
			filter(row_number() == i)
		sp_tmp <-as(sf_tmp, "Spatial")
		elev_tmp <- raster::mask(elev_250m_c, sp_tmp)
		elev_tmp_rc <- raster::reclassify(elev_tmp, c(-Inf, sf_tmp$elev_min,0, sf_tmp$elev_min,sf_tmp$elev_max,1, sf_tmp$elev_max,Inf,0))
		new_range_sp_tmp <- polygonizer(elev_tmp_rc) 
		new_range_sf_tmp <- st_as_sf(new_range_sp_tmp)
		new_range_sf <- new_range_sf_tmp %>%
									filter(DN == 1) 
		new_range_sf_u <- st_union(new_range_sf)
		new_range_sp <-as(new_range_sf_u, "Spatial")
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
		dsn = paste("C:/Users/Sara/Desktop/SEARRP/processed_spat_data/new_ranges", new_spp_sp, sep="/"), 
		layer = new_spp_sp, 
		driver = "ESRI Shapefile")

	#  Combine new spatial ranges with information from data frame
	new_spp_ranges_w_dat <- st_join(new_spp_ranges_sf, spp_df_sf_join)
	save(new_spp_ranges_w_dat, 
		file = paste("C:/Users/Sara/Desktop/SEARRP/processed_spat_data", new_spp_sf, sep = "/"))
	}