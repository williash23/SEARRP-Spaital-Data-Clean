###############################################################################
#  Updated spatial data clean.
#  April 30, 2018.
#  Sara Williams
###############################################################################



# =============================================================================
#  Notes 
# =============================================================================

####  Data from the Rainforest Trust project:
####   Datum: Timbalai 1948 Boreno
####   Projection: Rectified Skew Orthomorphic Borneo Grid
####   EPSG: 29873
####   "+proj=omerc +lat_0=4 +lonc=115 +alpha=53.31582047222222 +k=0.99984 +x_0=590476.87 +y_0=442857.65 +gamma=53.13010236111111 +ellps=evrstSS +towgs84=-679,669,-48,0,0,0,0 +units=m +no_defs" 



# =============================================================================
#  Load packages and functions.
# =============================================================================
library(sf)
library(sp)
library(raster)
library(dplyr)
library(stringr)
library(ggplot2)

st_erase = function(x, y) st_difference(x, st_union(st_combine(y)))


	
# =============================================================================
#  Load and process data.
# =============================================================================		

	# ----------------------
	#  Desired CRS.
	proj_UTM <- "+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"

	# ----------------------
	#  Boundaries
	sabah_border_sp <- shapefile("C:/Users/saraw/Documents/Spatial_data/Boundaries/Sabah state boundaries.shp")
	sabah_border <- st_transform(st_as_sf(sabah_border_sp), proj_UTM) 
	mainland_sabah_border <- sabah_border %>%
		dplyr::filter(Shape_Area > 70000000000) 
	#sabah_border <- st_union(sabah_border)
	#save(sabah_border, file = "C:/Users/saraw/Desktop/boundaries/sabah_border.Rdata")
	
	
	sarawak_border_sp <- shapefile("C:/Users/saraw/Documents/Spatial_data/Boundaries/MYS_adm2.shp")
	sarawak_border <- st_transform(st_as_sf(sarawak_border_sp), proj_UTM) %>%
		dplyr::filter(NAME_1 == "Sarawak") #%>%
		#st_union()
	#save(sarawak_border, file = "C:/Users/saraw/Desktop/boundaries/sarawak_border.Rdata")
	
	
	kali_border_sp <- shapefile("C:/Users/saraw/Documents/Spatial_data/Boundaries/IDN_adm2.shp")
	kali_border <- st_transform(st_as_sf(kali_border_sp), proj_UTM) %>%
		dplyr::filter(str_detect(NAME_1, "Kalimantan") # %>%
		#st_union()
	#save(kali_border, file = "C:/Users/saraw/Desktop/boundaries/kali_border.Rdata")
	
	
	ssk_border <- st_union(sabah_border, sarawak_border, kali_border, by_feature = TRUE)
	
	
	
	# ----------------------
	#  Permanent Forest Reserves and Totally Protected Areas
	for_res_sp <- shapefile("C:/Users/saraw/Documents/Spatial_data/PFR/Permanent Forest Reserve and other reserves and licenses.shp")
	for_res <- st_transform(st_as_sf(for_res_sp), proj_UTM) %>%
		dplyr::select(PFR_CLASS, NAME, PFR_Area_h)
	for_res$PFR_CLASS <- as.factor(for_res$PFR_CLASS)
	
	
	sabah_tpa_sp <- shapefile("C:/Users/saraw/Documents/Spatial_data/TPA/Totally Protected Areas 2017.shp")
	sabah_tpa <- st_transform(st_as_sf(sabah_tpa_sp), proj_UTM) %>%
		dplyr::select(NAME = FR_NAME, CLASS, DESIG = REMARK)
	sabah_tpa$DESIG <- as.factor(sabah_tpa$DESIG)
	sabah_tpa$CLASS <- as.factor(sabah_tpa$CLASS)
	sabah_tpa_u <- st_union(st_buffer(sabah_tpa, 0.1))
	#save(sabah_tpa, file = "C:/Users/saraw/Desktop/boundaries/sabah_tpa.Rdata")
	
	
	sarawak_tpa_sp <- shapefile("C:/Users/saraw/Documents/Spatial_data/TPA/WDPA_Jan2018_MYS-shapefile-polygons.shp")
	sarawak_UTM <- spTransform(sa_sp, CRS( "+proj=utm +zone=50 +datum=WGS84 +units=m +no_defs +ellps=WGS84 +towgs84=0,0,0"))
	sarawak_tpa <- st_transform(st_as_sf(sarawak_tpa_sp), proj_UTM) %>%
		st_intersection(sarawak_border) %>%
		dplyr::select(NAME, CLASS =  PA_DEF, DESIG = DESIG_ENG)
	sarawak_tpa$DESIG <- as.factor(sarawak_tpa$DESIG)
	sarawak_tpa$CLASS <- as.factor(sarawak_tpa$CLASS)
	#save(sarawak_tpa, file = "C:/Users/saraw/Desktop/sarawak_cover/sarawak_tpa.Rdata")
	
	kali_tpa_sp <- shapefile("C:/Users/saraw/Documents/Spatial_data/TPA/WDPA_Jan2018_IDN-shapefile-polygons.shp")
	kali_tpa <- st_transform(st_as_sf(kali_tpa_sp), proj_UTM) %>%
		st_intersection(kali_border) %>%
		dplyr::select(NAME, CLASS =  PA_DEF, DESIG = DESIG_ENG)
	kali_tpa$DESIG <- as.factor(kali_tpa$DESIG)
	kali_tpa$CLASS <- as.factor(kali_tpa$CLASS)
	
	ssk_tpa <- rbind(sabah_tpa, sarawak_tpa, kali_tpa)

	#  ----------------------
	#  Locked in and locked out areas.
	sfi_RT_sp <- shapefile("C:/Users/saraw/Documents/Spatial_data/Boundaries/Sabah_Forest_Industries_(SFI).shp")
	sfi_RT <- st_transform(st_as_sf(sfi_RT_sp), proj_UTM) %>%	
		dplyr::select(FR_NAME, PFR_CLASS, Licensee)
	sfi_RT_u <- st_union(sfi_RT)
	sfi <- sfi_RT_u
	#save(sfi, file = "C:/Users/saraw/Desktop/boundaries/sfi.Rdata")
		
	idris_RT_sp <- shapefile("C:/Users/saraw/Documents/Spatial_data/Boundaries/Idris Hydraulic.shp")
	idris_RT <- st_transform(st_as_sf(idris_RT_sp), proj_UTM) %>%	
		dplyr::select(FR_NAME, PFR_CLASS, Licensee)
	idris_RT_u <- st_union(idris_RT)
	idris <- idris_RT_u
	#save(idris, file = "C:/Users/saraw/Desktop/boundaries/idris.Rdata")
	
	sfi_idris <- rbind(sfi_RT, idris_RT)
	sfi_idris_u <- st_union(sfi_idris)
	
	
	sega_RT_sp <- shapefile("C:/Users/saraw/Documents/Spatial_data/PFR/Segaliud-Lokan_boundaries.shp")
	sega <- st_transform(st_as_sf(sega_RT_sp), proj_UTM) 
	
	deram <- for_res %>%
		dplyr::filter(NAME == "Deramakot Forest Reserve")


	crock_kina_tmp <- sabah_tpa %>% dplyr::filter(NAME == "Bukit Hampuan" | NAME == "Tenompok")
	crock_kina_tmp_b <- st_buffer(crock_kina_tmp, 5000)
	crock_kina <- st_union(crock_kina_tmp_b)
	
	
	
	
# =============================================================================
#  Planning unit grids.
# =============================================================================

	# ----------------------
	#  Forest area
	load(file = "C:/Users/saraw/Desktop/planning_unit_grids/acd_agg_sf_for.Rdata") 
	
	# ----------------------
	#  Entire study area without existing TPAs
	sabah_for <- st_intersection(acd_agg_sf_for, mainland_sabah_border)
	sabah_for_no_pa <- st_erase(sabah_for, st_buffer(sabah_tpa, 0.1))
	sabah_for_no_pa_sp <- as(sabah_for_no_pa, 'Spatial')
	sa_grid_sp <- make_grid(sabah_for_no_pa_sp,  type = "square", cell_area = 5000000, clip = FALSE)
	sa_grid <- st_as_sf(sa_grid_sp)
	#save(sa_grid, file = "C:/Users/saraw/Desktop/planning_unit_grids/sa_grid.Rdata")
	
	# ----------------------
	#  SFI without existing TPAs
	sfi_grid_df_tmp <- as.data.frame(st_intersects(sa_grid, sfi_RT_u, sparse = FALSE))
	names(sfi_grid_df_tmp)[1] <- "IN_tmp"
	sfi_grid_df <- sfi_grid_df_tmp %>%
		dplyr::mutate(IN = ifelse(IN_tmp == "FALSE", 0, 1)) %>%
		dplyr::select(IN)
	sfi_grid <- sa_grid %>%
		bind_cols(sfi_grid_df) %>%
		dplyr::filter(IN == 1)
	save(sfi_grid, file = "C:/Users/saraw/Desktop/planning_unit_grids/sfi_grid.Rdata")
	
	# ----------------------
	#  Idris without existing TPAs
	idris_grid_df_tmp <- as.data.frame(st_intersects(sa_grid, idris_RT_u, sparse = FALSE))
	names(idris_grid_df_tmp)[1] <- "IN_tmp"
	idris_grid_df <- idris_grid_df_tmp %>%
		dplyr::mutate(IN = ifelse(IN_tmp == "FALSE", 0, 1)) %>%
		dplyr::select(IN)
	idris_grid <- sa_grid %>%
		bind_cols(idris_grid_df) %>%
		dplyr::filter(IN == 1)
	save(idris_grid, file = "C:/Users/saraw/Desktop/planning_unit_grids/idris_grid.Rdata")
		
	# ----------------------
	#  Deramakot FR
	deram_grid_df_tmp <- as.data.frame(st_intersects(sa_grid, deram, sparse = FALSE))
	names(deram_grid_df_tmp)[1] <- "IN_tmp"
	deram_grid_df <- deram_grid_df_tmp %>%
		dplyr::mutate(IN = ifelse(IN_tmp == "FALSE", 0, 1)) %>%
		dplyr::select(IN)
	deram_grid <- sa_grid %>%
		bind_cols(deram_grid_df) %>%
		dplyr::filter(IN == 1)
	save(deram_grid, file = "C:/Users/saraw/Desktop/planning_unit_grids/deram_grid.Rdata")
	
	# ----------------------
	#  Segalied-Lookan FR
	sega_grid_df_tmp <- as.data.frame(st_intersects(sa_grid, sega, sparse = FALSE))
	names(sega_grid_df_tmp)[1] <- "IN_tmp"
	sega_grid_df <- sega_grid_df_tmp %>%
		dplyr::mutate(IN = ifelse(IN_tmp == "FALSE", 0, 1)) %>%
		dplyr::select(IN)
	sega_grid <- sa_grid %>%
		bind_cols(sega_grid_df) %>%
		dplyr::filter(IN == 1)
	save(sega_grid, file = "C:/Users/saraw/Desktop/planning_unit_grids/sega_grid.Rdata")
	
	# ----------------------
	#  Area between Crocker Range and Kinabalu Park to lock out
	crock_kina_grid_df_tmp <- as.data.frame(st_intersects(sa_grid, crock_kina, sparse = FALSE))
	names(crock_kina_grid_df_tmp)[1] <- "IN_tmp"
	crock_kina_grid_df <- crock_kina_grid_df_tmp %>%
		dplyr::mutate(IN = ifelse(IN_tmp == "FALSE", 0, 1)) %>%
		dplyr::select(IN)
	crock_kina_grid <- sa_grid %>%
		bind_cols(crock_kina_grid_df) %>%
		dplyr::filter(IN == 1)
	#save(crock_kina_grid, file = "C:/Users/saraw/Desktop/planning_unit_grids/crock_kina_grid.Rdata")
	
	
	
	
	
	
	
	
	
	
	
	
	# ----------------------
	#  Entire study area with existing TPAs
	sabah_for <- st_intersection(acd_agg_sf_for, mainland_sabah_border)
	sabah_for_w_pa_sp <- as(sabah_for, 'Spatial')
	sa_grid_w_pa_sp <- make_grid(sabah_for_w_pa_sp,  type = "square", cell_area = 5000000, clip = FALSE)
	sa_grid_w_pa <- st_as_sf(sa_grid_w_pa_sp)
	#save(sa_grid_w_pa, file = "C:/Users/saraw/Desktop/5_5_18/sa_grid_w_pa.Rdata")
	
	# ----------------------
	#  Object to lock in existing TPAs
	tpa_grid_df_tmp <- as.data.frame(st_intersects(sa_grid_w_pa, sabah_tpa_u, sparse = FALSE))
	names(tpa_grid_df_tmp)[1] <- "IN_tmp"
	tpa_grid_df <- tpa_grid_df_tmp %>%
		dplyr::mutate(IN = ifelse(IN_tmp == "FALSE", 0, 1)) %>%
		dplyr::select(IN)
	tpa_grid <- sa_grid_w_pa %>%
		bind_cols(tpa_grid_df) %>%
		dplyr::filter(IN == 1)
	#save(tpa_grid, file = "C:/Users/saraw/Desktop/5_5_18/tpa_grid.Rdata")
		
	# ----------------------
	#  SFI with existing TPAs
	sfi_grid_w_pa_df_tmp <- as.data.frame(st_intersects(sa_grid_w_pa, sfi_RT_u, sparse = FALSE))
	names(sfi_grid_w_pa_df_tmp)[1] <- "IN_tmp"
	sfi_grid_w_pa_df <- sfi_grid_w_pa_df_tmp %>%
		dplyr::mutate(IN = ifelse(IN_tmp == "FALSE", 0, 1)) %>%
		dplyr::select(IN)
	sfi_grid_w_pa <- sa_grid_w_pa %>%
		bind_cols(sfi_grid_w_pa_df) %>%
		dplyr::filter(IN == 1)
	#save(sfi_grid_w_pa, file = "C:/Users/saraw/Desktop/5_5_18/sfi_grid_w_pa.Rdata")
	
	# ----------------------
	#  Idris with existing TPAs
	idris_grid_w_pa_df_tmp <- as.data.frame(st_intersects(sa_grid_w_pa, idris_RT_u, sparse = FALSE))
	names(idris_grid_w_pa_df_tmp)[1] <- "IN_tmp"
	idris_grid_w_pa_df <- idris_grid_w_pa_df_tmp %>%
		dplyr::mutate(IN = ifelse(IN_tmp == "FALSE", 0, 1)) %>%
		dplyr::select(IN)
	idris_grid_w_pa <- sa_grid_w_pa %>%
		bind_cols(idris_grid_w_pa_df) %>%
		dplyr::filter(IN == 1)
	#save(idris_grid_w_pa, file = "C:/Users/saraw/Desktop/5_5_18/idris_grid_w_pa.Rdata")

	# ----------------------
	#  Deramakot locked out area without existing TPAs
	deram_grid_w_pa_df_tmp <- as.data.frame(st_intersects(sa_grid_w_pa, deram, sparse = FALSE))
	names(deram_grid_w_pa_df_tmp)[1] <- "IN_tmp"
	deram_grid_w_pa_df <- deram_grid_w_pa_df_tmp %>%
		dplyr::mutate(IN = ifelse(IN_tmp == "FALSE", 0, 1)) %>%
		dplyr::select(IN)
	deram_grid_w_pa <- sa_grid_w_pa %>%
		bind_cols(deram_grid_w_pa_df) %>%
		dplyr::filter(IN == 1)
	#save(deram_grid_w_pa, file = "C:/Users/saraw/Desktop/5_5_18/deram_grid_w_pa.Rdata")
	
	
	
# =============================================================================
#  Connectivity planning unit grid.
# =============================================================================


	# ----------------------
	#  Entire study area without existing TPAs - smaller for connectivity
	sabah_for <- st_intersection(acd_agg_sf_for, mainland_sabah_border)
	sabah_for_no_pa <- st_erase(sabah_for, st_buffer(sabah_tpa, 0.1))
	sabah_for_no_pa_sp <- as(sabah_for_no_pa, 'Spatial')
	sa_grid_sm_sp <- make_grid(sabah_for_no_pa_sp,  type = "square", cell_area = 1000000, clip = FALSE)
	sa_grid_sm <- st_as_sf(sa_grid_sm_sp)
	
	# ----------------------
	#  Buffer around SW Sabah (around SFI and Idris areas)
	sw_sabah_b <- st_buffer(sfi_idris_u, 45000)
	bb_tmp <- st_bbox(sw_sabah_b)	
	bb_tmp[3] <- 477809
		st_as_sfc.bbox = function(bb) {
		box = st_polygon(list(matrix(bb[c(1, 2, 3, 2, 3, 4, 1, 4, 1, 2)], ncol=2, byrow=TRUE)))
		st_sfc(box, crs=st_crs(bb))
		}
	bb <- st_as_sfc.bbox(bb_tmp)
	
	# ----------------------
	#  Select areas within entire study area that are only in SW Sabah.
	sw_sabah_c <- st_intersection(sw_sabah_b, bb)
	sw_sabah_df_tmp <- as.data.frame(st_intersects(sa_grid, sw_sabah_c, sparse = FALSE))
	names(sw_sabah_df_tmp)[1] <- "IN_tmp"
	sw_sabah_grid_df <- sw_sabah_df_tmp %>%
		dplyr::mutate(IN = ifelse(IN_tmp == "FALSE", 0, 1)) %>%
		dplyr::select(IN)
	sw_sabah_grid <- sa_grid %>%
		bind_cols(sw_sabah_grid_df) %>%
		dplyr::filter(IN == 1)
	#save(sw_sabah_grid, file = "C:/Users/saraw/Desktop/planning_unit_grids/sw_sabah_grid.Rdata")
	