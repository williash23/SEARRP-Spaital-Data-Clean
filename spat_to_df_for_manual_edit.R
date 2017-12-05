#  Sara Williams
#  June 13, 2017
#  Importing species information and adjusting species distribution polygons 
#  with spatial data such as elevation.



library(maptools)
library(rgdal)
library(raster)
library(tidyverse) 
library(stringr)
library(sf)



path <- "C:/Users/saraw/Documents/SEARRP"
path_spat_dat <- paste(path, "processed_spat_data", sep = "/")
path_df_dat <- paste(path, "processed_excel_data", sep = "/")
setwd(path)



#  Load species distirbution data using 'sf' package
load(file = paste(path_spat_dat, "trans_crop_proj/sf_mammals.Rdata", sep = "/"))
load(file = paste(path_spat_dat, "trans_crop_proj/sf_amphibians.Rdata", sep = "/"))
load(file = paste(path_spat_dat, "trans_crop_proj/sf_birds.Rdata", sep = "/"))



#  Convert shapefiles to sf_object (simple feature) for easy use with dplyr, ggplot, etc.
#   Filter only species that are threatened (critically endangered, endangered, vulnerable) 
#   and filter spp polygons that are labale 5 under the "presence" column (which indicates
#   extinct in that area according to IUCN polygon data).
sf_mammals_threat <- st_as_sf(mammals_c) %>%
	dplyr::filter(code == "CR" | code == "EN" | code == "VU" | code == "NT" | code == "DD") %>%
	dplyr::select(-citation)
	
	####  NOTE: the shapefile from the IUCN spatial data for Melogale everetti is incorrect.

	

sf_amphibians_threat <- st_as_sf(amphibians_c) %>%
	dplyr::filter(code == "CR" | code == "EN" | code == "VU"| code == "NT" | code == "DD") %>%
	dplyr::select(-citation)
	
	####  NOTE: the shapefile from the IUCN spatial data for Limnonectes ibanorum, Limnonectes rhacodus, 
	####   Rhacophorus fasciatus, and Pelophryne signata shows that these species ranges barely overlap Sabah 
	####   and thus does not have any part that falls within the elevational constraints. 

	

iucn_dat <- read.csv("C:/Users/saraw/Documents/SEARRP/raw_excel_data/iucn_all.csv")	%>%
	dplyr::select(binomial, class, order, family)
sf_birds_threat_tmp <- st_as_sf(birds_c) %>%
	dplyr::select(id_no = SISID, binomial = SCINAME, subspecies = result.subspecies, subpop = result.subpopulation,
			   RL_assess_year = DATE_, RL_code = result.category, shape_Leng = Shape_Length, 
			   shape_Area = Shape_Area) 
sf_birds_threat <- sf_birds_threat_tmp %>%
	left_join(iucn_dat, by = "binomial") %>%
	dplyr::filter(RL_code == "CR" | RL_code == "EN" | RL_code == "VU" | code == "NT" | code == "DD") 


	
# save(sf_mammals_threat, file = paste(path_spat_dat, "spp_ranges_no_constraints/sf_mammals_threat.Rdata", sep = "/"))
# save(sf_amphibians_threat, file = paste(path_spat_dat, "spp_ranges_no_constraints/sf_amphibians_threat.Rdata", sep = "/"))
# save(sf_birds_threat, file = paste(path_spat_dat, "spp_ranges_no_constraints/sf_birds_threat.Rdata", sep = "/"))




#  Write to csv to manually go through with IUCN data and collect info on 
#   elevational constraints, tolerance to distrubed habitat, water and forest dependency 
#   via Excel file

df_mammals_threat <- sf_mammals_threat %>%
	arrange(binomial, shape_Leng)
st_geometry(df_mammals_threat) <- NULL

df_amphibians_threat <- sf_amphibians_threat %>%
	arrange(binomial, shape_Leng)
st_geometry(df_amphibians_threat) <- NULL

df_birds_threat <- sf_birds_threat %>%
	arrange(binomial, shape_Leng)
st_geometry(df_birds_threat) <- NULL

#write.csv(df_mammals_threat , file =  paste(path_df_dat, "spp_data/sabah_mammals_threatened_tmp.csv", sep"/"))
#write.csv(df_amphibians_threat, file = paste(path_df_dat, "spp_data/sabah_amphibians_threatened_tmp.csv", sep"/"))
#write.csv(df_birds_threat, file = paste(path_df_dat, "spp_data/sabah_birds_threatened_tmp.csv", sep"/"))
#  Manually edited csv file will drop _tmp in file name

####  NOTE: if no constraint information is given from IUCN on elevation, range is input manually
####   as 0-4095m, or 0 as minimum for elevation ranges "up to XXm", or 4095m 
####   as maximum for emlevation ranges "above XXm"
####  If there were descrepancies on elevation constraints within the IUCN species accounts,
####   I used the more "lenient" values (i.e., the smaller lower limit and larger upper limit).




	
	
	
	
	
	
	
	

	

##  If wanted, combine spatial data and info from IUCN orJed's data sheet and filter
##   for elevational constraints; then manually go through with IUCN 
##   data and collect info on elevational constraints, tolerance to distrubed habitat, 
##   water and forest dependency via Excel file
	
#  IUCN data: Mammal, amphibians, birds from web scrape of IUCN species profiles
iucn_dat_tmp <- read.csv("C:/Users/Sara/Desktop/SEARRP/iucn_all.csv")
iucn_dat <- iucn_dat_tmp %>%
	filter(str_detect(country_range, "Malay"))
iucn_mam_dat <- iucn_dat %>%
	filter(class == "Mammalia") %>%
	select(-unit.href, -rl_stat, -habitat_range, -country_range, -sci_name)
iucn_mam_dat$binomial <- droplevels(iucn_mam_dat$binomial)
iucn_amphs_dat <- iucn_dat %>%
	filter(class == "Amphibia") 
iucn_bird_dat <- iucn_dat %>%
	filter(class == "Aves") 

#  Data from jed
#jed_mam_dat_tmp <- read.csv("C:/Users/Sara/Desktop/SEARRP/mammals_dat.csv")
#jed_mam_dat <- jed_mam_dat_tmp %>%
#	select(id_no = Species.ID, class = Class, order = Order, family = Family,
#			  genus = Genus, species = Species, binomial, forest_dep = Forest.dep, 
#			  elev_min, elev_max, RL_stat = Red.List.status, year_stat = Year.assessed) %>%
#	filter(RL_stat != "LC")


#  Combine ....