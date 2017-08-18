#  Sara Williams
#  June 13, 2017; July 15, 2017; August 9, 2017
#  Script to run function to adjust species distribution polygons with 
#  information on constraints such as elevation, forest, and water dependency.

library(maptools)
library(rgdal)
library(raster)
library(sf)
library(tidyverse) 
library(stringr)

setwd("C:/Users/saraw/Documents/SEARRP")

#  Load environmental spatial data files created in spat_dat_exp.R script
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/elev_250m_c.Rdata")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/hydro_vec_c.Rdata")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/coast_str.Rdata")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/log_rds_c.Rdata")
for <- raster("C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/for_cov_c.grd")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/border_sabah_d.Rdata")

#  Source raster_to_polygon function and adj_species_elev_poly function
source("C:/Users/saraw/Documents/SEARRP/scripts/raster_to_polygon_fun.R")
source("C:/Users/saraw/Documents/SEARRP/scripts/adj_species_poly_elev_fun.R")
source("C:/Users/saraw/Documents/SEARRP/scripts/adj_species_poly_water_fun.R")


#  Run function to adjust spp ranges based on elevational constraints
adj_spp_poly_elev_fun(
	dat = "C:/Users/saraw/Documents/SEARRP/processed_excel_data/sabah_mammals_threatened.csv",
	sf = "C:/Users/saraw/Documents/SEARRP/processed_spat_data/sf_mammals_threat.rds",
	new_spp_sp = "mammals_elev.shp") #, 
	#new_spp_sf = "new_mammal_elev_w_dat.rds")
####  NOTE: the shapefile from the IUCN spatial data for Melogale everetti is incorrect.
####   Must leave this spp out for the loop to work. It has been removed from
####   the larger data files but saved on it's own CSV file.							

adj_spp_poly_elev_fund(
	dat = "C:/Users/saraw/Documents/SEARRP/processed_excel_data/sabah_amphibians_threatened.csv",
	sf = "C:/Users/saraw/Documents/SEARRP/processed_spat_data/sf_amphs_threat.rds",
	new_spp_sp = "amphibians_elev.shp") #, 
	#new_spp_sf = "new_amphibian_elev_w_dat.rds")
####  NOTE: the shapefile from the IUCN spatial data for Limnonectes ibanorum, Limnonectes rhacodus, 
####   Rhacophorus fasciatus, andPelophryne signata shows that these species ranges barely overlap Sabah 
####   and thus does not have any part that falls within the elevational constraints. Must leave this spp out 
####   for the loop to work. It has been removed from the larger data files but saved on it's own CSV file.		

adj_spp_poly_elev_fun(
	dat = "C:/Users/saraw/Documents/SEARRP/processed_excel_data/sabah_birds_threatened.csv",
	sf = "C:/Users/saraw/Documents/SEARRP/processed_spat_data/sf_birds_threat.rds",
	new_spp_sp = "birds_elev.shp") #, 
	#new_spp_sf = "new_bird_elev_w_dat.rds")



#  Run function to adjust spp ranges based on water dependency constraints.
#   First, create buffer of desired size around all water (rivers and coastline) - currently 1000m
rivers <- st_as_sf(hydro_vec_c)
rivers_b <- st_buffer(rivers, 1000)
coast_b <- st_buffer(coast_str, 990) # coast line already includes a 10m buffer from coast line creation
all_water <- st_union(rivers_b, coast_b)
all_water_sp <- as(all_water, "Spatial")
rivers_sp <- as(rivers_b, "Spatial")

adj_spp_poly_water_fun(
	dat = "C:/Users/saraw/Documents/SEARRP/processed_excel_data/sabah_mammals_threatened.csv",
	sf = "C:/Users/saraw/Documents/SEARRP/processed_spat_data/sf_mammals_threat.rds",
	new_spp_sp = "mammals_water.shp",
	water_feature = all_water_sp) #, 
	#new_spp_sf = "new_mammal_water_w_dat.rds")

adj_spp_poly_water_fun(
	dat = "C:/Users/saraw/Documents/SEARRP/processed_excel_data/sabah_amphibians_threatened.csv",
	sf = "C:/Users/saraw/Documents/SEARRP/processed_spat_data/sf_amphs_threat.rds",
	new_spp_sp = "amphibians_water.shp",
	water_feature = rivers_sp) #, 
	#new_spp_sf = "new_amphibian_water_w_dat.rds")
####  NOTE: the shapefile from the IUCN spatial data for Staurois parvus show that it covers 
####   only two very small areas. Until finding a hydrological spatial data set with finer detial, 
####   must designate this spp as not water dependent ("0") for the loop to work. 

adj_spp_poly_water_fun(
	dat = "C:/Users/saraw/Documents/SEARRP/processed_excel_data/sabah_birds_threatened.csv",
	sf = "C:/Users/saraw/Documents/SEARRP/processed_spat_data/sf_birds_threat.rds",
	new_spp_sp = "birds_water.shp",
	water_feature = all_water_sp) #, 
	#new_spp_sf = "new_bird_water_w_dat.rds")



amphs_new <- shapefile("C:/Users/saraw/Documents/SEARRP/processed_spat_data/new_ranges/amphibians_water.shp")
amphs_new_sf <- st_as_sf(amphs_new)
#  Plot
ggplot(new_mam_ranges_dat_sf) +
  geom_sf(aes(fill = RL_code)) +
  scale_color_viridis("IUCN RL Status") +
  coord_sf(crs = st_crs(new_mam_ranges_dat_sf)) +
  theme_bw()

  
 