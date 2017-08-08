#  Sara Williams
#  June 13, 2017; July 15, 2017
#  Script to run function to adjust species distribution polygons with 
#  information on constraints such as elevation and forest and water dependency.

library(maptools)
library(rgdal)
library(raster)
library(sf)
library(tidyverse) 
library(stringr)

setwd("C:/Users/saraw/Documents/SEARRP")

#  Load environmental spatial data files created in spat_dat_exp.R script
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/elev_250m_c.Rdata")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/hydro_vec_b.Rdata")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/log_rds_c.Rdata")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/for_cov_c.Rdata")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/border_sabah_d.Rdata")

#  Source raster_to_polygon function and adj_species_elev_poly function
source("C:/Users/saraw/Documents/SEARRP/scripts/raster_to_polygon_fun.R")
source("C:/Users/saraw/Documents/SEARRP/scripts/adj_species_poly_fun.R")



#  Run function to adjust spp ranges based on elevational constraints
adj_spp_poly_elev_fun(dat = "C:/Users/saraw/Documents/SEARRP/processed_excel_data/sabah_mammals_threatened.csv",
	sf = "C:/Users/saraw/Documents/SEARRP/processed_spat_data/sf_mammals_threat.rds",
	new_spp_sp = "mammals.shp") #, 
	#new_spp_sf = "new_mammal_elev_w_dat.rds")
####  NOTE: the shapefile from the IUCN spatial data for Melogale everetti is incorrect.
####   Must leave this spp out for the loop to work. It has been removed from
####   the larger data files but saved on it's own CSV file.							

adj_spp_poly_elev_fund(dat = "C:/Users/saraw/Documents/SEARRP/processed_excel_data/sabah_amphibians_threatened.csv",
	sf = "C:/Users/saraw/Documents/SEARRP/processed_spat_data/sf_amphs_threat.rds",
	new_spp_sp = "amphibians.shp") #, 
	#new_spp_sf = "new_amphibian_elev_w_dat.rds")
####  NOTE: the shapefile from the IUCN spatial data for Limnonectes ibanorum, Limnonectes rhacodus, 
####   Rhacophorus fasciatus, andPelophryne signata shows that these species ranges barely overlap Sabah 
####   and thus does not have any part that falls within the elevational constraints. Must leave this spp out 
####   for the loop to work. It has been removed from the larger data files but saved on it's own CSV file.		

adj_spp_poly_elev_fun(dat = "C:/Users/saraw/Documents/SEARRP/processed_excel_data/sabah_birds_threatened.csv",
	sf = "C:/Users/saraw/Documents/SEARRP/processed_spat_data/sf_birds_threat.rds",
	new_spp_sp = "birds.shp") #, 
	#new_spp_sf = "new_bird_elev_w_dat.rds")



#  Run function to adjust spp ranges based on water dependency constraints.
#   Convert water feature sf data to sp format for all runs
hydro_vec_b_sp <- as(hydro_vec_b, "Spatial")	








#  Plot
ggplot(new_mam_ranges_dat_sf) +
  geom_sf(aes(fill = RL_code)) +
  scale_color_viridis("IUCN RL Status") +
  coord_sf(crs = st_crs(new_mam_ranges_dat_sf)) +
  theme_bw()

  
 