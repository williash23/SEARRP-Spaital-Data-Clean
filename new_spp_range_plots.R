#  Sara Williams
#  August 16, 2017
#  Script to plot adjusted species distribution polygons with 
#  information on constraints such as elevation, forest, and water dependency.

library(sf)
library(sp)
library(viridis)
library(raster)
library(tidyverse) 
####  NOTE: Must install the development version of ggpot2 to use geom_sf().
####   devtools::install_github("tidyverse/ggplot2")


#  Load new spp range polygons
mams_new <- shapefile("C:/Users/saraw/Documents/SEARRP/processed_spat_data/new_ranges/mammals_elev.shp")
amphs_new <- shapefile("C:/Users/saraw/Documents/SEARRP/processed_spat_data/new_ranges/amphibians_elev.shp")
birds_new <- shapefile("C:/Users/saraw/Documents/SEARRP/processed_spat_data/new_ranges/birds_elev.shp")

#  Load other spp info
mams_dat <- read.csv("C:/Users/saraw/Documents/SEARRP/processed_excel_data/sabah_mammals_threatened.csv")
amphs_dat <- read.csv("C:/Users/saraw/Documents/SEARRP/processed_excel_data/sabah_amphibians_threatened.csv")
birds_dat <- read.csv("C:/Users/saraw/Documents/SEARRP/processed_excel_data/sabah_birds_threatened.csv")

#  Load environmental/border data
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/log_rds_sf.Rdata")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/for_cov_c.Rdata")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/border_sabah_d.Rdata")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/border_sarawak_d.Rdata")

#  Convert to sf objects
mams_sf <- st_as_sf(mams_new)
amphs_sf <- st_as_sf(amphs_new)
birds_sf <- st_as_sf(birds_new)

sarawak_sf <- st_as_sf(border_sarawak_d)

#  Combine sf object with other spp info
mams_sf_dat <- bind_cols(mams_sf, mams_dat)
amphs_sf_dat <- bind_cols(amphs_sf, amphs_dat)
birds_sf_dat <- bind_cols(birds_sf, birds_dat)



  
#  Rasterize the number of polygons overlapping each cell
#   Make empty raster template and SPDF
r_template <- raster(ncols = 1000, nrows = 1000, 
   crs = projection(border_sabah_d), 
   ext = extent(border_sabah_d))
pts <- rasterToPoints(r_template, spatial = TRUE)
proj4string(pts) = proj4string(mams_new) # otherwise over will fail here;
pts$n = sapply(over(pts, geometry(mams_new), returnList = TRUE), length)
gridded(pts) = TRUE


spplot(pts["n"], sp.layout=list("sp.polygons", mams_new, first=F))


mams_r <- rasterize(mams_new, r_template, fun = 'count')
 
#  Plot
mams_p <- ggplot(mams_sf_dat) +
  geom_sf(colour = "transparent", fill = "darkorange", alpha = 0.02) +
  #geom_sf(data = log_rds_sf, colour = "darkgrey") +
  coord_sf(crs = st_crs(32650)) +
  xlab("Latitude") +
  ylab("Longitude") +
  theme_bw()
mams_p
