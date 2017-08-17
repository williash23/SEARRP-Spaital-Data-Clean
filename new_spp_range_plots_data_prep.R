#  Sara Williams
#  August 16, 2017; August 17, 2017
#  Script to plot adjusted species distribution polygons with 
#  information on constraints such as elevation, forest, and water dependency.

library(sf)
library(sp)
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



#  Convert to sf objects
mams_sf <- st_as_sf(mams_new)
amphs_sf <- st_as_sf(amphs_new)
birds_sf <- st_as_sf(birds_new)

#  Combine sf object with other spp info
new_mams_sf_dat <- bind_cols(mams_sf, mams_dat)
new_amphs_sf_dat <- bind_cols(amphs_sf, amphs_dat)
new_birds_sf_dat <- bind_cols(birds_sf, birds_dat)

save(new_mams_sf_dat, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/new_mams_sf_dat.rds")
save(new_amphs_sf_dat, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/new_amphs_sf_dat.rds")
save(new_birds_sf_dat, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/new_birds_sf_dat.rds")



#  Rasterize via SpatialPixelsDataFrame the number of polygons overlapping each cell

#   Make empty raster template and SPDF
r_template <- raster(ncols = 500, nrows = 500, 
   crs = projection(border_sabah_d), 
   ext = extent(border_sabah_d))
   
#   Convert raster to points over area and count the number of polygons that overlay each point
#   Mammals
pts_mams <- rasterToPoints(r_template, spatial = TRUE)
proj4string(pts_mams) = proj4string(mams_new) # otherwise over will fail here;
pts_mams$n = sapply(over(pts_mams, geometry(mams_new), returnList = TRUE), length)
gridded(pts_mams) = TRUE
#   Amphibians
pts_amphs <- rasterToPoints(r_template, spatial = TRUE)
proj4string(pts_amphs) = proj4string(amphs_new) # otherwise over will fail here;
pts_amphs$n = sapply(over(pts_amphs, geometry(amphs_new), returnList = TRUE), length)
gridded(pts_amphs) = TRUE
#   Birds
pts_birds <- rasterToPoints(r_template, spatial = TRUE)
proj4string(pts_birds) = proj4string(birds_new) # otherwise over will fail here;
pts_birds$n = sapply(over(pts_birds, geometry(birds_new), returnList = TRUE), length)
gridded(pts_birds) = TRUE

# save(pts_mams, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/pts_mams.rds")
# save(pts_amphs, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/pts_amphs.rds")
# save(pts_birds, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/pts_ birds.rds")



#  Convert to raster 
mams_r <- raster(pts_mams)
amphs_r <- raster(pts_amphs)
birds_r <- raster(pts_birds)

# save(mams_r, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/mams_r.rds")
# save(amphs_r, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/amphs_r.rds")
# save(birds_r, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/birds_r.rds")
