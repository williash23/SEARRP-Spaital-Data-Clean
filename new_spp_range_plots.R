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



#  Load new spp range polygons sf objects, SpatialPixelsDataFrames and rasters
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/new_mams_sf_dat.rds")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/new_amphs_sf_dat.rds")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/new_birds_sf_dat.rds")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/pts_mams.rds")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/pts_amphs.rds")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/pts_ birds.rds")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/mams_r.rds")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/amphs_r.rds")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/maps/birds_r.rds")

#  Load environmental/border data
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/log_rds_sf.Rdata")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/for_cov_c.Rdata")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/border_sabah_d.Rdata")
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/border_sarawak_d.Rdata")


#  Function and data prep to use ggplot with RasterLayer
raster_ggplot <- function(rastx) {
  require(SDMTools)
  stopifnot(class(rastx) == "RasterLayer")
  gfx_data <- getXYcoords(rastx)
  # lats need to be flipped
  gfx_data <- expand.grid(lons = gfx_data$x, lats = rev(gfx_data$y), 
                            stringsAsFactors = FALSE, KEEP.OUT.ATTRS = FALSE)
  gfx_data$n_spp <- rastx@data@values
  return (gfx_data)
}

#  Convert to data frame readily available for ggplot2
mams_spp_gg <- raster_ggplot(mams_r)
amphs_spp_gg <- raster_ggplot(amphs_r)
birds_spp_gg <- raster_ggplot(birds_r)





mams_r_p <- ggplot(mams_spp_gg) +
  geom_raster(aes(lons, lats, fill = n_spp)) +
  #geom_sf(data = log_rds_sf, colour = "darkgrey") +
  #coord_sf(crs = st_crs(32650)) +
  scale_fill_distiller(palette = "Spectral", name = "Number of spp") +
  #scale_fill_gradient(low = "white", high = "red", guide = "colourbar") +
  xlab("Latitude") +
  ylab("Longitude") +
  ggtitle("Mammals - Threatened Species Ranges") +
  theme_bw()
mams_r_p





 
#  Plots using polygons and alpha (transparency)
mams_p <- ggplot(mams_sf_dat) +
  geom_sf(colour = "transparent", fill = "darkred", alpha = 0.018) +
  #geom_sf(data = log_rds_sf, colour = "darkgrey") +
  coord_sf(crs = st_crs(32650)) +
  xlab("Latitude") +
  ylab("Longitude") +
  ggtitle("Mammals - Threatened Species Ranges") +
  theme_bw()
mams_p

amphs_p <- ggplot(amphs_sf_dat) +
  geom_sf(colour = "transparent", fill = "darkred", alpha = 0.018) +
  #geom_sf(data = log_rds_sf, colour = "darkgrey") +
  coord_sf(crs = st_crs(32650)) +
  xlab("Latitude") +
  ylab("Longitude") +
  ggtitle("Amphibians - Threatened Species Ranges") +
  theme_bw()
amphs_p

birds_p <- ggplot(birds_sf_dat) +
  geom_sf(colour = "transparent", fill = "darkred", alpha = 0.018) +
  #geom_sf(data = log_rds_sf, colour = "darkgrey") +
  coord_sf(crs = st_crs(32650)) +
  xlab("Latitude") +
  ylab("Longitude") +
  ggtitle("Birds - Threatened Species Ranges") +
  theme_bw()
birds_p




# #  Plot using raster converted to sf object
# pts_mams_sf <- st_as_sf(pts_mams)

# mams_r_p <- ggplot(pts_mams_sf) +
  # geom_sf(fill = n) +
  # #geom_sf(data = log_rds_sf, colour = "darkgrey") +
  # #coord_sf(crs = st_crs(32650)) +
  # scale_fill_viridis("Number of species present") +
  # xlab("Latitude") +
  # ylab("Longitude") +
  # ggtitle("Mammals - Threatened Species Ranges") +
  # theme_bw()
# mams_r_p
