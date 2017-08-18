#  Sara Williams
#  August 16, 2017; August 17, 2017
#  Script to plot adjusted species distribution polygons with 
#  information on constraints such as elevation, forest, and water dependency.

library(sf)
library(sp)
library(raster)
library(tidyverse) 
library(rasterVis)
library(RColorBrewer)
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
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/border_kali_d.Rdata")


#  Use rasterVis package
#   Set color palette theme
r_theme <- rasterTheme(rev(brewer.pal(8, "Spectral")))

plot_mams_r <- levelplot(mams_r, par.settings = r_theme, 
	main = "Threated mammal species ranges \n",
	xlab= "Longitude (UTM)",
	ylab="Latitude (UTM)") + # add margin = FALSE if no margins wanted
	layer(sp.polygons(border_sabah_d, lwd = 0.8, col = 'grey40')) + 
	layer(sp.polygons(border_sarawak_d, lwd = 0.8, col = 'grey40', fill = 'gray40')) +
	layer(sp.polygons(border_kali_d, lwd = 0.8, col = 'grey40', fill = 'gray40')) 
plot_mams_r

plot_amphs_r <- levelplot(amphs_r, par.settings = r_theme, 
	main = "Threated amphibian species ranges \n",
	xlab= "Longitude (UTM)",
	ylab="Latitude (UTM)") +
	layer(sp.polygons(border_sabah_d, lwd = 0.8, col = 'grey40')) + 
	layer(sp.polygons(border_sarawak_d, lwd = 0.8, col = 'grey40', fill = 'gray40')) +
	layer(sp.polygons(border_kali_d, lwd = 0.8, col = 'grey40', fill = 'gray40')) 
plot_amphs_r

plot_birds_r <- levelplot(birds_r, par.settings = r_theme, 
	main = "Threated bird species ranges \n",
	xlab= "Longitude (UTM)",
	ylab="Latitude (UTM)") +
	layer(sp.polygons(border_sabah_d, lwd = 0.8, col = 'grey40')) + 
	layer(sp.polygons(border_sarawak_d, lwd = 0.8, col = 'grey40', fill = 'gray40')) +
	layer(sp.polygons(border_kali_d, lwd = 0.8, col = 'grey40', fill = 'gray40')) 
plot_birds_r




 
#  Plots using sf objects and alpha (transparency)
#   Generate required sf objects
sabah_sf <- st_as_sf(border_sabah_d)
sarawak_sf <- st_as_sf(border_sarawak_d)
kali_sf <- st_as_sf(border_kali_d)

plot_mams_p <- ggplot(new_mams_sf_dat) +
	geom_sf(data = sabah_sf, colour = "grey90", fill = "grey80") +
	geom_sf(colour = "transparent", fill = "#972D15", alpha = 0.03) +
	geom_sf(data = sarawak_sf, colour = "grey70", fill = "grey70") +
	geom_sf(data = kali_sf, colour = "grey70", fill = "grey70") +
	coord_sf(crs = st_crs(32650)) +
	xlab("Latitude") +
	ylab("Longitude") +
	xlim(315000, 755000) +
	ylim(455000, 815000) +
	ggtitle("Threatened mammal species range overlap (97 species) ") +
	theme_bw()
plot_mams_p

plot_amphs_p <- ggplot(new_amphs_sf_dat) +
	geom_sf(data = sabah_sf, colour = "grey90", fill = "grey80") +
	geom_sf(colour = "transparent", fill = "#972D15", alpha = 0.2) +
	geom_sf(data = sarawak_sf, colour = "grey70", fill = "grey70") +
	geom_sf(data = kali_sf, colour = "grey70", fill = "grey70") +
	coord_sf(crs = st_crs(32650)) +
	xlab("Latitude") +
	ylab("Longitude") +
	xlim(315000, 755000) +
	ylim(455000, 815000) +
	ggtitle("Threatened amphibian species range overlap (58 species)") +
	theme_bw()
plot_amphs_p

plot_birds_p <- ggplot(new_birds_sf_dat) +
	geom_sf(data = sabah_sf, colour = "grey90", fill = "grey80") +
	geom_sf(colour = "transparent", fill = "#972D15", alpha = 0.01) +
	geom_sf(data = sarawak_sf, colour = "grey70", fill = "grey70") +
	geom_sf(data = kali_sf, colour = "grey70", fill = "grey70") +
	coord_sf(crs = st_crs(32650)) +
	xlab("Latitude") +
	ylab("Longitude") +
	xlim(315000, 755000) +
	ylim(455000, 815000) +
	ggtitle("Threatened bird species range overlap (120 species)") +
	theme_bw()
plot_birds_p





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
   
#  Plot raster-like objects
mams_r_p <- ggplot(mams_spp_gg) +
  geom_raster(aes(lons, lats, fill = n_spp)) +
  coord_equal() +
  geom_polygon(data = fortify(border_sabah_d), aes(long, lat, group=group)) +
  scale_fill_distiller(palette = "Spectral", name = "Number of spp") +
  xlab("Latitude") +
  ylab("Longitude") +
  ggtitle("Mammals - Threatened Species Ranges") +
  theme_bw()
mams_r_p


amphs_r_p <- ggplot() +
  geom_raster(data = amphs_spp_gg, aes(lons, lats, fill = n_spp)) +
  scale_fill_distiller(palette = "Spectral", name = "Number of spp") +
  xlab("Latitude") +
  ylab("Longitude") +
  ggtitle("Amphibians - Threatened Species Ranges") +
  theme_bw()
amphs_r_p 

birds_r_p <- ggplot(birds_spp_gg) +
  geom_raster(aes(lons, lats, fill = n_spp)) +
  scale_fill_distiller(palette = "Spectral", name = "Number of spp") +
  xlab("Latitude") +
  ylab("Longitude") +
  ggtitle("Birds - Threatened Species Ranges") +
  theme_bw()
birds_r_p

