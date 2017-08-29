#  Sara Williams
#  August 18, 2017; August 29, 2017
#  Script to prepare forest dover raster for clustering into focal nodes of contiguous forest


library(rgeos)
library(maptools)
library(rgdal)
library(raster)
library(rasterVis)
library(RColorBrewer)



#  Load raw data 
load(file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/border_sabah_d.Rdata")
for_cov <- raster("C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/for_cov_c.grd")


# Info on raw raster
#  Raster values:
# Value   	Count				Class                
# 0 		104,621					No Data
# 1 		227,111,756			Intact forest (2016)
# 2		180,829,207			Logged forest (2016)
# 3 		8,578,050				Regrowth (1973-2016)
# 4 		399,953,847			Non-forest
# 5 		6,719,940				Water     

	

#  Convert raster to polygons based on forest cover value (0-5)
#   Method 1: using polygonizer() function. Done only once and saved output.
#for_cov_p <- polygonizer(for_cov)
# Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
# ..@ data       :'data.frame': 210210 obs. of  1 variable:
# .. ..$ DN: int [1:210210] 1 1 1 1 1 0 0 0 0 0 ...
# ..@ polygons   :List of 210210
#writeOGR(for_cov_p,"C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps", "for_cov_p", driver="ESRI Shapefile")


#####  NOTE: The following steps are a less efficient way of achieveing the same results as
#####  Method 2 above. Documented here, but not used. 
#  Method 2
#   Reclassify the main forest cover raster layer (which has 6 values - 0 to 5 - so that each new raster layer has only 
#   values at the pertinent level. 

#  Set up reclassification matrix for each value and generate a new raster for each value individually
rcl_0 <- matrix(c(1, NA, 2, NA, 3, NA, 4, NA, 5, NA), nrow = 5, ncol = 2, byrow = TRUE)
for_cov_0 <- raster::reclassify(for_cov, rcl_0)

rcl_1 <- matrix(c(0, NA, 2, NA, 3, NA, 4, NA, 5, NA), nrow = 5, ncol = 2, byrow = TRUE)
for_cov_1 <- raster::reclassify(for_cov, rcl_1)

rcl_2 <- matrix(c(0, NA, 1, NA, 3, NA, 4, NA, 5, NA), nrow = 5, ncol = 2, byrow = TRUE)
for_cov_2 <- raster::reclassify(for_cov, rcl_2)

rcl_3 <- matrix(c(0, NA, 1, NA, 2, NA, 4, NA, 5, NA), nrow = 5, ncol = 2, byrow = TRUE)
for_cov_3 <- raster::reclassify(for_cov, rcl_3)

rcl_4 <- matrix(c(0, NA, 1, NA, 2, NA, 3, NA, 5, NA), nrow = 5, ncol = 2, byrow = TRUE)
for_cov_4 <- raster::reclassify(for_cov, rcl_4)

rcl_5 <- matrix(c(0, NA, 1, NA, 2, NA, 3, NA, 4, NA), nrow = 5, ncol = 2, byrow = TRUE)
for_cov_5 <- raster::reclassify(for_cov, rcl_5)



#  Plot raw raster, just to take a look
for_cov_rat <- ratify(for_cov)
rat <- levels(for_cov_rat )[[1]]
rat$landcover <- c('No Data', 'Intact Forest', 'Logged Forest', 'Regrowth', 'Non-forest', 'Water')
rat$class <- c('No Data', 'Intact Forest', 'Logged Forest', 'Regrowth', 'Non-forest', 'Water')
levels(for_cov_rat) <- rat

#   All forest types different colors
levelplot(for_cov_rat, col.regions=c("darkgreen", "yellowgreen", "grey30", "wheat3", "yellow1", "deepskyblue3"), 
	margin = FALSE, xlim = c(300000, 750000), ylim = c(450000, 800000))
#  Intact, logged and regrowth forest same color
levelplot(for_cov_rat, col.regions=c("darkgreen", "darkgreen", "grey30", "wheat3", "darkgreen", "deepskyblue3"), 
	margin = FALSE, xlim = c(300000, 750000), ylim = c(450000, 800000))



#   Clump each new raster layer so that adjacent cells are grouped within the same ID
clump(for_cov_0, filename="C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps/for_cov_0clump.grd", directions=8)
clump(for_cov_1, filename="C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps/for_cov_1clump.grd", directions=8)
clump(for_cov_2, filename="C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps/for_cov_2clump.grd", directions=8)
clump(for_cov_3, filename="C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps/for_cov_3clump.grd", directions=8)
clump(for_cov_4, filename="C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps/for_cov_4clump.grd", directions=8)
clump(for_cov_5, filename="C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps/for_cov_5clump.grd", directions=8)

for_cov0 <- raster("C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps/for_cov_0clump.grd")
for_cov1 <- raster("C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps/for_cov_1clump.grd")
for_cov2 <- raster("C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps/for_cov_2clump.grd")
for_cov3 <- raster("C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps/for_cov_3clump.grd")
for_cov4 <- raster("C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps/for_cov_4clump.grd")
for_cov5 <- raster("C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps/for_cov_5clump.grd")



#   Convert raster to polygons of each clumped forest cover type
for_cov1_p <- polygonizer(for_cov1)
for_cov2_p <- polygonizer(for_cov2)
for_cov3_p <- polygonizer(for_cov3)
for_cov4_p <- polygonizer(for_cov4)
for_cov5_p <- polygonizer(for_cov5)

save(for_cov1_p, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps/for_cov1_p.Rdata")
save(for_cov2_p, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps/for_cov2_p.Rdata")
save(for_cov3_p, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps/for_cov3_p.Rdata")
save(for_cov4_p, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps/for_cov4_p.Rdata")
save(for_cov5_p, file="C:/Users/saraw/Documents/SEARRP/processed_spat_data/forest_clumps/for_cov5_p.Rdata")

#  Turn into sf objects and join with a data frame of forest cover type
for_cov1_sf <- st_as_sf(for_cov1_p)
for_cov1_dat <- data.frame(rep(1, nrow(for_cov1_sf))
colnames(for_cov1_dat)[1] <- "for_cov"
for_cov1_sf <- merge(for_cov1_sf, for_cov1_dat)

for_cov2_sf <- st_as_sf(for_cov2_p)
for_cov2_dat <- data.frame(rep(2, nrow(for_cov2_sf))
colnames(for_cov2_dat)[1] <- "for_cov"

for_cov3_sf <- st_as_sf(for_cov3_p)
for_cov3_dat <- data.frame(rep(3, nrow(for_cov3_sf))
colnames(for_cov1_dat)[1] <- "for_cov"

for_cov4_sf <- st_as_sf(for_cov4_p)
for_cov4_dat <- data.frame(rep(4, nrow(for_cov4_sf))
colnames(for_cov4_dat)[1] <- "for_cov"

for_cov5_sf <- st_as_sf(for_cov5_p)
for_cov5_dat <- data.frame(rep(5, nrow(for_cov5_sf))
colnames(for_cov5_dat)[1] <- "for_cov"

#  Merge all forest types
for_cov12_sf <- st_intersection(for_cov1_sf, for_cov2_sf)
for_cov123_sf <- st_intersection(for_cov12_sf, for_cov3_sf)