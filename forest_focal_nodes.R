#  Sara Williams
#  August 18, 2017
#  Script to make "focal node" areas of contiguous forest



library(maptools)
library(rgdal)
library(raster)



for_cov <- raster("C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/for_cov_c.grd")



#  Method 1
for_cov_p <- polygonizer(for_cov)
  # Formal class 'SpatialPolygonsDataFrame' [package "sp"] with 5 slots
  # ..@ data       :'data.frame': 210210 obs. of  1 variable:
  # .. ..$ DN: int [1:210210] 1 1 1 1 1 0 0 0 0 0 ...
  # ..@ polygons   :List of 210210


#  Method 2

# reclassify the values into three groups

for_cov_0 <- raster::reclassify(for_cov, c(-Inf, -1, NA, -0.5, 0.5, 1, 1, Inf, NA))
for_cov_1 <- raster::reclassify(for_cov, c(-Inf, 0, NA, 0.5, 1.5, 1, 2, Inf, NA))
for_cov_2 <- raster::reclassify(for_cov, c(-Inf, 1, NA, 1.5, 2.5, 1, 3, Inf, NA))
for_cov_3 <- raster::reclassify(for_cov, c(-Inf, 2, NA, 2.5, 3.5, 1, 4, Inf, NA))
for_cov_4 <- raster::reclassify(for_cov, c(-Inf, 3, NA, 3.5, 4.5, 1, 5, Inf, NA))
for_cov_5 <- raster::reclassify(for_cov, c(-Inf, 4, NA, 4.5, 5.5, 1, 6, Inf, NA))


clump(for_cov_0, filename="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/for_cov_0clump.grd", directions=8)
clump(for_cov_1, filename="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/for_cov_1clump.grd", directions=8)
clump(for_cov_2, filename="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/for_cov_2clump.grd", directions=8)
clump(for_cov_3, filename="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/for_cov_3clump.grd", directions=8)
clump(for_cov_4, filename="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/for_cov_4clump.grd", directions=8)
clump(for_cov_5, filename="C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/for_cov_5clump.grd", directions=8)


writeRaster(for_cov_0,"C:/Users/saraw/Documents/SEARRP/processed_spat_data/trans_crop_proj/for_cov_0.grd")