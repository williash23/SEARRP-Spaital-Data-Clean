#  Function to create grid, sourced from M. Strimas: http://strimas.com/spatial/hexagonal-grids/

library(dplyr)
library(tidyr)
library(sp)
library(raster)
library(rgeos)
library(rgbif)
library(viridis)
library(gridExtra)
library(rasterVis)
set.seed(1)

make_grid <- function(x, type, cell_width, cell_area, clip = FALSE) {
  if (!type %in% c("square", "hexagonal")) {
    stop("Type must be either 'square' or 'hexagonal'")
  }
  
  if (missing(cell_width)) {
    if (missing(cell_area)) {
      stop("Must provide cell_width or cell_area")
    } else {
      if (type == "square") {
        cell_width <- sqrt(cell_area)
      } else if (type == "hexagonal") {
        cell_width <- sqrt(2 * cell_area / sqrt(3))
      }
    }
  }
  # buffered extent of study area to define cells over
  ext <- as(extent(x) + cell_width, "SpatialPolygons")
  projection(ext) <- projection(x)
  # generate grid
  if (type == "square") {
    g <- raster(ext, resolution = cell_width)
    g <- as(g, "SpatialPolygons")
  } else if (type == "hexagonal") {
    # generate array of hexagon centers
    g <- spsample(ext, type = "hexagonal", cellsize = cell_width, offset = c(0, 0))
    # convert center points to hexagons
    g <- HexPoints2SpatialPolygons(g, dx = cell_width)
  }
  
  # clip to boundary of study area
  if (clip) {
    g <- gIntersection(g, x, byid = TRUE)
  } else {
    g <- g[x, ]
  }
  # clean up feature IDs
  row.names(g) <- as.character(1:length(g))
  return(g)
}

#  Example use
ecuador <- getData(name = "GADM", country = "ECU", level = 0, 
                   path = "data/hexagonal-grids/") %>% 
  disaggregate %>% 
  geometry
# exclude gapalapos
ecuador <- sapply(ecuador@polygons, slot, "area") %>% 
  {which(. == max(.))} %>% 
  ecuador[.]
# albers equal area for south america
ecuador <- spTransform(ecuador, CRS(
  paste("+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60",
        "+x_0=0 +y_0=0 +ellps=aust_SA +units=km +no_defs")))
hex_ecu <- make_grid(ecuador, type = "hexagonal", cell_area = 2500, clip = FALSE)


#  Polygon coverage: cell areas example
hex_area <- make_grid(ecuador, type = "hexagonal", cell_area = 2500, clip = TRUE)
hex_area <- gArea(hex_area, byid = T) %>% 
  data.frame(id = names(.), area = ., stringsAsFactors = FALSE) %>% 
  SpatialPolygonsDataFrame(hex_area, .)
hex_cover <- gIntersection(hex_area, pastaza, byid = TRUE) %>% 
  gArea(byid = TRUE) %>% 
  data.frame(id_both = names(.), cover_area = ., stringsAsFactors = FALSE) %>% 
  separate(id_both, "id", extra = "drop") %>% 
  merge(hex_area, ., by = "id")
hex_cover$cover_area[is.na(hex_cover$cover_area)] <- 0
hex_cover$pct_cover <- 100 * hex_cover$cover_area / hex_cover$area
#  plot
 area
p1 <- spplot(hex_cover, "cover_area", col = "white", lwd = 0.5,
       main = expression(km^2),
       col.regions = plasma(256),
       par.settings = list(axis.line = list(col =  'transparent')),
       colorkey = list(
         space = "bottom",
         at = seq(0, 2500, length.out = 256),
         axis.line = list(col =  'black'))
       )
# percent cover
p2 <- spplot(hex_cover, "pct_cover", col = "white", lwd = 0.5,
       main = expression("%"),
       col.regions = plasma(256),
       par.settings = list(axis.line = list(col =  'transparent')),
       colorkey = list(
         space = "bottom",
         at = seq(0, 100, length.out = 256),
         axis.line = list(col =  'black'))
       )
grid.arrange(p1, p2, ncol = 2, top = "Ecuador: Coverage by Pastaza State")

#  Raster aggregation over hexagonal cells example
srtm <- getData('alt', country = 'ECU', path = "data/hexagonal-grids/") %>% 
  projectRaster(t_crop, to = raster(hex_ecu, res=1)) %>% 
  setNames('elevation')
 hex_srtm <- extract(srtm, hex_ecu, fun = mean, na.rm = TRUE, sp = TRUE)
p1 <- levelplot(srtm, 
                col.regions = terrain.colors,
                margin = FALSE, scales = list(draw = FALSE),
                colorkey = list(
                  #space = "bottom",
                  at = seq(0, 6000, length.out = 256),
                  labels = list(at = 1000 * 0:6, 
                                labels = format(1000 * 0:6, big.mark = ","))
                )
      )
p2 <- spplot(hex_srtm,
             col.regions = terrain.colors(256),
             at = seq(0, 4000, length.out = 256),
             colorkey = list(
               labels = list(at = seq(0, 4000, 500), 
                             labels = format(seq(0, 4000, 500), big.mark = ","))
             )
      )
grid.arrange(p1, p2, ncol = 2, top = "Ecuador SRTM Elevation (m)")