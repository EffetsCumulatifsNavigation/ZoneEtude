# Libraries
library(sf)
library(magrittr)
library(tidyverse)
library(raster)
library(stars)

# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Import study area
# -----------------
#
#
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Load shapefile
tc <- st_read('./Data/StudyArea/StudyArea.geojson') %>%
      mutate(val_ras = 1)

tcbuf <- st_buffer(tc, 2600)

# Bounding box
bb <- st_bbox(tcbuf)
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #


# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Raster 2x2km
# ------------
#
#
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Create raster with resolution 2km x 2km
template <- raster(xmn = bb[1], xmx = bb[3], ymn = bb[2], ymx = bb[4], resolution = c(2000,2000), crs = 32198) %>%
            setValues(NA) %>%
            st_as_stars()

# Rasterize study area
r2000 <- stars::st_rasterize(tcbuf["val_ras"], template = template)
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #


# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Raster 1x1km
# ------------
#
#
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Warp 2km x 2km raster to 1km x 1km raster
r1000 <- st_warp(r2000, cellsize = c(1000,1000), crs = 32198)
st_crs(r1000) <- 32198 # This is weird, and may be a mistake
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #



# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Polygons grid
# -------------
#
#
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
p2000 <- st_as_sf(r2000, as_points = FALSE, merge = FALSE)
p1000 <- st_as_sf(r1000, as_points = FALSE, merge = FALSE)
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #



# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Export
# ------
#
#
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
write_stars(r2000, dsn = "./Data/StudyGrid/Grid_Raster2000.gpkg", driver = "GPKG")
write_stars(r1000, dsn = "./Data/StudyGrid/Grid_Raster1000.gpkg", driver = "GPKG")
st_write(p2000, dsn = './Data/StudyGrid/Grid_Poly2000.geojson', append = FALSE)
st_write(p1000, dsn = './Data/StudyGrid/Grid_Poly1000.geojson', append = FALSE)
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
