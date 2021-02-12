# Libraries
library(sf)
library(magrittr)
library(tidyverse)

# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Shoreline classification
# ------------------------
#
#
# Start with the shoreline classification
# It is coarser that what we wish to use, but it'll be useful to work with the
# BDTQ dataset
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

# The shoreline classification is available at:
dataURL <- 'http://data.ec.gc.ca/data/sites/emergencies/shoreline-segmentation-with-shoreline-cleanup-assessment-technique-scat-classification/quebec-saint-lawrence-river-shoreline-classification/ShorelineClassification_QC_OpenDataCatalogue.gdb.zip'

# Name of file to download
output <- './Data/Shoreline/Shoreline.zip'

# Download file
download.file(dataURL, destfile = output)

# Unzip file
unzip(zipfile = output, exdir = './Data/Shoreline/')

# Load file
shore <- st_read("./Data/Shoreline/ShorelineClassification_QC_OpenDataCatalogue.gdb") %>%
         st_transform(4326)
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #




# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# West-East limit
# ---------------
#
#
# The study area is limited at:
#   - West: Châteaugay c(45.36036045135636, -73.75031695865471)
#   - North-East: Pointe-des-Monts c(49.3201737746945, -67.3863570885806)
#   - South-East: Cap-Chat c(49.08400517243045, -66.73280250278107)
#   - Saguenay: Saint-Fulgence c(48.45866465291821, -70.90417373996353)
#
# Clip shoreline classification
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Coordinates to form polygon
ch <- c(-73.75031695865471, 45.36036045135636)
pdm <- c(-67.3863570885806, 49.3201737746945)
cc <- c(-66.73280250278107, 49.08400517243045)
stf <- c(-70.90417373996353, 48.45866465291821)

# Matrix of coordinates
mat <- matrix(c(ch, ch[1], ch[2]+1, stf, pdm[1]-.4, pdm[2]+.1, pdm, cc, cc[1]-6, ch[2], ch), ncol = 2, byrow = TRUE,
              dimnames = list(c(), c('Longitude','Latitude')))

# Geometry to crop extent
bb <- mat %>%
      as.data.frame() %>%
      st_as_sf(coords = c('Longitude','Latitude'), crs = 4326) %>%
      group_by() %>%
      summarise(geometry = st_combine(geometry)) %>%
      st_cast("POLYGON")
mapview(bb)

# Crop shoreline classification
shore <- st_crop(shore, bb)
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #



# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# BDTQ tile index
# ---------------
#
# BDTQ data offers a more detailed coast outline:
# https://www.donneesquebec.ca/recherche/dataset/cartes-topographiques-a-l-echelle-de-1-20-000
#
# Download the BDTQ tile index
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

# URL to get the tile index
dataURL <- 'ftp://transfert.mern.gouv.qc.ca/public/diffusion/RGQ/Documentation/BDTQ/Index_BDTQ.zip'

# Name of file to download
output <- './Data/BDTQ/Index/BDTQ_index.zip'

# Download file
download.file(dataURL, destfile = output)

# Unzip file
unzip(zipfile = output, exdir = './Data/BDTQ/Index/')

# Load file
index <- st_read("./Data/BDTQ/Index/Index_BDTQ.shp") %>%
         st_transform(4326)
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #



# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Identify BDTQ tiles to load
# ---------------------------
#
#
# Intersect with shoreline classification
# Then extract url for tile data needed
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Intersect with shoreline classification to identify tiles
l <- st_intersects(shore, index) %>%
     unlist() %>%
     unique() %>%
     sort()

# Vector of urls
dataURL <- index$Cover[l]

# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Load cover BDTQ data for Study area
# -----------------------------------
#
#
# Download BDTQ cover data and select water only
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Name of file to download
output <- './Data/BDTQ/Cover/BDTQ_cover.zip'

for(i in dataURL) {
  # Download file
   download.file(i, destfile = output)

  # Unzip file
  unzip(zipfile = output, exdir = './Data/BDTQ/Cover/')
}

# List of folders
f <- list.dirs('./data/bdtq/cover', recursive = FALSE)

# Data targetted for each tile
dat <- '/hydro_s/aat.adf'

# Load shapefiles
cover <- list()
for(i in 1:length(f)) {
  uid <- paste0(f[i], dat)
  cover[[i]] <- st_read(uid, 'PAL', options = "ENCODING=UTF-8")
}

# Weird thing with encoding, need to figure this out later
# For now, it'll be ugly to identify the Saguenay River
sag <- character()
for(i in 1:length(cover)) sag <- c(sag, cover[[i]]$TOPONYME)
sag <- sort(unique(sag))
sag <- sag[stringr::str_detect(sag, 'Saguenay')]
sag <- sag[!stringr::str_detect(sag, 'Petit')]

# Select water only
topo <- c(sag,"Fleuve Saint-Laurent","Golfe du Saint-Laurent")
for(i in 1:length(cover)) {
  uid <- cover[[i]]$TOPONYME %in% topo
  cover[[i]] <- cover[[i]][uid,]
}

# Single polygon
cover <- bind_rows(cover) %>%
         summarise(Area = sum(AREA)) %>%
         mutate(Zone = 'Zone_Etude_TC_CumulEffets')

# Transforme projection
cover <- st_transform(cover, 4326)



# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Manage holes in the data
# ------------------------
#
#
# Now for the not so reproducible portion of the script
# I need to make sure that the polygon is continuous across the study area
# I will use `mapedit` to do this
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# library(mapedit)
# gaps <- mapview(cover) %>% editMap()
# gaps <- bind_rows(gaps)
# st_write(gaps, dsn = 'Data/BDTQ/Gaps/Gaps.shp')
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #



# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Continuous study area polygon
# -----------------------------
#
#
# Dissolve everything together now
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
gaps <- st_read('./Data/BDTQ/Gaps/Gaps.shp') %>%
        st_buffer(0) %>%
        mutate(Zone = 'Zone_Etude_TC_CumulEffets')

# Bind and union
cover <- rbind(cover[,'Zone'], gaps[,'Zone']) %>%
         st_union()
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #


# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Crop one last time
# ------------------
#
#
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
cover <- st_intersection(cover, bb)
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #

# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Transform projection
# --------------------
#
# Use Québec Lambert EPSG:32198
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
cover <- st_transform(cover, crs = 32198)
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #


# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Export and zip
# --------------
#
#
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
# Export shapefile
st_write(cover, dsn = './Data/StudyArea/StudyArea.geojson', append = FALSE)
# =~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~=~-~= #
