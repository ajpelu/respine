library("rgbif")
library("rgdal")
library("sp")
library("raster")
library("dplyr")
library("sf")


## uuid of Sinfonevada
sinfo_uuid <- 'db6cd9d7-7be5-4cd0-8b3c-fb6dd7446472'

### See metadata
sinfo_meta <- datasets(uuid = sinfo_uuid)


# Get table of ocurrences
sf <- occ_data(datasetKey=sinfo_meta$data$key, limit = 8000)

df <- sf$data %>% dplyr::select(decimalLatitude, decimalLongitude, scientificName)

# How many species by plot
richness_loc <- df %>%
  group_by(decimalLatitude, decimalLongitude) %>%
  count() %>% tibble::rownames_to_column(var='id_plot') %>% data.frame() %>%
  rename(rich = n)


richness_sp <- SpatialPointsDataFrame(richness_loc[,c("decimalLongitude", "decimalLatitude")],
                                      richness_loc)
projection(richness_sp) <-  CRS("+init=epsg:4326")



# Ecosystem of Sierra Nevada
machine <- 'ajpelu'
mydsn = paste0('/Users/', machine,
               '/Dropbox/carto_public/ECOSISTEMAS_SN')
eco <- readOGR(dsn=mydsn, layer = 'ecosistemas', encoding="UTF-8", verbose = TRUE)

eco_t <- spTransform(eco, CRS("+init=epsg:4326"))



# https://gis.stackexchange.com/questions/226035/join-spatial-point-data-with-multiple-polygon-data-using-r
# Convert to sf-objects
richness_sp.sf <- st_as_sf(richness_sp)
eco_t.sf <- st_as_sf(eco_t)

# Keep all "meuse.sf", sort by row.names(meuse.sf). Default overlay is "intersects".
aux <- st_join(richness_sp.sf, eco_t.sf[,c('COD_ECOSIS', 'ECOSISTE_1')])

# Convert back to Spatial*
richness_sp_eco <- as(aux, "Spatial")



richnes_by_eco <- aux %>%
  group_by(ECOSISTE_1) %>%
  summarise(mean = mean(rich),
            sd = sd(rich),
            se = sd/sqrt(length(rich)),
            n = length(rich),
            min = min(rich),
            max = max(rich)) %>%
  as.data.frame() %>%
  dplyr::select(-geometry)


