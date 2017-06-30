## ---- message=FALSE, eval=FALSE------------------------------------------
#  library("rgbif")
#  library("rgdal")
#  library("sp")
#  library("raster")
#  library("dplyr")
#  library("sf")
#  library("knitr")

## ---- eval=FALSE---------------------------------------------------------
#  ### uuid of Sinfonevada
#  sinfo_uuid <- 'db6cd9d7-7be5-4cd0-8b3c-fb6dd7446472'
#  
#  ### See metadata
#  sinfo_meta <- datasets(uuid = sinfo_uuid)
#  
#  # Get table of ocurrences
#  sf <- occ_data(datasetKey=sinfo_meta$data$key, limit = 8000)

## ---- eval=FALSE---------------------------------------------------------
#  # Get only the fields of interest
#  df <- sf$data %>% dplyr::select(decimalLatitude, decimalLongitude, scientificName)
#  
#  # How many species by plot
#  richness_loc <- df %>%
#    group_by(decimalLatitude, decimalLongitude) %>%
#    count() %>% tibble::rownames_to_column(var='id_plot') %>% data.frame() %>%
#    rename(rich = n)

## ---- eval=FALSE---------------------------------------------------------
#  # Prepare Ecosystems data
#  wd <- getwd()
#  
#  temporalwd <- setwd(tempdir())
#  unzip('../data/ecosistemas_sn.zip', exdir = temporalwd)
#  
#  eco <- readOGR(dsn=temporalwd, layer = 'ecosistemas', encoding="UTF-8", verbose = TRUE)
#  
#  # Transform projection
#  eco_t <- spTransform(eco, CRS("+init=epsg:4326"))

## ---- eval=FALSE---------------------------------------------------------
#  # Create an spatial point dataframe for the plots
#  richness_sp <- SpatialPointsDataFrame(richness_loc[,c("decimalLongitude", "decimalLatitude")],
#                                        richness_loc)
#  projection(richness_sp) <-  CRS("+init=epsg:4326")

## ---- eval=FALSE---------------------------------------------------------
#  # See this example
#  # https://gis.stackexchange.com/questions/226035/join-spatial-point-data-with-multiple-polygon-data-using-r
#  
#  # Convert to sf-objects
#  richness_sp.sf <- st_as_sf(richness_sp)
#  eco_t.sf <- st_as_sf(eco_t)
#  
#  # Keep all "meuse.sf", sort by row.names(meuse.sf). Default overlay is "intersects".
#  aux <- st_join(richness_sp.sf, eco_t.sf[,c('COD_ECOSIS', 'ECOSISTE_1')])
#  
#  # Convert back to Spatial*
#  richness_sp_eco <- as(aux, "Spatial")

## ---- eval=FALSE---------------------------------------------------------
#  aux <- aux %>% mutate(newECO = recode_factor(COD_ECOSIS,
#                                                      `8`="Pine plantations",
#                                                      `2`="High-mountain meadows",
#                                                      `3`="High-mountain shrubland",
#                                                      `5`="Mid-mountain shrubland",
#                                                      `1`="Pastures",
#                                                      `6`="Aquatic systems",
#                                                      `NA`="NA",
#                                                      .default = 'Natural Forests'))
#  
#  aux <- aux %>% mutate(ECOSISTE_1 = recode_factor(ECOSISTE_1,
#           `Matorral de alta monta\361a (enebrales, sabinales, piornales)` =
#             "Matorral de alta montana (enebrales, sabinales, piornales)",
#           `Matorrales de media monta\361a (retamares, tomillares, etc.)`=
#             "Matorrales de media montana (retamares, tomillares, etc.)",
#           `Pastos de media monta\361aMaSis` = "Pastos de media montana",
#           `Pinares aut\363ctonos de P. sylvestris` = "Pinares autoctonos de P. sylvestris",
#           `Pinares aut\363ctonos sobre dolom\355as` = "Pinares autoctonos sobre dolomias",
#           `Repoblaciones de con\355feras`= "Repoblaciones de coniferas",
#           `Sistemas acu\341ticos` = "Sistemas acuaticos"))

## ---- eval=FALSE---------------------------------------------------------
#  richSinfo <- aux %>%
#    group_by(ECOSISTE_1, COD_ECOSIS) %>%
#    summarise(mean = mean(rich),
#              sd = sd(rich),
#              se = sd/sqrt(length(rich)),
#              n = length(rich),
#              min = min(rich),
#              max = max(rich),
#              median = median(rich)) %>%
#      as.data.frame() %>%
#    dplyr::select(-geometry)

## ---- eval=FALSE---------------------------------------------------------
#  richSinfo_agg <- aux %>%
#    group_by(newECO) %>%
#    summarise(mean = mean(rich),
#              sd = sd(rich),
#              se = sd/sqrt(length(rich)),
#              n = length(rich),
#              min = min(rich),
#              max = max(rich),
#              median = median(rich)) %>%
#    as.data.frame() %>%
#    dplyr::select(-geometry)
#  

## ---- eval=FALSE---------------------------------------------------------
#  richPot <- data.frame(cbind(
#    eco = c('Plantations', 'Quercus ilex forests', 'Natural deciduous forests'),
#    n = c(442, 45, 26),
#    potRich = c(13.09, 14.92, 17.55),
#    lowerInterval = c(12.82, 13.72, 15.62),
#    upperInterval = c(13.34, 16.11, 19.66)))
#  

