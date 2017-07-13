library(raster)
source('./R/createLandscape.R')
source('./R/initRichness.R')
source('./R/dist2nf.R')
library(landscapeR)
library(rasterVis)
library(rgeos)

### 1 ####
set.seed(123)
m <- matrix(nrow=100, ncol=200, byrow = T)
r <- raster(m)
extent(r) <- matrix(c(0, 0, 200, 100), nrow=2)
r[] <- 0


## inputs
size_pp <- 1500
den_pp <- 900
size_nf <- 500
n_nf <- 5
pastUse <- 'Oak' # TODO switch past use

# Landscape parameteres
#size_pp <- input$size_pp  # size pine plantations
# den_pp <- input$density_pp # density pine plantations
# size_nf <- input$size_nf # size natural forests
# n_nf <- input$n_nf  # n patchs natural forests

### 2 ### CREA LANDSCAPE
myl <- createLandscape(r, size_pp = size_pp, size_nf = size_nf, n_nf = n_nf)


### 3 ### PLOT LANDSCAPE CREADO

colour_tree_density <- '#238b45'
colores <- c('lightgoldenrod1', # Crops
             'green', # Natural forests
             'gray99', # Other
             colour_tree_density) # Pine plantation

## Legend
myKey <- list(text = list(lab = c("crop", "natural forest","other", "pine")),
              rectangles=list(col = colores),
              space='bottom', columns=4)

levelplot(myl, att='landuse', scales=list(draw=FALSE),
          col.regions = colores, colorkey=FALSE, key = myKey)


### 4 ### VALORES RIQUEZA INICIALES

borde <- rasterToPolygons(myl, fun=function(x){x==2}, dissolve = TRUE)

levelplot(myl, att='landuse', scales=list(draw=FALSE),
          col.regions = colores, colorkey=FALSE, key = myKey) +
  layer(sp.polygons(borde, fill=NA))


myr_range <- as.data.frame(
  cbind(value = c(0,1,2,3),
        lowRich = c(0, 12.82, mean(13.72, 15.62), 5),
        upRich = c(0, 13.34, mean(16.11, 19.66), 7)))


dist_raster <- dist2nf(myl, nf_value = 2)

mapa_riqueza <- initRichness(r = myl,
                             draster = dist_raster,
                             r_range = myr_range,
                             treedensity = den_pp,
                             pastUse = pastUse,
                             rescale = TRUE)






r <- myl
treedensity <- den_pp






## ~ TreeDensity
### Fraction of Potential Richness (tree Density Eq. 3 Gomez Aparicio et al. 2009)
treedensity <- den_pp
ftreeden <- exp(-0.5*((treedensity - 0.22)/1504.1)^2)

## ~ PastUSE
### Past Land Use
fplu <- ifelse(pastUse == 'Oak', .9999,
               ifelse(pastUse == 'Shrubland', .4982,
                      ifelse(pastUse == 'Crop', .0279, .0001)))

# Get contribution factor of tree Density and Past Land Use
f2 <- (.6*ftreeden + .1*fplu)

sh <- calc(dist_raster, fun=function(x){1.7605 - 0.0932*(sqrt(sqrt(x)))})

# Create a stack with the shanon diversity raster and landuse raster, and then compute values for pine plantations
s <- calc(stack(myl, sh), fun=function(x)  ifelse(x[1] == 1 , x[1]*x[2],  NA))


sh_scaled <- (s - cellStats(s, "min"))/(cellStats(s, "max") - cellStats(s, "min"))



r_range <- myr_range



ola <- initRichness(r, r_range, treedensity, pastUse, rescale=TRUE)





