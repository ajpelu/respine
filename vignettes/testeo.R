library(raster)
source('./R/createLandscape.R')
source('./R/initRichness.R')
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

borde <- rasterToPolygons(myl, fun=function(x){x==2},
                             dissolve = TRUE)

levelplot(myl, att='landuse', scales=list(draw=FALSE),
          col.regions = colores, colorkey=FALSE, key = myKey) +
  layer(sp.polygons(borde, fill=NA))


myr_range <- as.data.frame(
  cbind(value = c(0,1,2,3),
        lowRich = c(0, 12.82, mean(13.72, 15.62), 5),
        upRich = c(0, 13.34, mean(16.11, 19.66), 7)))


mapa_riqueza <- initRichness(r = myl,
                             r_range = myr_range,
                             treedensity = den_pp,
                             pastUse = pastUse)

initRichness <- function(r, r_range, treedensity, pastUse, rescale=TRUE)




  # !!! por aqui vas



dist_raster <- dist2nf(myl, nf_value = 2)

sh <- calc(dist_raster, fun=function(x){1.7605 - 0.0932*(sqrt(sqrt(x)))})


# myl[myl==1] <- 99
# pine_mask <- calc(myl, fun=function(x){ x[x < 98] <- NA; return(x)})
# myl[myl==99] <- 1
# plot(pine_mask)

ifelse
mystack <- stack(myl, sh)

i <- calc(mystack, fun=function(x)  ifelse(x[1] == 1 , x[1]*x[2],  x[1]))




MyFun <- function(x, y) abs(x) * y
res <- overlay(x, y, fun = MyFun)

ok <- overlay(x=myl, y=sh, fun = function(x, y) {ifelse(x == 1, x*y, x)})



s <- calc(r, fun=function(x){ x[x < 4] <- NA; return(x)} )
shm <- mask
