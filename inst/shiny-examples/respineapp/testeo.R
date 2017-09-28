## Testeo y cacharreo

library(raster)
source('./inst/shiny-examples/respineapp/createLandscape.R')
source('./inst/shiny-examples/respineapp/initRichness.R')
source('./inst/shiny-examples/respineapp/dist2nf.R')
source('./inst/shiny-examples/respineapp/disper.R')
source('./inst/shiny-examples/respineapp/disper_time.R')
library(landscapeR)
library(rasterVis)
library(rgeos)

### 1 ####
set.seed(333)
# m <- matrix(nrow=53, ncol=63, byrow = T)
# r <- raster(m)
# extent(r) <- matrix(c(0, 0, 630, 530), nrow=2)
# r[] <- 0

ancho <- 63 * 2
alto <- 53 * 2
m <- matrix(nrow=alto, ncol=ancho, byrow = T)
r <- raster(m)
extent(r) <- matrix(c(0, 0, ancho, alto), nrow=2)
r[] <- 0





## inputs
size_pp <- 1000
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
nf_value <- 2

nf_edges <- rasterToPolygons(myl, fun=function(x){x==nf_value}, dissolve = TRUE)

levelplot(myl, att='landuse', scales=list(draw=FALSE),
          col.regions = colores, colorkey=FALSE, key = myKey) +
  layer(sp.polygons(nf_edges, fill=NA))


myr_range <- as.data.frame(
  cbind(value = c(0,1,2,3),
        lowRich = c(0, 12.82, mean(13.72, 15.62), 1),
        upRich = c(0, 13.34, mean(16.11, 19.66), 3)))


### Raster de distancias. Distancias desde los bordes de las manchas de Natural forest hacia el retso de celdas del espacio
dist_raster <- dist2nf(myl, nf_value = 2)

## Visualiza el resultado de las distancias
# Esto para eliminar la tabla de atributos
dist_raster <- setValues(raster(dist_raster), dist_raster[])

levelplot(dist_raster, scales=list(draw=FALSE), margin=FALSE,
          par.settings = RdBuTheme) +
  layer(sp.polygons(nf_edges, fill=NA))



# Computa riquiza inicial

mapa_riqueza <- initRichness(r = myl,
                             draster = dist_raster,
                             r_range = myr_range,
                             treedensity = den_pp,
                             pastUse = pastUse,
                             rescale = FALSE)



plot(mapa_riqueza)





###### Disperse module

v <- disper(x=myl, xr=mapa_riqueza, nf_value = 2, pp_value = 1)


# percentage disperser
per_sb <- 0.1
per_mb <- 0.3
per_ma <- 0.6

# propaguleInputs
piB <- (3.7)/10
piM <- (0.2)/10

vv <- disper_time(msb = v[['msb']],
                  mmb = v[['mmb']],
                  mma = v[['mma']],
                  x = myl, xr=mapa_riqueza, pp_value = 1,
                  per_sb = per_sb, per_mb = per_mb, per_ma = per_ma,
                  propaguleInputBird = piB,
                  propaguleInputMammal = piM,
                  time_span = 10)






x=myl
xr=mapa_riqueza
nf_value = 2
pp_value = 1



rich_nf <- calc(stack(x, xr), fun=function(x) ifelse(x[1] == nf_value, x[1]*x[2], NA))
rich_pp <- calc(stack(x, xr), fun=function(x) ifelse(x[1] == pp_value, x[1]*x[2], NA))


nf_edges <- rasterToPolygons(x, fun=function(x){x == nf_value}, dissolve = TRUE)

i <- 1


# Contribution from each Natural Forest patch
## Get boundary limits of NF, and save as shapefile
nf_edges <- rasterToPolygons(x, fun=function(x){x == nf_value}, dissolve = TRUE)

## Dissagregate nf polygons
nf_pol <- disaggregate(nf_edges)


edges <- rasterToPolygons(x, fun=function(x){x %in% c(nf_value, pp_value)}, dissolve = TRUE)


Touching_List <- gTouches(edges, byid = TRUE, returnDense=FALSE)







i <- 5
# Operations for each polygon
for (i in 1:length(nf_pol)) {

  # Distance between el NF polygon i and all cells of the raster
  d  = gDistance(nf_pol[i,], as(x,"SpatialPoints"), byid=TRUE)
  dnf_i <- x
  dnf_i[] = apply(d,1,min) # Minimun distance
  names(dnf_i) <- paste0('nf',i) # Add name of layer (nfi, i is the number)

  dnf_i10 <- calc(dnf_i, fun = function(x){x*10}) # Multiply to 10 meters
  names(dnf_i10) <- paste0('nf',i,'_meters')

  ## Richess values for each nf i
  rich_nf_i <- mask(xr, nf_pol[i,])
  rpot_i <- cellStats(rich_nf_i, mean)




  # Adjacency module
  ## Rasterize nfi
  aux_nfi <- rasterize(nf_pol[i,], x)
  aux_nfi[aux_nfi == 1] <- nf_value
  aux_nfi[is.na(aux_nfi[])] <- 0

  # Rasterize pine plantations
  aux_pine <- calc(x, fun = function(x) ifelse(x == pp_value, pp_value, 0))

  # Merge two raster
  aux <- calc(stack(aux_nfi, aux_pine), fun = function(x){x[1]+x[2]})
  aux[aux == 0] <- NA

  # Vectorize all
  # https://stackoverflow.com/questions/45338384/calculate-the-length-of-shared-boundaries-between-multiple-polygons
  aux_shape <- rasterToPolygons(aux, dissolve = TRUE)
  aux_shape_pine <- rasterToPolygons(aux, fun=function(x){x == pp_value}, dissolve = TRUE)

  # perimeter pine
  perimeter_pine <- rgeos::gLength(aux_shape_pine)

  # Which object touch to which
  touching_list <- rgeos::gTouches(aux_shape, byid = TRUE, returnDense=FALSE)


  # Loop para ver si el poligono esta compartiendo frontera
  if (is.null(touching_list$`1`)) {

    l_lines <- 0

  } else {
    # ---- Calculate perimeters of all polygons ----
    perimeters <- sp::SpatialLinesLengths(as(aux_shape, "SpatialLines"))

    from <- 1
    lines <- rgeos::gIntersection(aux_shape[from,], aux_shape[touching_list[[from]],], byid = TRUE)

    l_lines <- sp::SpatialLinesLengths(lines)

    }

  # plot(aux_shape[c(from, touching_list[[from]]),])
  # plot(lines, add = TRUE, col = 'red', lwd = 2)

# adjacency index (0 - 1)
  ai <- l_lines/perimeter_pine


  # Dispersion contribution
  ## Small bird dispersion
  sb_i <- calc(dnf_i10, fun = function(x){dlnorm(x, meanlog = log(51), sdlog = .7)})
  names(sb_i) <- paste0('sb',i)
  sb_i_pot <- sb_i * 0.5 * rpot_i # Asumimos que coge la mitad de las semillas (mejorar)
  names(sb_i_pot) <- paste0('sb',i, 'pot')

  ## Medium bird dispersion
  mb_i <- calc(dnf_i10, fun = function(x){dlnorm(x, meanlog = log(201), sdlog = .7)})
  names(mb_i) <- paste0('mb',i)
  mb_i_pot <- mb_i * 0.5 * rpot_i # Asumimos que coge la mitad de las semillas (mejorar)
  names(mb_i_pot) <- paste0('mb',i, 'pot')

  ## Mammal dispersion
  ma_i <- calc(dnf_i10, fun = function(x){
    ifelse(x <= 400, dweibull(x, shape = 1.385, scale = 137),
           dlnorm(x, meanlog = 6.621, sdlog = 0.297))})
  names(ma_i) <- paste0('ma',i)
  ma_i_pot <- ma_i * ((0.5 * rpot_i) + 1)  # Asumimos que coge la mitad de las semillas (mejorar). Ademas añadimos semillas de tierras agrícolas (maximo 3)
  names(ma_i_pot) <- paste0('ma',i, 'pot')

  rasters_i <- stack(dnf_i10,
                     sb_i, mb_i, ma_i,
                     sb_i_pot, mb_i_pot, ma_i_pot)

  nf_singles <- stack(nf_singles, rasters_i)

  sb <- stack(sb, sb_i)
  mb <- stack(mb, mb_i)
  ma <- stack(ma, ma_i)

  sbpot <- stack(sbpot, sb_i_pot)
  mbpot <- stack(mbpot, mb_i_pot)
  mapot <- stack(mapot, ma_i_pot)
}

# Compile an unique raster by disperser vector (sum all single raster )
r_sbpot <- sum(sbpot)
names(r_sbpot) <- 'r_sbpot'
r_mbpot <- sum(mbpot)
names(r_mbpot) <- 'r_mbpot'
r_mapot <- sum(mapot)
names(r_mapot) <- 'r_mapot'


# Mask by pine plantantion
## Pine plantations boundary
pp_limit <- rasterToPolygons(x, fun=function(x){x == pp_value}, dissolve = TRUE)

msb <- mask(r_sbpot, pp_limit)
names(msb) <- 'msb' # mask small bird
mmb <- mask(r_mbpot, pp_limit)
names(mmb) <- 'mmb' # mask medium bird
mma <- mask(r_mapot, pp_limit)
names(mma) <- 'mma' # mask mammal

out <- stack(nf_singles,
             r_sbpot, r_mbpot, r_mapot,
             msb, mmb, mma)

return(out)



r <- raster(nrows=10, ncols=10)
adj <- adjacency(raster=r, fromCells = c(1,30,55,72,100), toCells = c(1:ncell(r)), directions=4)


r <- raster(nrow=18, ncol=36, xmn=0)
r[150:250] <- 1
r[251:450] <- 2
b <- boundaries(r, type='inner')
plot( boundaries(r, type='outer') )
plot( boundaries(r, classes=TRUE) )

r <- raster(nrows=10, ncols=10)
adjacent(r, cells=c(1, 55), directions=8, pairs=TRUE)

a <- adjacent(r, cell = c(1,55,90), directions=4, sorted=TRUE)
a



r <- raster(ncols=12, nrows=12)
set.seed(0)
r[] <- round(runif(ncell(r))*0.7 )
rc <- clump(r)
freq(rc)
plot(rc)



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









library(ks)
library(KernSmooth)
library(raster)


set.seed(0)
GPS <- data.frame(lon=runif(100), lat=runif(100)*2)

est <- bkde2D(GPS, bandwidth=0.1, gridsize = c(400L, 400L))
names(est) <- c('x', 'y', 'z')
est.raster <- raster(est)
plot(est.raster)
??


https://stackoverflow.com/questions/35752736/creating-a-raster-file-from-a-kernel-smooth-with-differing-x-y-resolutions
https://cran.r-project.org/web/packages/splancs/splancs.pdf


# ver libro virgilio Applied Spatial Data Analysis with R kernel2d




library(dplyr)
library(ggplot2)
library(fitdistrplus)







# --------------------
# Arrendajo from Gomez 2003
dis <- c(10, 30, 50, 70, 90, 150, 250, 350, 450, 550, 700, 900)
per <- c(1.07, 1.40, 2.34, 3.78, 9.82, 33.01, 16.11, 11.40, 10.06, 10.33, 4.89, 0.77)
df <- data.frame(cbind(dis, per))

df <- df %>%
  mutate(k = -0.1 + 0.02*log(dis) + 0.04*(log(dis^2)),
         u = 1/k)

df %>% ggplot(aes(x=dis, y=u)) + geom_point() + geom_smooth()

# Fit a lognormal and weibull
arre_lnorm <- fitdist(df$per, "lnorm")
arre_w <-  fitdist(df$per, "weibull")
# --------------------



grid <- seq(0,1500,1)
# Arrendajo (Pons & Pausas 2007)
plot(dlnorm(grid,meanlog = 3.844, sdlog = 0.851), col='blue',
     type="l")

# kernel para mamíferos González-Varo et al. 2012 http://onlinelibrary.wiley.com/doi/10.1111/1365-2656.12024/full
lines(grid,
      ifelse(grid <= 400,
             dweibull(grid, shape = 1.385, scale = 137),
             dlnorm(grid, meanlog = 6.621, sdlog = 0.297)),
      type="l",xlab="x",ylab="f(x)", col='red')


lines(grid,
      dlnorm(grid, meanlog = log(200),
             sdlog= arre_lnorm$estimate["sdlog"]), col='blue',
      lty=2)

lines(grid,
      dweibull(grid,
               shape = arre_w$estimate["shape"],
               scale = 88),
      col='blue', lty=6)


lines(grid,
      dlnorm(grid,meanlog = arre_lnorm$estimate["meanlog"],
             sdlog= arre_lnorm$estimate["sdlog"]), col='blue',
      lty=2)





dis <- data.frame(d = seq(1,1000, b=1))

dis <- dis %>%
  mutate(k = -0.1 + 0.02*d + 0.04*(d^2),
         u = 1/k,
         u2 = 1/(-0.1 - 0.02*(log(d)) + 0.04*((log(d))^2)),
         gu = 1/(0.14 - 2.17*(log(d)) - 2.50*((log(d))^2) + 0.7*((log(d))^3)))

dis %>% filter(u > 0) %>%
  ggplot(aes(x=d, y=gu)) + geom_line()




#### Proposal

data <- data.frame(
  dis = seq(0,1500,1))

data <- data %>%
  mutate(
    sb = dlnorm(dis, meanlog = log(51), sdlog = .9),
    mb = dlnorm(dis, meanlog = log(151), sdlog = 1),
    ma = ifelse(dis <= 400,
                dweibull(dis, shape = 1.385, scale = 137),
                dlnorm(dis, meanlog = 6.621, sdlog = 0.297)),
    gg = dlnorm(dis, meanlog = log(450), sdlog = 1))


data %>% ggplot(aes(x=dis)) +
  geom_line(aes(x=dis, y=sb), color='blue') +
  geom_line(aes(x=dis, y=mb), color='black') +
  geom_line(aes(x=dis, y=ma), color='red') +
  geom_line(aes(x=dis, y=gg), color='green') +
  scale_x_continuous(breaks = seq(0,1500, 100)) +
  theme_bw() +
  theme(panel.grid = element_blank()) +
  ylab('f(x)') +
  annotate("text",
           x= 1000, y = 0.010,
           label=("blue = sb, red = ma, black=mb, green = gg"))




plot(columbus, border="grey")
plot(col.gal.nb, coords, add=TRUE)
plot(dxx, coords, add=TRUE, col="red")
title(main=paste("Differences (red) in Columbus GAL weights (black)",
                 "and polygon generated queen weights", sep="\n"))
xxx <- poly2nb(columbus, queen=FALSE)
dxxx <- diffnb(xxx, col.gal.nb)
plot(columbus, border = "grey")
plot(col.gal.nb, coords, add = TRUE)
plot(dxxx, coords, add = TRUE, col = "red")
title(main=paste("Differences (red) in Columbus GAL weights (black)",
                 "and polygon generated rook weights", sep="\n"))
cards <- card(xx)
maxconts <- which(cards == max(cards))
if(length(maxconts) > 1) maxconts <- maxconts[1]
fg <- rep("grey", length(cards))
fg[maxconts] <- "red"
fg[xx[[maxconts]]] <- "green"
plot(columbus, col=fg)
title(main="Region with largest number of contiguities")
example(nc.sids)
system.time(xxnb <- poly2nb(nc.sids))
plot(nc.sids)
plot(xxnb, coordinates(nc.sids), add=TRUE, col="blue")




require("rgdal")
require("rgeos")
library(sp)
library(raster)

download.file("https://www.dropbox.com/s/vbxx9dic34qwz63/Polygons.zip?dl=1", "Polygons.zip")

unzip("./Polygons.zip")
Shapefile <- readOGR(".","Polygons")

plot(edges, col=edges@data)


Touching_List <- gTouches(edges, byid = TRUE, returnDense=FALSE)

# Touching_DF <- setNames(utils::stack(lapply(Touching_List, as.character)), c("TOUCHING", "ORIGIN"))

# ---- Calculate perimeters of all polygons ----
perimeters <- sp::SpatialLinesLengths(as(edges, "SpatialLines"))

# ---- Example with the first object of the list and first neighbor ----
from <- 1
to <- 1
line <- rgeos::gIntersection(edges[from,], edges[Touching_List[[from]][to],])
l_line <- sp::SpatialLinesLengths(line)

plot(edges[c(from, Touching_List[[from]][to]),])
plot(line, add = TRUE, col = "red", lwd = 2)

# ---- Example with the first object of the list and all neighbors ----
from <- 1
lines <- rgeos::gIntersection(Shapefile[from,], Shapefile[Touching_List[[from]],], byid = TRUE)
l_lines <- sp::SpatialLinesLengths(lines)

plot(Shapefile[c(from, Touching_List[[from]]),])
plot(lines, add = TRUE, col = 1 + 1:length(Touching_List[[from]]), lwd = 2)
