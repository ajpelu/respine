## Testeo y cacharreo

library(raster)
source('./inst/shiny-examples/respineapp/createLandscape.R')
source('./inst/shiny-examples/respineapp/initRichness.R')
source('./inst/shiny-examples/respineapp/dist2nf.R')
source('./inst/shiny-examples/respineapp/disper.R')
source('./inst/shiny-examples/respineapp/disper_time.R')
source('./inst/shiny-examples/respineapp/adjfactor.R')
library(landscapeR)
library(rasterVis)
library(rgeos)

### 1 ####
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
# nf_value <- 2
#
pp_value <- 1
pine_limit <- rasterToPolygons(myl, fun=function(x){x==pp_value}, dissolve = TRUE)



levelplot(myl, att='landuse', scales=list(draw=FALSE),
          col.regions = colores, colorkey=FALSE, key = myKey) +
  layer(sp.polygons(pine_limit, fill=NA))


myr_range <- as.data.frame(
  cbind(value = c(0,1,2,3),
        lowRich = c(0, 12.82, mean(13.72, 15.62), 1),
        upRich = c(0, 13.34, mean(16.11, 19.66), 2)))


### Raster de distancias. Distancias desde los bordes de las manchas de Natural forest hacia el retso de celdas del espacio
dist_raster <- dist2nf(myl, nf_value = 2)

## Visualiza el resultado de las distancias
# Esto para eliminar la tabla de atributos
# dist_raster <- setValues(raster(dist_raster), dist_raster[])
#
# levelplot(dist_raster, scales=list(draw=FALSE), margin=FALSE,
#           par.settings = RdBuTheme) +
#   layer(sp.polygons(nf_edges, fill=NA))



# Computa riquiza inicial

mapa_riqueza <- initRichness(r = myl,
                             draster = dist_raster,
                             r_range = myr_range,
                             treedensity = den_pp,
                             pastUse = pastUse,
                             rescale = FALSE)










# nf_ri <- calc(stack(myl, mapa_riqueza), function())


# rich_pp <- reactive({
#   calc(stack(landscapeInit(), rasterRich()), fun=function(x) ifelse(x[1] == pp_value, x[1]*x[2], NA))
# })

nf_value <- 2

rich_nf <- calc(stack(myl, r), fun=function(x) ifelse(x[1] == nf_value, (x[1]/nf_value)*x[2], NA))
plot(rich_nf)
cellStats(rich_nf, min)
cellStats(rich_nf, mean)
cellStats(rich_nf, max)

crop_value <- 3

rich_crop <- calc(stack(myl, r), fun=function(x) ifelse(x[1] == crop_value, (x[1]/crop_value)*x[2], NA))
plot(rich_crop)
cellStats(rich_crop, min)
cellStats(rich_crop, mean)
cellStats(rich_crop, max)

pp_value <- 1

rich_pp <- calc(stack(myl, r), fun=function(x) ifelse(x[1] == pp_value, (x[1]/crop_value)*x[2], NA))
plot(rich_pp)
cellStats(rich_pp, min)
cellStats(rich_pp, mean)
cellStats(rich_pp, max)

rich_nf <- reactive({
  rich_nf <- calc(stack(myl, r), fun=function(x) ifelse(x[1] == nf_value, (x[1]/nf_value)*x[2], NA))
  list(
    min = cellStats(rich_nf, min),
    mean = cellStats(rich_nf, mean),
    max = cellStats(rich_nf, max)
  )
  })








# Natural forest (2)
aux <- r_range[which(r_range$value == 2), ]
r[r == 2]  <- sample(aux$lowRich:aux$upRich, ncell(r[r==2]), replace = TRUE)


plot(r)

r[r == 0] <- NA
r[r == 1] <- -100
r[r == 2] <- -200
r[r == 3] <- -300

aux <- r_range[which(r_range$value == 2), ]
r[r == -200]  <- sample(aux$lowRich:aux$upRich, ncell(r[r==-200]), replace = TRUE)

aux <- r_range[which(r_range$value == 3), ]
r[r == -300]  <- sample(aux$lowRich:aux$upRich, ncell(r[r==-300]), replace = TRUE)

ftreeden <- exp(-0.5*((treedensity - 0.22)/1504.1)^2)

sh <- calc(draster, fun=function(x){1.7605 - 0.0932*(sqrt(sqrt(x)))})

# Create a stack with the shanon diversity raster and landuse raster, and then compute values for pine plantations
s <- calc(stack(r, sh), fun=function(x)  ifelse(x[1] == -100 , (x[1]/-100)*x[2], NA))

# Scale the distance effect from 0 to 1
sh_scaled <- (s - cellStats(s, "min"))/(cellStats(s, "max") - cellStats(s, "min"))

## ~ PastUSE
### Past Land Use
fplu <- ifelse(pastUse == 'Oak', .9999,
               ifelse(pastUse == 'Shrubland', .4982,
                      ifelse(pastUse == 'Crop', .0279, .0001)))

###### ---------------------------------------

## Combine factor to correct pine plantations (OJO!!!!!!!!!!!!!! PESOS)
f_pine <- (sh_scaled*0.35) + (.45*ftreeden + .2*fplu)

aux <- r_range[which(r_range$value == 1), ]
r[r == -100]  <- sample(aux$lowRich:aux$upRich, ncell(r[r==-100]), replace = TRUE)

r <- calc(stack(r, f_pine),
          fun = function(x) ifelse(x[1] == 1, x[1]*x[2], x[1]))



plot(r)
plot(sh)
plot(s)




r
















# nf_ri <- calc(stack(myl, mapa_riqueza), function())


# rich_pp <- reactive({
#   calc(stack(landscapeInit(), rasterRich()), fun=function(x) ifelse(x[1] == pp_value, x[1]*x[2], NA))
# })

nf_value <- 2

rich_nf <- calc(stack(myl, mapa_riqueza), fun=function(x) ifelse(x[1] == nf_value, (x[1]/nf_value)*x[2], NA))
plot(rich_nf)
cellStats(rich_nf, mean)

crop_value <- 3

rich_crop <- calc(stack(myl, mapa_riqueza), fun=function(x) ifelse(x[1] == crop_value, (x[1]/crop_value)*x[2], NA))
plot(rich_crop)
cellStats(rich_crop, min)














###### Disperse module
v <- disper(x=myl, xr=mapa_riqueza, nf_value = 2, pp_value = 1)




# levelplot(dist_raster, scales=list(draw=FALSE), margin=FALSE,
#           par.settings = RdBuTheme)



levelplot(stack(v[['msb']],
                v[['mmb']],
                v[['mma']]),
          margin=FALSE,  par.settings = RdBuTheme)





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



# crs(vv) <- "+proj=longlat +datum=WGS84 +no_defs +ellps=WGS84 +towgs84=0,0,0"
#writeRaster(vv, '/Users/ajpelu/Desktop/stackTEST/out.tif', bylayer=TRUE)

msb = v[['msb']]
mmb = v[['mmb']]
mma = v[['mma']]
x = myl
xr=mapa_riqueza
pp_value = 1
propaguleInputBird = piB
propaguleInputMammal = piM
time_span = 10





levelplot(stack(vv),
          margin=FALSE,  par.settings = RdBuTheme)






x=myl
xr=mapa_riqueza
nf_value = 2
pp_value = 1







# Cosas que organizar para el adjacency module
# https://stackoverflow.com/questions/45338384/calculate-the-length-of-shared-boundaries-between-multiple-polygons
  ad <- c(0.606, 10.2, 24.2, 33.4, 21.6, 7.53, 39.0, 44.1, 99.5)
  seedlim <- c(0.797, 0.800, 0.789, 0.666, 0.666, 0.526, 0.468, 0.395, 0.392)
  df_ad <- as.data.frame(cbind(ad, seedlim))
  model_ad <- lm(seedlim~ad, data=df_ad)


  adj <- c(0,25,50,75,100)


  adj <- as.data.frame(adj) %>%
    mutate(sl = 0.736658946 -0.004037077 * adj,
           sli = 1/sl,
           zadj = (sl - min(sl)) / (max(sl) - min(sl)),
           zadji = (sli - min(sli)) / (max(sli) - min(sli)),
           zadji1 = zadji + 1)











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






sh <- st_as_sf(aux_shape)
Touching_List <- st_touches(sh)
line <- st_intersection(sh[1,], sh[Touching_List[[1]][1],])
l_line <- st_length(line)


sh


Shapefile.sf <- st_as_sf(aux_shape)


# ---- Touching list ----cl
Touching_List <- st_touches(Shapefile.sf)
# ---- Polygons perimeters ----
perimeters <- st_length(Shapefile.sf)


from <- 1
lines <- st_intersection(Shapefile.sf[from,], Shapefile.sf[Touching_List[[from]],])
lines <- st_cast(lines) # In case of multiple geometries (ex. from=71)
l_lines <- st_length(lines)






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
