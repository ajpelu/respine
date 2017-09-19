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



plot(mapa_riqueza)


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





