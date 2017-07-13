set.seed(123)
m <- matrix(nrow=100, ncol=200, byrow = T)
r <- raster(m)
extent(r) <- matrix(c(0, 0, 200, 100), nrow=2)
r[] <- 0


myl <- createLandscape(r, size_pp = 1500, size_nf = 500, n_nf = 6)

borde <- rasterToPolygons(myl, fun=function(x){x==2},
                             dissolve = TRUE)

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
