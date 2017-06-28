#' Landscape function
#' @param r A raster object
#' @param size_pp The size of pine plantations expressed as number of pixels (integer)
#' @param n_nf Number of patchs of Natural forests (a number between 1 and 5)
#' @param size_nf The size of natural forest patchs (a value between 10 and 500)
#' @return A raster object with the landscape confifured
#' @examples
#' m <- matrix(nrow=100, ncol=200, byrow = TRUE)
#' r <- raster(m)
#' extent(r) <- matrix(c(0, 0, 200, 100), nrow=2)
#' r[] <- 0
#' myl <- createLandscape(r, size_pp = 1000, size_nf = 500, n_nf = 4)
#' plot(myl)


createLandscape <- function(r, size_pp, n_nf, size_nf){
  # Create pine plantation patch
  pp <- makeClass(r, val=1, npatch = 1, rast=TRUE,
                  size = size_pp,
                  pts = matrix(c(100,50), nrow=1, ncol=2))

  # Create natural forest patchs
  nf <- makeClass(pp, val=2, rast=TRUE,
                  npatch=n_nf,
                  size=size_nf)

  # Create Crops patchs
  ### 7.5 % n cells backgroud availables
  size_c <- ceiling(ncell(nf[nf==0])*0.075)
  n_crops <- sample(3:8, size=1)

  l <- makeClass(nf, val=3,
                 npatch = n_crops,
                 size= sample(10:size_c, size = n_crops),
                 rast=TRUE)

  # Add factor uses
  l <- ratify(l)
  rat <-levels(l)[[1]]
  rat$landuseValue <- levels(l)[[1]]$ID
  rat$landuse <- c('Other', 'Pine plantation', 'Natural Forest', 'Crops')
  levels(l) <- rat

  return(l)

}
