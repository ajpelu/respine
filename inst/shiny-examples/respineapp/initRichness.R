#' Set values of Initial Richness
#'
#' Esta funcion establece los valores iniciales de riqueza de especies
#'
#' @param r A \code{raster} object
#'
#' @param draster A \code{raster} object with distance to nearest oak TODO (COMPLETAR)
#'
#' @param r_range A \code{data frame} with three columns: \code{value} of land use
#' (\code{integer}: 0 = "Other", 1 = "Pine plantation", 2 = "Natural Forests",
#' 3 = "Crop"); \code{lowRich} and \code{upRich} (lower an upper value of the
#' range of Richness: See Gomez-Aparicio et al 2009)
#'
#' @param treedensity density of the pine plantation (\code{integer})
#'
#' @param pastUse the past land use of the pine plantation (\code{character}).
#' One of "Oak", "Shrubland", "Pasture" or "Crop"
#'
#' @param rescale If "TRUE" the results are rescaled
#'
#' @return A \code{raster} object with values of initial Richness for each
#' pixel.
#'
#' @references
#'
#' Gomez-Aparicio L, Zavala MA, Bonet FJ, Zamora R (2009) Are pine plantations
#' valid tools for restoring Mediterranean forests? An assessment along abiotic
#' and biotic gradients. Ecological Applications, 19: 2124 - 2141.
#'
#'

initRichness <- function(r, draster, r_range, treedensity, pastUse, rescale=TRUE){

  # Natural forest (2)
  aux <- r_range[which(r_range$value == 2), ]
  r[r == 2]  <- sample(aux$lowRich:aux$upRich, ncell(r[r==2]), replace = TRUE)

  # Crops (3)
  aux <- r_range[which(r_range$value == 3), ]
  r[r == 3]  <- sample(aux$lowRich:aux$upRich, ncell(r[r==3]), replace = TRUE)

  # Pine plantations (1)
  # R ~ potR * (0.1*(pastUSE) + 0.7*(treeDensity))
  ## ojo como asignamos los pesos??
  # f <- (.1*fplu + .3*fdis + .6*ftreeden)


  ## ~ TreeDensity
  ### Fraction of Potential Richness (tree Density Eq. 3 Gomez Aparicio et al. 2009)
  ftreeden <- exp(-0.5*((treedensity - 0.22)/1504.1)^2)

  ## ~ PastUSE
  ### Past Land Use
  fplu <- ifelse(pastUse == 'Oak', .9999,
                 ifelse(pastUse == 'Shrubland', .4982,
                        ifelse(pastUse == 'Crop', .0279, .0001)))

  ## ~ Distance to Seed Source

  ## Compute diversity raster (See Gonzalez-Moreno et al. 2011)
  sh <- calc(draster, fun=function(x){1.7605 - 0.0932*(sqrt(sqrt(x)))})

  # Create a stack with the shanon diversity raster and landuse raster, and then compute values for pine plantations
  s <- calc(stack(r, sh), fun=function(x)  ifelse(x[1] == 1 , x[1]*x[2],  NA))

  # Scale the distance effect from 0 to 1
  sh_scaled <- (s - cellStats(s, "min"))/(cellStats(s, "max") - cellStats(s, "min"))

  ## Combine factor to correct pine plantations (OJO!!!!!!!!!!!!!! PESOS )
  f_pine <- (sh_scaled*0.3) + (.6*ftreeden + .1*fplu)

  aux <- r_range[which(r_range$value == 1), ]
  r[r == 1]  <- sample(aux$lowRich:aux$upRich, ncell(r[r==1]), replace = TRUE)

  aux <- r_range[which(r_range$value == 3), ]
  r[r == 3]  <- sample(aux$lowRich:aux$upRich, ncell(r[r==3]), replace = TRUE)

  r <- calc(stack(r, f_pine),
            fun = function(x) ifelse(x[1] == 1, x[1]*x[2], x[1]))

  # Rescale results
  if (rescale)
    r <- (r - cellStats(r, "min"))/(cellStats(r, "max") - cellStats(r, "min"))

  return(r)

}
