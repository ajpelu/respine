#' Potential Richness of Sierra Nevada forests
#'
#' A dataset containing the potential richness of Pine plantantios, natural
#' \emph{Quercus ilex} forests and deciduous forests. The potential richness
#' computed using the equations provided by Gomez-Aparicio et al. (2009). See
#' Equation 1 and Appendix A2 of this paper.
#'
#' Gomez-Aparicio L, Zavala MA, Bonet FJ, Zamora R (2009) Are pine plantations
#' valid tools for restoring Mediterranean forests? An assessment along abiotic
#' and biotic gradients. Ecological Applications, 19: 2124 - 2141.
#'
#' @format A \code{data frame} with three rows and 5 columns:
#' \describe{
#'  \item{eco}{Ecosystem type}
#'  \item{n}{number of plots used to estimate the potential richness. See methods
#'    section in Gomez-Aparicio et al. (2009)}
#'  \item{potRich}{predicted values of Richness computed using Eq. 1 of
#'    Gomez-Aparicio et al. (2009)}
#'  \item{lowerInterval}{lowere value of the Support interval}
#'  \item{upperInterval}{lowere value of the Support interval}
#' }
#'
#' @references \url{https://dx.doi.org/10.1890/08-1656.1}
"richPot"

#' Richness of Sierra Nevada (Spain) forests from SINFONEVADA aggregated
#'
#' A dataset containing the richness of Sierra Nevada ecosystems according to
#' Sinfonevada (Perez-Luque et al. 2014). The richness value of each plot
#' was used to compute mean values by ecosystem types using a spatial overlay of
#' the plots and the ecosystems map of Sierra Nevada. Then the ecoystem were
#' reclassify.
#'
#' Perez-Luque A, Bonet F, Perez-Perez R, Aspizua R, Lorite J, Zamora R (2014)
#' Sinfonevada: Dataset of Floristic diversity in Sierra Nevada
#' forests (SE Spain). PhytoKeys 35: 1-15.
#'
#' @format A \code{data frame} with 11 rows and 8 columns:
#' \describe{
#'  \item{newECO}{name of the ecosystem type}
#'  \item{mean}{richnnes mean value of the plots within this ecosystem type}
#'  \item{sd}{standard deviation of the richness mean value of the plots within
#'   this ecosystem type}
#'  \item{se}{standard error of the richness mean value of the plots within
#'   this ecosystem type}
#'  \item{n}{number of plots within this ecosystem type}
#'  \item{min}{minimun value of richnnes of the plots within this ecosystem type}
#'  \item{max}{maximum value of richnnes of the plots within this ecosystem type}
#'  \item{median}{richnnes median value of the plots within this ecosystem type}
#' }
#'
#' @references \url{https://doi.org/10.3897/phytokeys.35.6363}
"richSinfo_agg"

#' Richness of Sierra Nevada (Spain) forests from SINFONEVADA
#'
#' A dataset containing the richness of Sierra Nevada ecosystems according to
#' Sinfonevada (Perez-Luque et al. 2014). The richness value of each plot
#' was used to compute mean values by ecosystem types using a spatial overlay of
#' the plots and the ecosystems map of Sierra Nevada.
#'
#' Perez-Luque A, Bonet F, Perez-Perez R, Aspizua R, Lorite J, Zamora R (2014)
#' Sinfonevada: Dataset of Floristic diversity in Sierra Nevada
#' forests (SE Spain). PhytoKeys 35: 1-15.
#'
#' @format A \code{data frame} with 11 rows and 9 columns:
#' \describe{
#'  \item{ECOSISTE_1}{name of the ecosystem type}
#'  \item{COD_ECOSIS}{code of the ecosystem type}
#'  \item{mean}{richnnes mean value of the plots within this ecosystem type}
#'  \item{sd}{standard deviation of the richness mean value of the plots within
#'   this ecosystem type}
#'  \item{se}{standard error of the richness mean value of the plots within
#'   this ecosystem type}
#'  \item{n}{number of plots within this ecosystem type}
#'  \item{min}{minimun value of richnnes of the plots within this ecosystem type}
#'  \item{max}{maximum value of richnnes of the plots within this ecosystem type}
#'  \item{median}{richnnes median value of the plots within this ecosystem type}
#' }
#'
#' @references \url{https://doi.org/10.3897/phytokeys.35.6363}
"richSinfo"

#' Relation between Shannon Diversity index and Seed Source distance to oak
#' vegetation
#'
#' A dataset containing the Shannon diversity of pine plantantions and the
#' distance to nearest Oak formation. This dataset come from by Gonzalez-Moreno (2009),
#' from the digitalization of Figure 3.2.B. It was used by Gonzalez-Moreno (2009)
#' and by Gonzalez-Moreno et al. 2011 to assess the effect of distance in the
#' Shannon diversity observed in pine plantations (proximity to seed source).
#' A dataset containing the richness of Sierra Nevada ecosystems according to
#'
#' The equation obtained fitting a linear model between distance and shannon
#' diversity will be used to determine the initial richnnes of each pixel in the
#' pine plantations.
#'
#' González-Moreno P (2009) Natural regeneration and biodiversity in pine
#' plantations of Southern Spain: a landscape approach. Master’s Thesis,
#' Wageningen University, Netherlands
#'
#' Gonzalez-Moreno P, Quero JL, Bonet FJ, Poorter L, Zamora R. (2011) Is spatial
#' structure the key to promote plant diversity in Mediterranean forest
#' plantations?. Basic and Applied Ecology 12 (3): 251-259
#'
#'
#' @format A \code{data frame} with 3 columns:
#' \describe{
#'  \item{wdhOak}{Weighted distance downhill to seed source. See references.
#'   Distance is double square root transformed}
#'  \item{SH}{Shannon diversity index }
#'  \item{dist}{Distance to oak formation (in meters)}
#' }
#'
#' @references \url{https://doi.org/10.1016/j.baae.2011.02.012}
"diversity_distance"
