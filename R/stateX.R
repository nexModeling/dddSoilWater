#' Processing the soil water content
#'
#' The function \code{stateX()} processes the soil water content both for soils and bogs.
#' The process follows the expression dZ/dt= G(t) - X(t)- Ea(t) in Skaugen, Peerebom and Nilsson (2015).
#' There are hence three steps:
#'  i- it processes the actual evapotranspiration Ea(t)
#'  ii- it processes the soil moisture G(t)
#'  iii- it processes the excess water X(t)
#' @param eatemp evapotranspiration
#' @param cea Degree day factor for evpotranspiration(mm/degree/day)
#' @param M Groundwater Storage Capacity (GSC)
#' @param D the potential volumee of water that is needed for complete saturation at time t
#' @param G Volume of received moisture input of rain and snowmelt a time t
#' @param middelsca average snow coverage over the level zones
#' @param R Ratio defining field capacity (fracion of D)
#' @param Z Actual water volume present in the soil moisture zone
#' @param Gbog Volume of received moisture input of rain and snowmelt a time t on Bogs
#' @param Zbog Actual water volume present on bogs
#' @keywords soilWater
#' @export
#' @examples
#' \dontrun{
#' stateX()
#' }
stateX <-function(eatemp,cea,M,D,G,middelsca,R,Z,Gbog,Zbog){

  # UPDATE SOIL WATER CONTENT
  # Actual evapotranspiration
  Ea <- ea(soilType="soil",eatemp=eatemp,cea=cea,M=M,D=D,G=G,middelsca=middelsca)
  # Update soil moisture G(t) because of evapotranspiration
  G <- G-Ea
  if (G < 0) G <- 0
  # Excess water
  tmp <- excessWater(G=G,Z=Z,D=D,R=R)
  X  = tmp$X
  G  = tmp$G


  # UPDATE BOG WATER CONTENT
  # Actual evapotranspiration
  Eabog <- ea(soilType="bog",eatemp=eatemp,cea=cea,M=M,D=D,G=Gbog,middelsca=middelsca)
  # Update soil moisture G(t) because of evapotranspiration
  Gbog <- Gbog-Eabog
  if (Gbog < 0) G <- 0
  # Excess water
  tmp <- excessWater(G=Gbog,Z=Zbog,D=1,R=M)
  Xbog  = tmp$X
  Gbog  = tmp$G


  res <- list(Ea           = Ea,
              G            = G,
              X            = X,
              Eabog        = Eabog,
              Gbog         = Gbog,
              Xbog         = Xbog)

  return(res)
}
