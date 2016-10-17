#' Actual evapotranspiration
#'
#' The function \code{ea()} processes the actual evapotranspiration.
#' It follows the expression of Skaugen,Peerebom and Nilsson(2015):
#' Ea = Ep*(S+Z)/M, with Ep = cea*T, the potential evapotranspiration
#' @param soilType type of soil (soil or bog)
#' @param eatemp evapotransiration
#' @param cea degree-day factor, positive (resp neg) for positive (resp neg) temperature
#' @param M groundwater storage capacity
#' @param D the potential volumee of water that is needed for complete saturation at time t
#' @param G Volume of received moisture input of rain and snowmelt a time t
#' @param middelsca average snow coverage over the level zones
#' @keywords soilWater
#' @export
#' @examples
#' \dontrun{
#' ea()
#' }
ea <-function(soilType,eatemp,cea,M,D,G,middelsca){

  ea.soil <- function(eatemp,cea,M,D,G,middelsca){
    if(eatemp>0){
      Ea <- cea*eatemp*((M-D+G)/M)*(1-middelsca)
    } else Ea <-0
    return(Ea)
  }

  ea.bog <- function(eatemp,cea,M,D,G,middelsca){
    if(eatemp>0){
      Ea <- cea*eatemp*((G)/M)*(1-middelsca)
    } else Ea <-0
    return(Ea)
  }

  Ea <- switch(soilType,
    "soil"    = ea.soil(eatemp=eatemp,cea=cea,M=M,D=D,G=G,middelsca=middelsca),
    "bog"     = ea.bog(eatemp=eatemp,cea=cea,M=M,D=D,G=G,middelsca=middelsca),
    (message=paste0("Invalid soil type:", soilType,"."))
  )
  return(Ea)
}
