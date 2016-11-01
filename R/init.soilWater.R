#' Initialize information related to Soil Moisture
#'
#' Initialize the information related to ssoil Moisture
#' @param method method for the initialization, "load", "source", "manual", "procecessed"
#' @param path directory where to get the files
#' @param Ea actual evapotranspiration
#' @param G Volume of received moisture input of rain and snowmelt
#' @param X excess water
#' @param Eabog actual evapotranspiration on bog
#' @param Gbog Volume of received moisture input of rain and snowmelt a time t on Bogs
#' @param Xbog excess water on bog
#' @param eatemp evapotranspiration
#' @param cea Degree day factor for evpotranspiration(mm/degree/day)
#' @param M Groundwater Storage Capacity (GSC)
#' @param D the potential volumee of water that is needed for complete saturation
#' @param middelsca average snow coverage over the level zones
#' @param R Ratio defining field capacity (fracion of D)
#' @param Z Actual water volume present in the soil moisture zone
#' @param Zbog Actual water volume present on bogs#' @keywords soilWater
#' @export
#' @examples
#' \dontrun{
#' init.soilWater()
#' }
init.soilWater <-function(method=NULL,path=NULL,Ea,G,X,Eabog,Gbog,Xbog,
                eatemp,cea,M,D,middelsca,R,Z,Zbog){

  soilWater <- switch(method,
    "manual"    = init.manual(Ea=Ea,G=G,X=X,Eabog=Eabog,Gbog=Gbog,Xbog=Xbog),
    "processed" = init.parocessed(eatemp=eatemp,cea=,M=M,D=D,G=G,middelsca=middelsca,R=R,Z=Z,Gbog=Gbog,Zbog=Zbog),
    "load"      = init.load(path=path),
    (message=paste0("Invalid method:", method,".")))

  return(soilWater)
}

init.manual <- function(Ea,G,X,Eabog,Gbog,Xbog){

  res <- list(Ea=Ea,
                 G=G,
                 X=X,
                 Eabog=Eabog,
                 Gbog=Gbog,
                 Xbog=Xbog)
  return(res)

}

init.load <- function(path){
  load(paste0(path,"soilWater.rda"))
  return(soilWater)
}

init.processed <-function(eatemp,cea,M,D,G,middelsca,R,Z,Gbog,Zbog){
  if ( (!is.null(eatemp)) && (!is.null(cea)) && (!is.null(M)) &&
       (!is.null(D)) && (!is.null(G)) && (!is.null(middelsca)) &&
       (!is.null(R)) && (!is.null(Z)) && (!is.null(Gbog)) && (!is.null(Zbog)) ) {
   res <- stateX.soilWater(eatemp=eatemp,cea=,M=M,D=D,G=G,middelsca=middelsca,R=R,Z=Z,Gbog=Gbog,Zbog=Zbog)
   return(res)
   } else stop("NULL arguments in parameters")
}
