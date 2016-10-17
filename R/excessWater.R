#' Excess of water
#'
#' The function \code{excessWater()} processes the excess of water.
#' It follows the expression of Skaugen and Onof (2013) and Skaugen,Peerebom and Nilsson(2015)
#' if (G(t)+Z(t))/D(t) >R : X(t) = G(t) + Z(t) -R*D(t)
#' if (G(t)+Z(t))/D(t) <R : X(t) = 0
#' @param G Volume of received moisture input of rain and snowmelt a time t
#' @param Z Actual water volume present in the soil moisture zone
#' @param D the potential volumee of water that is needed for complete saturation at time t
#' @param R Ratio defining field capacity (fracion of D)
#' @keywords soilWater
#' @export
#' @examples
#' \dontrun{
#' excessWater()
#' }
excessWater <-function(G,Z,D,R){

  D <- max(D,0,na.rm=TRUE)
  rat <- ifelse( (D>0), (G+Z)/D, 1)
  if(rat > R) {
    X <- (G+Z)-R*D
    G <- R*D
  } else {
    X <- 0
    G <- G+Z
  }
  res <- list(X = X,
              G = G)
  return(res)
}
