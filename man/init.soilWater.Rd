% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/init.soilWater.R
\name{init.soilWater}
\alias{init.soilWater}
\title{Initialize information related to Soil Moisture}
\usage{
init.soilWater(method = NULL, path = NULL, Ea, G, X, Eabog, Gbog, Xbog,
  eatemp, cea, M, D, middelsca, R, Z, Zbog)
}
\arguments{
\item{method}{method for the initialization, "load", "source", "manual", "procecessed"}

\item{path}{directory where to get the files}

\item{Ea}{actual evapotranspiration}

\item{G}{Volume of received moisture input of rain and snowmelt}

\item{X}{excess water}

\item{Eabog}{actual evapotranspiration on bog}

\item{Gbog}{Volume of received moisture input of rain and snowmelt a time t on Bogs}

\item{Xbog}{excess water on bog}

\item{eatemp}{evapotranspiration}

\item{cea}{Degree day factor for evpotranspiration(mm/degree/day)}

\item{M}{Groundwater Storage Capacity (GSC)}

\item{D}{the potential volumee of water that is needed for complete saturation}

\item{middelsca}{average snow coverage over the level zones}

\item{R}{Ratio defining field capacity (fracion of D)}

\item{Z}{Actual water volume present in the soil moisture zone}

\item{Zbog}{Actual water volume present on bogs#' @keywords soilWater}
}
\description{
Initialize the information related to ssoil Moisture
}
\examples{
\dontrun{
init.soilWater()
}
}

