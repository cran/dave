\name{wssit}
\alias{wssit}
\docType{data}
\title{
Four kilometre grid forest data of Switzerland, site information
}
\description{
Four kilometre grid forest data of Switzerland, site information. Also see ws30, ws200
and ws500 for vegetation data.
}
\usage{data(wssit)}
\format{
  A data frame with 726 observations on the following 18 variables.
  \describe{
    \item{\code{x.coord}}{a numeric vector}
    \item{\code{y.coord}}{a numeric vector}
    \item{\code{hoehe}}{a numeric vector}
    \item{\code{elev}}{a numeric vector}
    \item{\code{slp}}{a numeric vector}
    \item{\code{ddeg.0}}{a numeric vector}
    \item{\code{prcp.yy}}{a numeric vector}
    \item{\code{sfro.yy}}{a numeric vector}
    \item{\code{tave.cc}}{a numeric vector}
    \item{\code{srad.yy}}{a numeric vector}
    \item{\code{swb}}{a numeric vector}
    \item{\code{min7}}{a numeric vector}
    \item{\code{s.depth}}{a numeric vector}
    \item{\code{s.wcap}}{a numeric vector}
    \item{\code{s.nutrient}}{a numeric vector}
    \item{\code{s.wperm}}{a numeric vector}
    \item{\code{s.wetn}}{a numeric vector}
    \item{\code{pH.LFI}}{a numeric vector}
  }
}
\details{
Corresponding vegetation data is in ws30, ws200 and ws500 respectively.
}
\source{
Wohlgemuth, T., Moser B., Braendli, U.-B., Kull, P. & Schuetz, M. 2008. 
Diversity of forest plant species at the community and landscape scales in Switzerland. 
Plant Biosystems 142: 604-613.
}
\references{
Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell,
Chichester.
}
\examples{
summary(wssit)
}
\keyword{datasets}
