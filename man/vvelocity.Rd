\name{vvelocity}         % DESCRIPTION OF FUNCTION vvelocity, 15. 09. 2017
\alias{vvelocity}
\alias{vvelocity.default}
\alias{plot.vvelocity}
\alias{vvelocity2}
\title{
Printing ordinations of velocity and acceleration and differently transformed speed profiles
}
\description{
Given a data frame of a multivariate (vegetation) time series this plots a pco ordination using circles with diameters proportional to rate of change (velocity), a pco ordination \code{\link{pco}} using cirlces with diameters proportional to change in velocity (acceleration) and three velocity profiles with differently transformed species scores (from quantitative to qualitative).
}
\usage{
vvelocity(pveg, timescale, y, ...)
vvelocity2(pveg, timescale, y=1)

\method{vvelocity}{default}(pveg,timescale,y,...)
\method{plot}{vvelocity}(x,tlabs,scal=1,...)
}
\arguments{
  \item{pveg}{
A data frame of a multivariate (vegetation) time series
}
  \item{timescale}{
A vector of points in time of releves (rows)
}
  \item{y}{
Transformation of species scores: x'= x exp(y)
}
  \item{\dots}{
Additional arguments passed to plot.
}
  \item{tlabs}{
A vector of releve labels used for annotation of data points in ordinations. See example.
}
  \item{scal}{
A variable for scaling the circles in the ordinations. Default is scal=1.
}
  \item{x}{
An object of class "vvelocity".
}
}
\value{
An object of class "vvelocity" with at least the following items:

\item{pveg }{The input vegetation data frame}
\item{timescale }{The input time scale}
\item{y }{Transformation of species scores: x'= x exp(y)}
}
\details{
See also \code{\link{pco}} for the ordinations used.
}
\references{
Wildi, O. 2017. Data Analysis in Vegetation Ecology. 3rd ed. CABI, Oxfordshire, Boston.
}
\author{
Otto Wildi
}
\examples{
tlabs<- c(1,15,48,60,100,122,145)
timescale<- psit$Years.B.P
o.vvel<- vvelocity(pveg,timescale,y=0.5)
plot(o.vvel,tlabs=tlabs,scal=1)
}
\keyword{ ts }
\keyword{ multivariate }
