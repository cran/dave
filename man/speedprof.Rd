\name{speedprof}         % DESCRIPTION OF FUNCTION speedprof, 24.10.2012
\alias{speedprof}
\alias{speedprof.default}
\alias{plot.speedprof}
\alias{speedprof2}
\title{
Plotting velocity profiles of multivariate time series
}
\description{
From a multivariate time series of vegetation data this first computes a distance matrix (\code{\link{vegdist}}, euclidean). The first plot is a graphical representation of the full distance matrix, the second a profile of change per time step  of different length (velocity).
}
\usage{
speedprof(veg, timescale, orders, y = 1, adjust,...)
speedprof2(veg, timescale, orders, y = 1, adjust)

\method{speedprof}{default}(veg, timescale, orders, y = 1, adjust,...)
\method{plot}{speedprof}(x,...)
}
\arguments{
  \item{veg}{
This is a vegetation data frame, releves are rows, species columns. Releves are
considered a time series.
}
  \item{timescale}{
A vector of points in time of releves (rows)
}
  \item{orders}{
Orders used for printing the velocity profile, i.e., the number of time steps used for calculating speed (rate of change per time unit).
}
  \item{y}{
Transformation of species scores: x'= x exp(y)
}
  \item{adjust}{
Parameter adjust=TRUE re-scales releves to vector sum=100 percent (assuming cover %).
}
  \item{x}{
An object of class "speedprof".
}
  \item{\dots}{
Parameter out.seq, the plotting interval
}
}
\value{
An object of class "SNPsm" with at least the following items:

\item{nrel }{The number of releves}
\item{dmatrix }{The distance matrix}
\item{timescale }{The time scale}
\item{orders }{Time step lengths considered for plotting the profile}
}
\references{
Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell, Chichester.
}
\author{
Otto Wildi
}
\examples{
orders<- c(1,2,3,4)
o.spp<- speedprof(tveg,tsit$Year,orders,y=1.0,adjust=TRUE)
plot(o.spp)
}
\keyword{ multivariate }
\keyword{ ts }