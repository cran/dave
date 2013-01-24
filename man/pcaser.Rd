\name{pcaser}         % DESCRIPTION OF FUNCTION pcaser, 24. 01 2013
\alias{pcaser}
\alias{pcaser.default}
\alias{plot.pcaser}
\alias{pcaser2}
\title{
Connecting time series in ordinations
}
\description{
Given a vegetation data frame containing various time series this plots a pca-ordination (\code{\link{pca}}) and connects points belonging to the same series with lines. The plots where the releves come from are identified by plotlabels (see below).
}
\usage{
pcaser(veg, plotlabels, y, ...)
pcaser2(veg, plotlabels, y=1)

\method{pcaser}{default}(veg, plotlabels, y, ...)
\method{plot}{pcaser}(x,...,lines=TRUE)
}
\arguments{
  \item{veg}{
This is a vegetation data frame, releves are rows, species columns
}
  \item{plotlabels}{
Plot names. Rows with identical name are assumed to belong to the same time series.
}
  \item{y}{
Transformation of species scores: x'= x exp(y)
}
  \item{\dots}{
Parameter lines=TRUE will connect the series. Otherwise they are distinguished by
symbols used in plot only.
}
  \item{x}{
An object of class "pcaser".
}
  \item{lines}{
A logical variable. When TRUE then points of the same time series are connected.
}


}
\value{
An object of class "pcaser" with at least the following items:

\item{comp1 }{Description of 'comp1'}
\item{nrel }{Total number of releves involved (i.e., row number)}
\item{nser }{Total number of time series (i.e., locations where the data stem from}
\item{scores }{The pca-ordination scores}
\item{plotlab }{Plot labels used for plotting}
\item{plotlabels }{Plot names}
\item{Eigv}{Eigenvalues (percentage)}
}
\references{
Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell, Chichester.
}
\author{
Otto Wildi
}
\examples{
o.pcaser<- pcaser(sn6veg,sn6sit$Plot.no,y=0.25)
plot(o.pcaser,lines=TRUE)                
}
\keyword{ hplot }
\keyword{ multivariate }
