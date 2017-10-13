\name{overly}         % DESCRIPTION OF FUNCTION overly, 14. 09. 2017
\alias{overly}
\alias{overly.default}
\alias{plot.overly}
\alias{overly2}
\title{
Overly of multivariate time series
}
\description{
This function attempts to superimpose (overlay) multivariate time series that typically stem from different locations to form one single series. Series are combined pairwise following the arrangement in a minimum spanning tree, \code{\link{spantree}}. The result is a time series which may be longer than the contributing series, but shorter than their length total.
}
\usage{
overly(veg, Plot.no, y, sint, ...)
overly2(veg, Plot.no, y, sint)

\method{overly}{default}(veg, Plot.no, y, sint, ...)
\method{plot}{overly}(x,...,colors=NULL,l.widths=NULL)
}
\arguments{
  \item{veg}{
This is a vegetation data frame, releves are rows, species columns. It is assumed
that it constitutes two or more time series
}
  \item{Plot.no}{
Plot names. Rows with identical name are assumed to belong to the same time series.
}
  \item{y}{
Transformation of species scores: x'= x exp(y)
}
  \item{sint}{
Length of time interval. Affects display only.
}
  \item{\dots}{
Parameters colors=NULL, l.width=NULL, colors and line widths in plots.
}
  \item{colors}{
A vector of colors, such as c(1,2,3). The entries are recycled upon printing.
}
  \item{l.widths}{
A vector of line widths used for plotting, e.g., c=(0.5,1,1.5,2).The entries are recycled upon printing.
}
  \item{x}{
An object of class "overly"
}
}
\details{
In plant ecology this procedure is also known as space-for-time substitution.
See also \code{\link{pco}},\code{\link{spantree}}.
}
\value{
An object of class "overly" with at least the following items:

\item{plot.labels}{Names of plots, see Plot.no above}
\item{n.tseps}{The resulting (synthetic) number of time steps}
\item{tseps}{A vector of time steps in time units}
\item{tser.data}{The resulting vegetation time steps}
\item{ord.scores}{The pco scores of the ordination of time series}
\item{d.mat}{Euclidean distance matrix of time series}
\item{vegraw}{Input vegetation data frame veg (see above)}
\item{linex1}{Starting address of the time series in the synthetic time frame}
\item{linex2}{End address of the time series in the synthetic time frame}
\item{ltex}{The plot names}
\item{sint}{The time interval (see above)}
\item{vegtypes}{The species names involved}
}
\references{
Wildi, O. and Schuetz, M. 2000. Reconstruction of a long-term recovery process from pasture to forest. 
Community Ecology 1: 25--32. 
Wildi, O. 2017. Data Analysis in Vegetation Ecology. 3rd ed. CABI, Oxfordshire, Boston.
}
\author{
Otto Wildi
}
\examples{
o.overly<- overly(sn59veg,sn59sit$Plot.no,y=0.5,sint=5)
# Plotting (i) minimum spanning tree, (ii) temporal arrangement of time series,
# (iii) synthetic multivariate time series
plot(o.overly,colors=NULL,l.width=NULL)
}
\keyword{ multivariate }
\keyword{ ts }
\keyword{ cluster }
