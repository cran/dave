\name{fspa}         % DESCRIPTION OF FUNCTION fspa, 24. 01 2013
\alias{fspa}
\alias{fspa.default}
\alias{plot.fspa}
\alias{fspa2}
\title{
Flexible shortest path adjustment
}
\description{
Flexible shortest path adjustment is a heuristic ordination method attempting to adjust pattern to ecological situations. It erases long distances in the  resemblance matrix and replaces these by the sum of intermediate steps. Subsequent ordination uses function \code{\link{pco}}.
}
\usage{
fspa(veg, method, d.rev, n.groups, ...)
fspa2(veg,method,d.rev=0.5,n.groups=3)

\method{fspa}{default}(veg, method, d.rev, n.groups, ...)
\method{plot}{fspa}(x,...,axes=c(1,2))
}
\arguments{
  \item{veg}{
A data frame of vegetation releves (rows) by species (columns)
}
  \item{method}{
The method used for calculating distance as available in function vegdist of package 
vegan, for instance method = "bray".
}
  \item{d.rev}{
The percentage of distances revised, for instance 0.5 (50 percent, the default).
}
  \item{n.groups}{
This classifies the data points for illustrative purposes (uses Ward's method).
}
  \item{\dots}{
Variable axes=c(1,2) (default), the axes to be plotted
}
  \item{x}{
An object of class "fspa".
}
  \item{axes}{
A vector of length two, assessing the axes used for plotting. Default is c(1,2).
}
}
\value{
An output list of class "fspa" with at least the following intems:

\item{ oldpoints }{Ordination scores before adjustment}
\item{ newpoints }{Ordination scores after adjustment}
\item{ symbols }{The symbols used for classified plot}
\item{ nline }{The number of lines to be drawn in the graph}
\item{ startline }{Coordinates where the lines start}
\item{ endline }{Coordinates where the lines end}
\item{ dmat.before }{Distance matrix before analysis}
\item{ dmat.after }{Distance matrix after analysis}
\item{ endline }{Coordinates where the lines end}
\item{ d.rev }{ proportion of distances revised}
}
\references{
Bradfield, G.E. & Kenkel, N.C. 1987. Nonlinear ordination using flexible shortest path adjustment of ecological distances. Ecology 68: 750--753.

Wildi, O. 2017. Data Analysis in Vegetation Ecology. 3rd ed. CABI, Oxfordshire, Boston.
}
\author{
Otto Wildi
}
\examples{
o.fspa<- fspa(sveg,method="euclid",d.rev=0.75,n.groups=6)  # sveg is vegetation data
plot(o.fspa,axes=c(1,2))                                   # plots axis 1, 2
}
\keyword{ multivariate }
\keyword{ graphs }
