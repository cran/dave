\name{dircor}       % DESCRIPTION OF FUNCTION dircor, 24. 01. 2013
\alias{dircor}
\alias{dircor2}
\alias{dircor.default}
\alias{plot.dircor}
\title{
Directional mantel correlation
}
\description{
Given a two-dimensional vegetation data frame and the x- and y-coordinates of the releves (the rows in the data frame) in geographical space, mantel correlation (function \code{\link{mantel}} in the vegan package) is evaluated at regular intervals of direction. Direction versus correlation is plotted including 95 percent confidence interval.  
}
\usage{
dircor(veg, x.axis, y.axis, step,...)
dircor2(veg, x.axis, y.axis, step = 5)

\method{dircor}{default}(veg, x.axis, y.axis, step,...)
\method{plot}{dircor}(x,...)
}
\arguments{
  \item{veg}{
A data frame of vegetation releves (rows) by species (columns)
}
  \item{x.axis}{
This is the x-coordinate in geographical space
}
  \item{y.axis}{
This is the y-coordinate in geographical space
}
  \item{step}{
The step length in degrees, used to draw the above mentioned function
}
  \item{x}{
An object of class "dircor"
}
  \item{\dots}{
Further variables used for printing
}
}

\details{
The method presently uses correlation as distance, as.dist((1-cor(t(sveg^2.0)))/2), as
distance measure for vegetation releves.}
\value{
An output list of class "dircor" with at least the following intems:

\item{steps}{The explicit steps used in degrees, 0 - 180 degrees}
\item{mean.correlation }{Mantel correlation at each step}
\item{lower.limit}{The lower confidence limits}
\item{upper.limit}{The upper confidence limits}
}
\references{
Legendre, P. & Fortin, M.-J. 1989. Spatial analysis and ecological modeling. 
Vegetatio 80: 107--138.

Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell,
Chichester.
}
\author{
Otto Wildi
}
\note{
Computation time is fairly long because function mantel() evaluates the confidence
limits for each directional step between 0 and 180 degrees.
}
\seealso{
The function mantel() in the vegan package.
}
\examples{
# vegetation data is taken from sveg
# the x- and y-axes are stored in object ssit
o.dircor<- dircor(sveg,ssit$x.axis,ssit$y.axis,step=40)
plot(o.dircor)
}
\keyword{ models }
\keyword{ multivariate }
