\name{outlier}         % DESCRIPTION OF FUNCTION outlier, 21.10.2012
\alias{outlier}
\alias{outlier.default}
\alias{plot.outlier}
\alias{print.outlier}
\alias{outly}
\title{
Outlier detection
}
\description{
Identifies outliers based on the nearest neighbour criterion. It starts by computing a matrix of distances (correlation, r, used as distance, dr=(1-r)/2). Variables with nearest neighbour distance larger than parameter thresh are considered outliers.
}
\usage{
outlier(veg, thresh, y,...)
outly(veg, thresh = 0.2, y = 0.5)

\method{outlier}{default}(veg, thresh, y,...)
\method{plot}{outlier}(x,...)
\method{print}{outlier}(x,...)
}
\arguments{
  \item{veg}{
This is a vegetation data frame, releves are rows, species columns
}
  \item{thresh}{
Threshold nearest neighbour distance for outliers
}
  \item{y}{
Transformation of species scores: x'= x exp(y)
}
  \item{x}{
An object of class "outlier"
}
  \item{\dots}{
Parameter out.seq, the plotting interval
}
}
\value{
An object of class "oulier" with at least the following items:

\item{threshold}{Threshold nearest neighbour distance for considering outliers}
\item{y}{Transformation of species scores: x'= x exp(y)}
\item{rel.names}{All row names}
\item{neigh.names}{Names of the corresponding nearest neighbours}
\item{neigh.dist}{Distance to the nearest neighbour}
\item{olddim}{Dimensions of data frame veg}
\item{newdim}{Dimensions of data frame with outliers erased}
\item{new.data}{Vegetation data frame without outliers}
\item{pco.points}{The pco ordination scores use for plotting}
}
\references{
Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell, Chichester.
}
\author{
Otto Wildi
}
\examples{
o.outlier<- outlier(nveg,thresh=0.2,y=0.5)
o.outlier                                    # a list of all variables
plot(o.outlier)                              # nearest neighbour histogram and
                                             # pco ordination 
}
\keyword{ multivariate }
\keyword{ arith }
