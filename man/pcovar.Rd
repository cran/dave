\name{pcovar}         % DESCRIPTION OF FUNCTION pcovar, 24. 01. 2013
\alias{pcovar}
\alias{pcovar.default}
\alias{plot.pcovar}
\alias{pcoatest}
\title{
Plotting 6 variants of principal coordinates analysis
}
\description{
Plotting 6 ordinations using euclidean distance, manhattan distance, chord distance, Canberra distance, Bray-Curtis distance (\code{\link{vegdist}}) and correlation as distance respectively. Transformation of scores can be adjusted according to x'= x exp(y). All ordinations (\code{\link{pco}}) superimposed to PCA solution (\code{\link{pca}}) by \code{\link{procrustes}} analysis.
}
\usage{
pcovar(veg, y, ...)
pcoatest(veg, y=1)

\method{pcovar}{default}(veg, y, ...)
\method{plot}{pcovar}(x,...,reversals=c(0,0,0,0,0,0))
}
\arguments{
  \item{veg}{
A vegetation data frame, releves are rows, species columns
}
  \item{y}{
Transformation of species scores: x'= x exp(y)
}
  \item{\dots}{
Additional plot paramteters, see par.
}
  \item{reversals}{
Vector reversals=c(0,0,0,0,0,0). When set to 1 the corresponding plot is mirrored vertically.
}
  \item{x}{
An object of class "pcovar".
}
}
\value{
An object of class "pcovar" with at least the following items:

\item{nrel }{The number of releves}
\item{nspec }{The number of species}
\item{y}{Transformation of species scores: x'= x exp(y)}
\item{euclidpca}{PCA coordinates, euclid used, adjusted by procrustes analysis}
\item{euclidpco}{PCO coordinates, euclid used, adjusted by procrustes analysis}
\item{manhpco}{PCO coordinates, manhattan used, adjusted by procrustes analysis}
\item{manhpca}{PCA coordinates, manhattan used, adjusted by procrustes analysis}
\item{cordpco}{PCO coordinates, chord distance used, adjusted by procrustes analysis}
\item{cordpca}{PCA coordinates, chord distance used, adjusted by procrustes analysis}
\item{canpco}{PCO coordinates, canberra dist. used, adjusted by procrustes analysis}
\item{canpca}{PCA coordinates, canberry dist. used, adjusted by procrustes analysis}
\item{bpco}{PCO coordinates, Bray-Curtis dist. used, adjusted by procrustes analysis}
\item{bpca}{PCA coordinates, Bray-Curtis dist. used, adjusted by procrustes analysis}
\item{corpco}{PCO coord., correlation as dist. used, adjusted by procrustes analysis}
\item{corpca}{PCA coord., correlation as dist. used, adjusted by procrustes analysis}
}
\references{
Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell,
Chichester.
}
\author{
Otto Wildi
}
\note{
This function serves primarily instructional purposes
}
\examples{
o.pcovar<- pcovar(sveg,y=1)
plot(o.pcovar,reversals=c(0,0,0,0,0,0))
}
\keyword{ multivariate }
\keyword{ hplot }