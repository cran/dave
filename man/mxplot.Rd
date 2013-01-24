\name{mxplot}
\alias{mxplot}
\alias{mxplot.default}
\alias{plot.mxplot}
\alias{matrixplot}
\title{
Matrixplot of groups similarities
}
\description{
This calclulates and plots average similarities of rows (releves) in a square, classified matrix. Correlation coefficient is used as similarity, upon plotting proportionas to the surface of circles. Hence, diagonal elements exhibit similarity of groups, off-diagonals all similarities to the remaining groups.
}
\usage{
mxplot(veg, rmember, use, y=1, ...)
matrixplot(veg, rmember, use, y=1)

\method{mxplot}{default}(veg, rmember, use, y=1,...)
\method{plot}{mxplot}(x,...,capacity=100)
}
\arguments{
  \item{veg}{
This is a vegetation data frame, releves are rows, species columns
}
  \item{rmember}{
Group membership of the rows or columns, typically taken from cluster analysis
}
  \item{use}{
Either "rows" or "columns"
}
  \item{y}{
Transformation of species scores: x'= x exp(y)
}
  \item{\dots}{
Capacity. Adjusts plot size to the number of groups.
}
  \item{x}{
An object of class "mxplot"
}
  \item{capacity}{
The number of group symbols that fit on one page
}
}
\details{
The distance measure used is "correlation used as distance". See reference.
}
\value{
An object of class "mxplot" with at least the following items:

\item{ order }{Dimension of the similarity matrix (equal to the number of groups ng)}
\item{ mmatrix }{The ng x ng matrix of average group similarity}
\item{ levels }{The ng group names (a vector of character variables)}
}
\references{
Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell, Chichester.
}
\author{
Otto Wildi
}
\note{
Plot parameter capacity only affects the format of plot. Just try.
}
\examples{
# Starts with classifying releves by cluster analysis
dd<- vegdist(sveg^0.5,method="euclid")         # dd is distance matrix
o.clust<- hclust(dd,method="ward")             # clustering
groups<- as.factor(cutree(o.clust,k=6))        # forming 6 groups

o.mxpl<- mxplot(sveg,groups,use="rows",y=0.5)
plot(o.mxpl,capacity=30)
}
\keyword{ multivariate }
\keyword{ hplot }