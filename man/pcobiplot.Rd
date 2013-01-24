\name{pcobiplot}         % DESCRIPTION OF FUNCTION pcobiplot, 24.01.2013
\alias{pcobiplot}
\alias{pcobiplot.default}
\alias{plot.pcobiplot}
\alias{pcocoor}
\title{
Computing and plotting a biplot ordination using principal coordinates analysis
}
\description{
Computing a principal coordinates analysis of releves (rows, see \code{\link{pco}}) and subsequently the correlations with all species (columns). Two ordinations are plotted, one for releves and an arrow-plot for species. Species are restricted to the list given in sel.sp and species names are abbreviated upon request (see \code{\link{make.cepnames}}).
}
\usage{
pcobiplot(veg, method, y = 1, ...)
pcocoor(veg, method, y = 1)

\method{pcobiplot}{default}(veg,method,y=1,...)
\method{plot}{pcobiplot}(x,...,axes=c(1,2),sel.sp=NULL,shortnames=TRUE)
}
\arguments{
  \item{veg}{
This is a vegetation data frame, releves are rows, species columns
}
  \item{method}{
The method used for calculating distance. See function vegdist(), package vegan.
}
  \item{y}{
Transformation of species scores: x'= x exp(y)
}
  \item{\dots}{
Plot parameters axes=c(1,2), sel.sp=NULL (species selection),shortnames=TRUE for 
abbreviation of species names
}
  \item{shortnames}{
A logical variable, when TRUE delivering shortnames of species (package vegan used).
}
  \item{axes}{
A vector of length two, assessing the axes used for plotting. Default is c(1,2).
}
  \item{x}{
An object of class "pcobiplot"
}
  \item{sel.sp}{
The species (column numbers) to be included in the plot of arrows
}
}
\value{
An object of class "pcobiplot" with at least the following items:

\item{nrel }{The number of releves}
\item{nspe }{The number of species}
\item{rpoints }{Ordination scores of releves}
\item{spoints }{Ordination scores of species}
\item{allspnames }{The full list of species names}
}
\references{
Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell, Chichester.
}
\author{
Otto Wildi
}
\note{
If sel.sp is not specified a random selection of 6 species is taken
}
\examples{
sel.sp<- c(3,11,23,31,39,46,72,77,96)                        # selection of species
o.pcobiplot<- pcobiplot(sveg,method="bray",y=0.25)           # used pco
plot(o.pcobiplot,sel.sp=sel.sp,axes=c(1,2),shortnames=TRUE)  # plot of releves, sepecies
}
\keyword{ multivariate }
\keyword{ hplot }
