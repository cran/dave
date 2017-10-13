\name{Mtabs}         % DESCRIPTION OF FUNCTION Mtabs, 20.10.2012
\alias{Mtabs}
\alias{Mtabs.default}
\alias{summary.Mtabs}
\alias{plot.Mtabs}
\alias{mtab}
\alias{plottab}
\alias{plottabl}
\alias{setgroupsize}
\title{
Structuring vegetation data tables
}
\description{
Mimics traditional manual ordering of vegetation data table by (i) clustering rows
and columns (\code{\link{hclust}}), (ii) rearranging the resulting groups according to the first AOC axis (\code{\link{aocc}}),
(iii) rearranging rows and columns inside groups based on CA (\code{\link{cca}}), (iv) Putting high 
resolving species on top of the table (\code{\link{aoc}}). Also offers variants for ordering.
}
\usage{
Mtabs(veg, method = "raw", y.r, y.s, k.r, k.s, ndiffs, ...)
mtab(veg, method = "raw", y.r, y.s, k.r, k.s, ndiffs)
plottab(veg,rorder=NULL,sorder=NULL,grr=NULL,grs=NULL,y=0.5)
plottabl(veg,rorder=NULL,sorder=NULL,grr=NULL,grs=NULL,y=0.5)
setgroupsize(vec)

\method{Mtabs}{default}(veg, method, y.r, y.s, k.r, k.s, ndiffs, ...)
\method{plot}{Mtabs}(x,...,method="normal")
\method{summary}{Mtabs}(object,...,range=NULL)
}
\arguments{
  \item{veg}{
This is a vegetation data frame, releves are rows, species columns
}
  \item{method}{
The method used for ordering: "raw", "sort", "ca", "clust", "aoc" or "mulva"
}
  \item{y.r}{
Transformation of species scores when clustering releves (rows): x'= x exp(y.r)
}
  \item{y.s}{
Transformation of species scores when clustering species (columns): x'= x exp(y.s)
}
  \item{k.r}{
The number of releve groups
}
  \item{k.s}{
The number of species groups
}
  \item{ndiffs}{
The number of (high resolving) species used for top portion of the table
}
  \item{\dots}{
Use method="normal" for conventional display, "compressed" for very large tables
}
  \item{rorder}{
The order of releves (rows) for printing
}
  \item{sorder}{
The order of species (columns) for printing
}
  \item{grr}{
The group labels of releves (rows) for printing
}
  \item{grs}{
The group labels of species (columns) for printing
}
  \item{x}{
An object of class "Mtabs"
}
  \item{object}{
An object of class "Mtabs"
}
  \item{range}{
A subset of species to be displayed in summary table, e.g., c(1,10) for the first 10.
}
  \item{vec}{
A vector of group labels, analyzed similar to function table(), but without sorting
}
  \item{y}{
Transformation of species scores: x'= x exp(y)
}
}
\details{
Function plottab() and plottabl() are for internal use only
}

\value{
An object of class "Mtabs" with at least the following items:

\item{method}{The method used for ordering}
\item{transf.r}{Argument y.r}
\item{transf.s}{Argument y.s}
\item{order.rel}{The resulting order of rows}
\item{order.sp}{The resulting order of columns}
\item{order.relgr}{The resulting order of releve groups}
\item{order.spgr}{The resulting order of species groups}
\item{MSCC}{Mean square contingency coefficient}
\item{CAeig.rel}{Eigenvalues of correspondence analysis}
\item{AOCeig.rel}{Eigenvalues of analysis of concentration}
\item{veg}{The input vegetation data frame}
\item{centroids}{The matrix of groups centroids (see summary.Mtabs}
}
\references{
Wildi, O. 1989. A new numerical solution to traditional phytosociological tabular 
classification. Vegetatio 81: 95--106.

Wildi, O. 2017. Data Analysis in Vegetation Ecology. 3rd ed. CABI, Oxfordshire, Boston.
}
\author{
Otto Wildi
}
\note{
This extremely complex procedure accords with conventions used in vegetation ecology.
It assumes that the vegetation data frame has many zero entries (plots in which species
are not found). The summary method displays a frequency table (relative frequency of 
all species within the releve groups, \code{\link{centroid}}).
}
\examples{
y.r<- 0.5 ; y.s<- 0.2                 # defining transformations used
k.r <- 3 ; k.s <- 4                   # row- and column numbers
ndiffs <- 18                          # no. of columns used to show pattern
o.Mt<-Mtabs(nveg,"mulva" ,y.r,y.s,k.r,k.s,ndiffs)
plot(o.Mt,method="normal")
# to see the original order simply replace "mulva" by "raw"
}
\keyword{ multivariate }
\keyword{ array }
