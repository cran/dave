\name{ccost}           %%  DESCRIPTION OF ccost(), 124. 01. 2013
\alias{ccost}
\alias{ccost.default}
\alias{print.ccost}
\alias{ccost2}
\title{
Cost function of 2 alternative classifications of rows in vegetation data
}
\description{
Given 2 alternative classifications (g groups) of rows in a data frame of vegetation data, confusion matrix, C, is derived first. Using the first classification a matrix of row centroids is derived (using function \code{\link{centroid}}) of wich a g by g distance matrix, W, is computed (correlation transformed to distance). Cost factor, cf, is the sum of element by element multiplication of C and W respectively, cf=sum(CW).
}
\usage{
ccost(veg, oldgr, newgr, y,...)
ccost2(veg,oldgr, newgr, y)

\method{ccost}{default}(veg, oldgr, newgr, y,...)
\method{print}{ccost}(x,...)
}
\arguments{
  \item{veg}{
A data frame of vegetation releves (rows) by species (columns)
}
  \item{oldgr}{
Initial classification, e.g., derived by hclust()
}
  \item{newgr}{
Final classification, e.g., result of a model
}
  \item{y}{
Transformation of species scores: x'= x exp(y)
}
  \item{x}{
An object of class "ccost"
}
  \item{\dots}{
Further variables used for printing
}
}
\details{
Cost factor cf has range 0 (both classifications identical) to n (number of rows),
where n is the worst case of misclassification.
}
\value{
An output list of class "ccost" with at least the following intems:
\item{dimension}{Dimension of confusion matrix (n by n)}
\item{ccost}{Cost factor, cf}
\item{old.groups}{Initial classification}
\item{new.groups}{Final classification}
\item{conf.matrix}{Confusion matrix}
\item{weight.matrix}{Weigth matrix}
\item{transf}{Transformation applied to scores, y-value}
%% ...
}
\references{
Ripley, B. D. 1996. Pattern recognition and neural networks.
Cambridge: Cambridge University Press.

Venables, W. N. & Ripley, B. D. 2010. Modern applied 
statistics with S. Fourth Edition. Springer, NY.

Wildi, O. 2017. Data Analysis in Vegetation Ecology. 3rd ed. CABI, Oxfordshire, Boston.
}
\author{
Otto Wildi
}
\examples{
# First, groups of releves are formed by cluster analysis
require(vegan)
dr<- vegdist(nveg^0.5,method="bray")      # dr is distance matrix of rows
o.clr<- hclust(dr,method="ward")          # this is clustering
oldgr<- cutree(o.clr,k=3)                 # 3 row groups formed
oldgr                                     # this displays initial classification:
# 2  4  6  9 10 18 25 27 39 49 50
# 1  2  1  3  2  3  1  2  3  1  3

# For simplicity we assume that row "2" and "50" change memebership:
newgr<- c(2,2,1,3,2,3,1,2,3,1,1)
o.ccost<- ccost(nveg,oldgr,newgr,y=0.5)           # does square root transformation
# Default method releasing cf
o.ccost                                           # displays C and W (see above)
}
\keyword{ misc }
\keyword{ array }
