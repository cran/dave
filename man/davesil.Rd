\name{davesil}     % DESCRIPTION OF davesil(), 14. 05. 2014
\alias{davesil}
\alias{davesil.default}
\alias{plot.davesil}
\alias{dsil}
\title{
Modified version of silhouette plotting
}
\description{
This is a wrapper for function \code{\link{silhouette}} in the cluster package. It also relies on the output of \code{\link{hclust}} and \code{\link{cutree}}.
}
\usage{
davesil(ddist, o.hclr, o.relgr, ...)
dsil(ddist, o.hclr, o.relgr)

\method{davesil}{default}(ddist, o.hclr, o.relgr, ...)
\method{plot}{davesil}(x,...,range=NULL)
}
\arguments{
  \item{ddist}{
A distance matrix, probably the same as used for clustering
}
  \item{o.hclr}{
Output object of function hclust()
}
  \item{o.relgr}{
Output object of function cutree()
}
  \item{\dots}{
Plot parameter range(a,b) can be specified to limit plot to the subsed specified by a (begin) and b (end).
}
  \item{x}{
An object of class "davesil"
}
  \item{range}{
A vector of length 2, allows to plot a portion of the silhouette, e.g., range=c(1,5) plots the first 5.
}
}
\value{
An output list of class "davesil" with at least the following items:
\item{sil}{Data for drawing the silhouette, computed by silhouette()}
\item{names}{The names of the items clustered, first 15 characters, used for plotting}
}
\references{
Rousseeuw, P.J. (1987). Silhouettes: A graphical aid to the interpretation and validation of cluster analysis. J. Comput. Appl. Math., 20: 53-65.

Wildi, O. 2017. Data Analysis in Vegetation Ecology. 3rd ed. CABI, Oxfordshire, Boston.
}
\author{
Otto Wildi
}
\details{
See function \code{\link{silhouette}} in the cluster package.
}
\examples{
# An oridinary cluster analysis
ddr<- as.dist((1-cor(t(nveg)))/2)         # distance matrix, correlation as distance
o.hclr<- hclust(ddr,method="complete")
o.relgr<- cutree(o.hclr,k=3)
# Getting silhouette plot
o.davesil<- davesil(ddr,o.hclr,o.relgr)
plot(o.davesil)
}
\keyword{ cluster }
\keyword{ hplot }
