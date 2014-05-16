\name{orank}         % DESCRIPTION OF FUNCTION orank, 25. 06. 2014
\alias{orank}
\alias{orank.default}
\alias{plot.orank}
\alias{summary.orank}
\alias{orank1}
\title{
Ranking by orthogonal components (RANK)
}
\description{
Given a correlation matrix of rows or columns this selects the variable sharing a 
maximum variance with all others and declares this rank 1. Reduces the matrix 
(covariances, correlations) by the contribution of the variable ranked first. Repeats
the process to derive consecutive ranks until no variance is left.
}
\usage{
orank(veg, use, rlimit=5, y=1, x.axis=NULL, y.axis=NULL,...)
orank1(veg, use, rlimit=5, y=1, x.axis=NULL, y.axis=NULL)

\method{orank}{default}(veg, use, rlimit=5, y=1, x.axis=NULL, y.axis=NULL,...)
\method{plot}{orank}(x,...)
\method{summary}{orank}(object,...)
}
\arguments{
  \item{veg}{
This is a vegetation data frame, releves are rows, species columns
}
  \item{use}{
Either "rows" or "columns"}
  \item{rlimit}{
The maximum number of ranks to be determined}
  \item{y}{
Transformation of species scores: x'= x exp(y)
}
  \item{x.axis}{
Horizontal axis used for plotting result in a sampling plan
}
  \item{y.axis}{
Vertical axis used for plotting result in a sampling plan
}
  \item{x}{
An object of class "orank"
}
  \item{\dots}{
Further variables used for printing
}
  \item{object}{
An object of class "orank"
}
}
\details{
If x-axis=NULL or y-axis=NULL then a pcoa-ordination is computed
and the first two axes used for plotting
}
\value{
An object of class "orank" with at least the following items:

\item{use}{Either "rows" or "columns"}
\item{n.ranks}{The number of ranks}
\item{var.names}{Names of the ranked variables}
\item{var.explained}{Explained variance of the ranked variables}
\item{var.percent}{Percentage of the variance explained}
\item{cum.var}{Cummulative variance of ranked variables, percentage}
\item{x.axis}{The same as input parameter x.axis}
\item{y.axis}{The same as input parameter y.axis}
}
\references{
Orloci, L. 1973. Ranking characters by a dispersion criterion.
Nature 244: 371--373.

Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell,
Chichester.
}
\author{
Otto Wildi
}
\note{
The present function exclusively relies on a correlation matrix, function \code{\link{cor}}.
}
\examples{
# Uses vegetation data frame sveg with vegetation data
# and ssit with corresponding x- and y-axes scores
x.axis=ssit$x.axis ; y.axis=ssit$y.axis
o.orank<- orank(sveg,use="rows",rlimit=5,y=0.25,x.axis,y.axis)
plot(o.orank)
}
\keyword{ multivariate }
\keyword{ arith }
