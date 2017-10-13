\name{fitmarkov}     % DESCRIPTION OF FUNCTION fitmarkov, 08. 12. 2012
\alias{fitmarkov}
\alias{fitmarkov.default}
\alias{plot.fitmarkov}
\alias{rfitmarkov}
\title{
Approximating a Markov chain
}
\description{
Given a vegetation data frame considerd a time series with releves as rows and species as columns transition matrices are derived vor each time step based on some simple assumptions. These are averaged and a model series is derived trough scalar products. Time steps are given in a separate vector t. Missing steps are properly processed.
}
\usage{
fitmarkov(veg, t, adjust = FALSE, ...)
rfitmarkov(veg, t, adjust)

\method{fitmarkov}{default}(veg, t, adjust = FALSE, ...)
\method{plot}{fitmarkov}(x,...)
}
\arguments{
  \item{veg}{
This is a vegetation data frame, releves are rows, species columns
}
  \item{t}{
The time step scale of length according with rows in x
}
  \item{x}{
An object of class "fitmarkov"  	
}
  \item{adjust}{
A logical vector adjusting the sum of species scores to 1.0. Default is adjust=FALSE
}
  \item{\dots}{
Vector colors of any length for line colors, vector widths for line widths. See
example below.
}
}
\details{
This method yields a possible solution for fitting a Markov series. The true process may be very different.
}
\value{
An output list of class "fitmarkov" with at least the following intems:

\item{fitted.data }{The fitted time series'}
\item{raw.data }{The input time series'}
\item{transition.matrix}{The mean transition matrix'}
\item{t.measured}{The time steps upon input where time steps may be missing'}
\item{t.modeled}{The time steps upon output, no missing steps'}
}
\references{
Orloci, L., Anand, M. & He, X. 1993. Markov chain: a realistic model for temporal coenosere? Biom. Praxim 33: 7-26.

Lippe, E., De Smitt, J.T. & Glenn-Lewin, D.C. 1985. Markov models and succession: a test from a heathland in the Netherlands. Journal of Ecology 73: 775-791.

Wildi, O. 2017. Data Analysis in Vegetation Ecology. 3rd ed. CABI, Oxfordshire, Boston.
}
\author{
Otto Wildi
}
\note{
The aim of this method is to provide a smooth curve based on input data. Because this relies on incomplete information, it is just one out of many solutions.
}
\examples{
# data frame ltim is Lippe's data (see references)
# ltim just contains the time scale of the same
o.fm<- fitmarkov(lveg,ltim$Year)
plot(o.fm)
}
\keyword{ multivariate }
\keyword{ models }