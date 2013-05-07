\name{tsit}
\alias{tsit}
\docType{data}
\title{
Time series from the Swiss National Park, Plot Tr6. Site data.
}
\description{
Time series from the Swiss National Park, plot Tr6. Site data: Plot names and yr of sampling. Vegetation is in \code{\link{tveg}}.
}
\usage{data(tsit)}
\format{
  A data frame with 16 observations on the following 2 variables.
  \describe{
    \item{\code{Plot.no}}{a factor with levels \code{Tr6}}
    \item{\code{Year}}{a numeric vector}
  }
}
\details{
A subset of data frame \code{\link{sn59sit}}.
}
\source{
Wildi, O. & Schuetz, M. 2000. Reconstruction of a
long-term recovery process from pasture to forest. Community Ecology 1: 25-32.
}
\references{
Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd Edition, Wiley-Blackwell,
Chichester.
}
\examples{
data(tsit)
## maybe str(tsit) ; plot(tsit) ...
}
\keyword{datasets}
