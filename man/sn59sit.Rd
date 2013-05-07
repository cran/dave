\name{sn59sit}
\alias{sn59sit}
\docType{data}
\title{
Time series from the Swiss National Park, 59 plots. Site data.
}
\description{
Time series from the Swiss National Park, 59 plots. Site data: Plot names and yr of sampling. Vegetation data in \code{\link{sn59veg}}.
}
\usage{data(sn59sit)}
\format{
  A data frame with 751 observations on the following 2 variables.
  \describe{
    \item{\code{Plot.no}}{a factor with 59 levels, the plot names}
    \item{\code{Year}}{a numeric vector}
  }
}
\source{
Wildi, O. & Schuetz, M. 2000. Reconstruction of a
long-term recovery process from pasture to forest. Community Ecology 1: 25-32.
}
\references{
Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell,
Chichester.
}
\examples{
summary(sn59sit)
}
\keyword{datasets}
