\name{sn6sit}
\alias{sn6sit}
\docType{data}
\title{
Time series from the Swiss National Park, 6 plots. Site data.
}
\description{
Time series from the Swiss National Park, 6 plots. Site data: Plot names and yr of sampling. Vegetation data in \code{\link{sn6veg}}.
}
\usage{data(sn6sit)}
\format{
  A data frame with 81 observations on the following 2 variables.
  \describe{
    \item{\code{Plot.no}}{a factor with levels \code{Ac9} \code{FN2} \code{MU21} \code{N8} \code{PF1} \code{Pin3}}
    \item{\code{Year}}{a numeric vector}
  }
}
\details{
A subset of data frame sn59sit
}
\source{
Wildi, O. & Schuetz, M. 2000. Reconstruction of a
long-term recovery process from pasture to forest. Community Ecology 1: 25-32.
}
\references{
Wildi, O. 2017. Data Analysis in Vegetation Ecology. 3rd ed. CABI, Oxfordshire, Boston.
}
\examples{
summary(sn6sit)
}
\keyword{datasets}
