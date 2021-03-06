\name{sn6veg}
\alias{sn6veg}
\docType{data}
\title{
Time series from the Swiss National Park, 6 plots. Vegetation data.
}
\description{
Time series from the Swiss National Park, 6 plots. Vegetation data. Plot names are in \code{\link{sn6sit}}.
}
\usage{data(sn6veg)}
\format{
  A data frame with 81 observations on the following 6 variables (species guilds), cover percentage.
  \describe{
    \item{\code{Aconitum}}{a numeric vector}
    \item{\code{Deschampsia}}{a numeric vector}
    \item{\code{Trisetum}}{a numeric vector}
    \item{\code{Festuca}}{a numeric vector}
    \item{\code{Carex}}{a numeric vector}
    \item{\code{Pinus}}{a numeric vector}
  }
}
\details{
A subset of data frame sn59veg
}
\source{
Wildi, O. & Schuetz, M. 2000. Reconstruction of a
long-term recovery process from pasture to forest. Community Ecology 1: 25-32.
}
\references{
Wildi, O. 2017. Data Analysis in Vegetation Ecology. 3rd ed. CABI, Oxfordshire, Boston.
}
\examples{
summary(sn6veg)
}
\keyword{datasets}
