\name{sn59veg}
\alias{sn59veg}
\docType{data}
\title{
Time series from the Swiss National Park, 59 plots. Vegetation data.
}
\description{
Time series from the Swiss National Park, 59 plots. Vegetation data. Variables are species guilds. Site data is in \code{\link{sn59sit}}.
}
\usage{data(sn59veg)}
\format{
  A data frame with 751 observations on the following 6 variables (the species guilds).
  \describe{
    \item{\code{Aconitum}}{a numeric vector}
    \item{\code{Trisetum}}{a numeric vector}
    \item{\code{Deschampsia}}{a numeric vector}
    \item{\code{Festuca}}{a numeric vector}
    \item{\code{Carex}}{a numeric vector}
    \item{\code{Pinus}}{a numeric vector}
  }
}
\source{
Wildi, O. & Schuetz, M. 2000. Reconstruction of a
long-term recovery process from pasture to forest. Community Ecology 1: 25-32.
}
\references{
Wildi, O. 2017. Data Analysis in Vegetation Ecology. 3rd ed. CABI, Oxfordshire, Boston.
}
\examples{
summary(sn59veg)
}
\keyword{datasets}
