\name{nsit}
\alias{nsit}
\docType{data}
\title{
European beach forest data, site factors
}
\description{
European beach forest data, site factors. Vegetation information is in data frame \code{\link{nveg}}.
}
\usage{data(nsit)}
\format{
  A data frame with 11 observations on the following 8 site variables.
  \describe{
    \item{\code{PH}}{a numeric vector}
    \item{\code{ALTITUDE}}{a numeric vector}
    \item{\code{SLOPE.deg}}{a numeric vector}
    \item{\code{X.AXIS}}{a numeric vector}
    \item{\code{Y.AXIS}}{a numeric vector}
    \item{\code{EXPOSURE}}{a factor with levels \code{E} \code{N} \code{S}}
    \item{\code{YEAR}}{a numeric vector}
    \item{\code{GROUP_NO}}{a numeric vector}
  }
}
\details{
Artificial data
}
\source{
Wildi, O. & Orloci, L. 1996. Numerical Exploration
of Community Patterns. 2nd ed. SPB Academic Publishing, The Hague.
}
\references{
Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell,
Chichester.
}
\examples{
summary(nsit)
}
\keyword{datasets}
