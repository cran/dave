\name{ssind}
\alias{ssind}
\docType{data}
\title{
Indicator values of all species in data set "sveg"
}
\description{
For each species in "sveg" all 8 indicator values are taken from the "Flora Indicativa by" Landolt et al. (2010). These are ranks on a 1 to 5 scale (except for M), but for some indicators half steps are used as well. Scores zero (0) are either undefined or unknown and must be treated as missing values
}
\usage{data(ssind)}
\format{
  A data frame with 119 observations on the following 9 variables.
  \describe{
    \item{\code{T}}{Temperature value (1-5, 9 steps)}
    \item{\code{K}}{Continentality value (1-5, 5 steps)}
    \item{\code{L}}{Light value (1-5, 5 steps)}
    \item{\code{F}}{Moisture value (1-5, 5 steps)}
    \item{\code{W}}{Moisture availability (1-3, 3 steps)}
    \item{\code{R}}{Reaction value (acidity, 1-5, 5 steps)}
    \item{\code{N}}{Nutrient value (1-5, 9 steps)}
    \item{\code{H}}{Humus value (1-3, 3 steps)}
    \item{\code{D}}{Soil aeration value (1-3, 3 steps)}
  }
}
\details{
Indicator values from vascular plants and brypphytes stem from different lists in Landolt et al. (2010). Note that this data set was not in cluded in the original dave package, but added in version 1.5 only.
}
\source{
Landolt, E., Baeumler, B., Erhardt, A., Hegg, O., Kloetzli, F., Laemmler, W., Nobis, M., Rudmann-Maurer, K., Schweingruber, F.H., Theurillat, J.-P., Urmi, E., Vust, M. and Wohlgemuth, T. 2010. Flora indicativa. Ecological Indicator Values and Biological Attributes of the Flora of Switzerland and the Alps. Haupt, Bern.
}
\references{
Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell,
Chichester.
}
\examples{
summary(ssind)
}
\keyword{datasets}
