\name{sspft}
\alias{sspft}
\docType{data}
\title{
Selected plant functional traits of all species in data set "sveg"
}
\description{
For each species in "sveg" a selction of 23 traits are taken from the "Flora Indicativa by" Landolt et al. (2010). These are all nominal variables. Score 1 indicates the trait is present, score 0 it is absent. Hence, there is no such things like missing values.
}
\usage{data(sspft)}
\format{
  A data frame with 119 observations on the following 23 variables.
  \describe{
    \item{\code{LF.g}}{Life form "geophyte" (plants with resting buds below ground)}
    \item{\code{LF.c}}{Life form "herbaceous" (herbaceous plant, buds on shoots above ground)}
    \item{\code{LF.z}}{Life form "woody chamaephyte" (dwarf shrub, buds above ground)}
    \item{\code{LF.p}}{Life form "phanerophyte" (woody shrub or tree, > 4m)}
    \item{\code{LF.n}}{Life form "nanophanerophyte" (woody shrub or tree, < 4m)}
    \item{\code{LF.a}}{Life form "hydrophyte" (plants with buds submerged)}
    \item{\code{LF.h}}{Life form "hemicryptophyte" (buds on or directly below ground, rosettes, tussocks)}
    \item{\code{LF.t}}{Life form "therophyte" (plant dying back, surviving as seed or annual hemicryptophyte)}
    \item{\code{LF.hp}}{Life form "hemiparasite" (plant obtaining water and nutrients from host, green leaves)}
    \item{\code{LF.k}}{Life form "short-lived hemicryptophyte" (resting buds near ground, hapaxantic species)}
    \item{\code{LF.ff}}{Life form "carnivorous species" (consuming some nutrients from animals)}
    \item{\code{LF.moss}}{Life form "moss" (in separate list of Landolt et al. 2010)}
    \item{\code{LF.sph}}{Life form "Sphagnum" (all species of genus Sphagnum)}
    \item{\code{FS.zw}}{Reproduction "hermaphrodictic, normal sexual" (pollination necessary to reproduce)}
    \item{\code{FS.cl}}{Reproduction "cleistogamouns" (normal flowers and self-pollinating)}
    \item{\code{FS.di}}{Reproduction "unisexual and dioceous" (only male or female organs)}
    \item{\code{FS.mo}}{Reproduction "unisexual and monoecious" (male, female and bisexual flowers on one plant)}
    \item{\code{FS.ve}}{Reproduction "polysexual" (male and/or female and bisexual flowers on one individual)}
    \item{\code{FS.fa}}{Reproduction "facultatively apomictic" (sexual and simultaneously asexual reproduction)}
    \item{\code{FS.oa}}{Reproduction "obligate apomictic" (mostly bisexual, fertilization necessary for reproduction)}
    \item{\code{c}}{Life strategy "competitive" (competitive, long-lived), range 1-3}
    \item{\code{r}}{Life strategy "ruderal" (pioneer species, short-lived), range 1-3}
    \item{\code{s}}{Life strategy "stress-tolerant" (adapted to harsh environmental conditions), range 1-3}
  }
}
\details{
Species traits from vascular plants and brypphytes stem from different lists in Landolt et al. (2010). Note that this data set was not in cluded in the original dave package, but added in version 1.5 only.
}
\source{
Landolt, E., Baeumler, B., Erhardt, A., Hegg, O., Kloetzli, F., Laemmler, W., Nobis, M., Rudmann-Maurer, K., Schweingruber, F.H., Theurillat, J.-P., Urmi, E., Vust, M. and Wohlgemuth, T. 2010. Flora indicativa. Ecological Indicator Values and Biological Attributes of the Flora of Switzerland and the Alps. Haupt, Bern.
}
\references{
Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell,
Chichester.
}
\examples{
summary(sspft)
}
\keyword{datasets}
