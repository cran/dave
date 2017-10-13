\name{EKs}
\alias{EKs}
\docType{data}
\title{
Swiss forest vegetation data base 1972, site information
}
\description{
Swiss forest vegetation data base 1972, site information. Vegetation data is in data frame \code{\link{EKv}}.
}
\usage{data(EKs)}
\format{
  A data frame with 2533 observations on the following 11 variables.
  \describe{
    \item{\code{Autor}}{a factor with author names as levels}
    \item{\code{Jahr}}{a numeric vector with year of survey}
    \item{\code{Tabellennr.}}{a numeric vector}
    \item{\code{Laufnr.}}{a numeric vector}
    \item{\code{Gesellschaftsname}}{a factor with name of vegetation unit as levels}
    \item{\code{Hoehe_u.M.}}{a numeric vector, elevation a.s.l.}
    \item{\code{Neigung_in_Prozent}}{a numeric vector}
    \item{\code{Exposition}}{a factor with exposure of plot as levels}
    \item{\code{Y.Koordinate}}{y coordinate, a numeric vector}
    \item{\code{X.Koordinate}}{x coordinate, a numeric vector}
    \item{\code{EK.Gesellschaftsnr}}{number (label) of vegetation unit the releve belongs to, a numeric vector}
  }
}
\details{
Classification used in 1972 is in variable "EK.Gesellschaftsnr"
}
\source{
Ellenberg, H. & Kloetzli, F. 1972.
Waldgesellschaften und Waldstandorte der Schweiz. Mitt. Eidgenoess.
Forsch. anst. Wald Schnee Landsch. 48(4): 587--930.

Keller, W., Wohlgemuth, T., Kuhn, N., Schuetz, M. &
Wildi, O. 1998. Waldgesellschaften der Schweiz auf floristischer
Grundlage. Mitteilungen der Eidgenoessischen Forschungsanstalt fuer
Wald, Schnee und Landschaft (WSL) 73, Vol. 2.
}
\references{
Wildi, O. 2017. Data Analysis in Vegetation Ecology. 3rd ed. CABI, Oxfordshire, Boston.
}
\examples{
summary(EKs)
}
\keyword{datasets}
