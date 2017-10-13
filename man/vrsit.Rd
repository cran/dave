\name{vrsit}
\alias{vrsit}
\docType{data}
\title{
Vraconnaz time series data, site factors and dates
}
\description{
A vegetation time series from 11 plots in the Vraconnaz peat bog, describing
species invasion and propagation after a bog burst in 1986. The corresponding
vegetation object is \code{\link{vrveg}}
}
\usage{data("vrsit")}
\format{
  A data frame with 231 observations on the following 26 variables.
  \describe{
    \item{\code{nom_de_lobjet}}{a factor with levels \code{La_Vraconnaz} \code{Vraconnaz}}
    \item{\code{nom_de_la_releviste}}{a factor with levels \code{E.Feldmeyer} \code{ef} \code{EF} \code{KE}}
    \item{\code{date}}{a factor with levels \code{01.01.91} \code{01.07.92} \code{01.07.99} \code{03.07.01} \code{04.07.01} \code{04.07.06} \code{04.07.07} \code{05.07.05} \code{05.07.06} \code{05.07.93} \code{06.07.04} \code{06.07.05} \code{06.07.06} \code{06.07.93} \code{07.07.04} \code{07.07.05} \code{07.07.93} \code{08.07.03} \code{08.07.04} \code{08.07.08} \code{09.07.02} \code{09.07.03} \code{09.07.08} \code{09.07.96} \code{09.07.97} \code{10.07.00} \code{10.07.02} \code{10.07.03} \code{10.07.07} \code{10.07.08} \code{10.07.89} \code{10.07.90} \code{10.07.97} \code{11.07.00} \code{11.07.02} \code{11.07.07} \code{11.07.89} \code{11.07.90} \code{11.07.95} \code{12.07.00} \code{12.07.07} \code{12.07.89} \code{12.07.90} \code{12.07.95} \code{13.07.89} \code{15.07.98} \code{16.07.98} \code{25.07.88} \code{26.07.88} \code{27.06.94} \code{27.07.88} \code{28.06.94} \code{28.07.88} \code{29.06.92} \code{29.06.94} \code{29.06.99} \code{29.07.88} \code{30.06.92} \code{30.06.99}}
    \item{\code{Jahr}}{a numeric vector}
    \item{\code{No_du_releve}}{a numeric vector}
    \item{\code{No_du_carre}}{a numeric vector}
    \item{\code{surface_.m2.}}{a numeric vector}
    \item{\code{recouvrement_muscinal}}{a numeric vector}
    \item{\code{recouvrement_herbace}}{a numeric vector}
    \item{\code{recouvrement_sous.arbore}}{a numeric vector}
    \item{\code{recouvrement_arbore}}{a numeric vector}
    \item{\code{litiere_seche}}{a numeric vector}
    \item{\code{tourbe_nue}}{a numeric vector}
    \item{\code{eau_libre}}{a numeric vector}
    \item{\code{Artenzahl}}{a numeric vector}
    \item{\code{Feuchtezahl}}{a numeric vector}
    \item{\code{Lichtzahl}}{a numeric vector}
    \item{\code{Temperaturzahl}}{a numeric vector}
    \item{\code{Kontinentalitaetszahl}}{a numeric vector}
    \item{\code{Reaktionszahl}}{a numeric vector}
    \item{\code{Naehrstoffzahl}}{a numeric vector}
    \item{\code{Humuszahl}}{a numeric vector}
    \item{\code{Dispersitaetszahl}}{a numeric vector}
    \item{\code{X}}{a numeric vector}
    \item{\code{Y}}{a numeric vector}
    \item{\code{Z}}{a numeric vector}
  }
}
\details{
For processing the data as time series only variable "Jahr" is used. There are 20 states in time resulting.
}
\source{
Feldmeyer-Christe, E., Kuechler, M. and Wildi, O. 2011. Patterns of early succession on bare peat in a
Swiss mire after a bog burst. Journal of Vegetation Science 22: 943-954.
}
\references{
Wildi, O. 2017. Data Analysis in Vegetation Ecology. 3rd ed. CABI, Oxfordshire, Boston.
}
\examples{
data(vrsit)
str(vrsit)
}
\keyword{datasets}
