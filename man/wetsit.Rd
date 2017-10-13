\name{wetsit}
\alias{wetsit}
\docType{data}
\title{
Random sample of Swiss wetland vegetation, site information.
}
\description{
Random sample of Swiss wetland vegetation, site information. Vegetation information is in \code{\link{wetveg}}.
Note: this differs from the same file in the 2nd edition as 4 observations are erased and 16 new site factors added.
}
\usage{data(wetsit)}
\format{
  A data frame with 1496 observations on the following 85 variables.
  \describe{
    \item{\code{EK2_Identifikation}}{a factor with levels identifying phytosociologica identity}
    \item{\code{Flnr_ek1}}{a numeric vector, a plot number}
    \item{\code{Flnr_ek2}}{a numeric vector, a plot number}
    \item{\code{Area}}{a numeric vector, surface of plot}
    \item{\code{ek}}{a numeric vector}
    \item{\code{Objekt_Nr}}{a numeric vector}
    \item{\code{Kanton}}{a factor with levels identifying canton} 
    \item{\code{Datum}}{a factor with levels for date of sampling} 
    \item{\code{Autor_Code}}{a factor with levels for author code} 
    \item{\code{Det_Code}}{a factor with levels of author initials}
    \item{\code{Erhebung}}{a numeric vector, survey, mainly 1}
    \item{\code{Torfmoose}}{a factor with levels for Sphagnum cover}
    \item{\code{Uebrige_Moose}}{a factor with levels for cover of other mosses}
    \item{\code{Zwergstraeucher}}{a factor with levels for cover of dwarf shrubs}
    \item{\code{Straeucher}}{a factor with levels for cover of shrubs}
    \item{\code{Baeume}}{a factor with levels for cover of trees}
    \item{\code{Nekromasse}}{a factor with levels for cover of necro mass}
    \item{\code{Offener_Torf}}{a factor with levels for cover of open turf}
    \item{\code{Offener_Mineralboden}}{a factor with levels for mineral soil}
    \item{\code{Offene_Wasserflaeche}}{a factor with levels for open water surface}
    \item{\code{Stark_abgefressen}}{a factor with levels for browsing \code{_} \code{Ja} \code{Nein}}
    \item{\code{Kurz_geschnitten}}{a factor with levels for cutting \code{_} \code{Ja} \code{Nein}}
    \item{\code{X}}{a factor with levels for x-axis in space} 
    \item{\code{Y}}{a factor with levels for y-axis in space} 
    \item{\code{Z}}{a factor with levels for z-axis in space, elevation}
    \item{\code{humidity}}{a numeric vector}
    \item{\code{light}}{a numeric vector}
    \item{\code{temperature}}{a numeric vector}
    \item{\code{continentality}}{a numeric vector}
    \item{\code{reaction}}{a numeric vector}
    \item{\code{nutrients}}{a numeric vector}
    \item{\code{humus}}{a numeric vector}
    \item{\code{dispersity}}{a numeric vector}
    \item{\code{Assoziation1_ek1}}{a factor with levels for alliance names, first choice}
    \item{\code{Assoziation2_ek1}}{a factor with levels for alliance names, second choice}
    \item{\code{Assoziation3_ek1}}{a factor with levels for alliance names, third choice}
    \item{\code{X1._Wert_ek1}}{a numeric vector}
    \item{\code{X2._Wert_ek1}}{a numeric vector}
    \item{\code{X3._Wert_ek1}}{a factor with levels (rather than a numeric vector as above)}
    \item{\code{Differenz_Wert1.Wert2}}{a numeric vector}
    \item{\code{Unterverband1_ek1}}{a factor with names of sub-alliance as levels, first choice}
    \item{\code{Unterverband2_ek1}}{a factor with names of sub-alliance as levels, second choice}
    \item{\code{Verband1_ek1}}{a factor with names of alliances as levels, first choice}
    \item{\code{Verband2_ek1}}{a factor with names of alliances as levels, first choice}
    \item{\code{unklassierbar_verband}}{a factor with levels \code{_} \code{ja}}
    \item{\code{Unterordnung1_ek1}}{a logical vector}
    \item{\code{Unterordnung2_1ek1}}{a logical vector}
    \item{\code{Ordnung1_ek1}}{a factor with order as levels, first choice}
    \item{\code{Ordnung2_ek1}}{a factor with order as levels, second choice}   
    \item{\code{unklassierbar_ordnung}}{a factor with levels \code{_} \code{ja}}
    \item{\code{Unterklasse1_ek1}}{a logical vector}
    \item{\code{Unterklasse2_ek1}}{a factor with levels \code{Polygono-Poenea_annuae}}
    \item{\code{Klasse1_ek1}}{a factor with class as levels, first choice}
    \item{\code{Klasse2_ek1}}{a factor with class as levels, second choice}
    \item{\code{BAFU.Gruppe1_ek1}}{a factor with vegetation type in german, first choice} 
    \item{\code{BAFU.Gruppe2_ek1}}{a factor with vegetation type in german, second choice}
    \item{\code{Wirkungskontrolle1_ek1}}{a factor with vegetation type in german as levels}
    \item{\code{Wirkungskontrolle2_ek1}}{a factor with vegetation type in german as levels} 
    \item{\code{Wirkungskontrolle_engl1_ek1}}{a factor with vegetation type in english as levels} 
    \item{\code{Wirkungskontrolle_engl2_ek1}}{a factor with vegetation type in english as levels} 
    \item{\code{Moor_j.n1_ek1}}{a factor with levels \code{j} \code{n}}
    \item{\code{Moor_j.n2_ek1}}{a factor with levels \code{j} \code{n}}
    \item{\code{Assoziation_Nr}}{a numeric vector}
    \item{\code{Unterverband_Nr}}{a numeric vector}
    \item{\code{Verband_Nr}}{a numeric vector}
    \item{\code{Unterordnung_Nr}}{a logical vector}
    \item{\code{Ordnung_Nr}}{a numeric vector}
    \item{\code{Unterklasse_Nr}}{a logical vector}
    \item{\code{Klasse_Nr}}{a numeric vector}
    \item{\code{slp25_d8}}{a numeric vector}
    \item{\code{ddeg300}}{a numeric vector}
    \item{\code{precyy}}{a numeric vector}
    \item{\code{sfroyy}}{a numeric vector}
    \item{\code{tminall}}{a numeric vector}
    \item{\code{sradyy}}{a numeric vector}
    \item{\code{swb}}{a numeric vector}
    \item{\code{mind7}}{a numeric vector}
    \item{\code{SOILTYPE}}{a factor with levels identifying soil type}
    \item{\code{GRUNDIGKEI}}{a numeric vector}
    \item{\code{SKELETT}}{a numeric vector}
    \item{\code{WASSERSPEI}}{a numeric vector}
    \item{\code{NAHRSTOFF}}{a numeric vector}
    \item{\code{WASSERDURC}}{a numeric vector}
    \item{\code{VERNASS}}{a numeric vector}
    \item{\code{GT_ID}}{a numeric vector}
 }
}
\source{
Graf, U., Wildi, O., Feldmeyer-Christe, E. &
Kuechler, M. 2010. A phytosociological classification of Swiss mire
vegetation. Botanica Helvetica 120: 1-13.
}
\references{
Wildi, O. 2017. Data Analysis in Vegetation Ecology. 3rd ed. CABI, Oxfordshire, Boston.
}
\examples{
summary(wetsit)
}
\keyword{datasets}
