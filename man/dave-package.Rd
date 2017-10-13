\name{dave}      % HELP FILE FOR dave PACKAGE, 10. 10. 2017
\alias{dave}
\alias{dave}
\docType{ package }
\title{
Data Analysis in Vegetation Ecology
}
\description{
A collection of function accompaining the book "Data Analysis in Vegetation Ecology". 
These are mainly multivariate methods explained in the book but not found elsewhere.
The package also includes all the data sets used in the book.
}
\details{
\tabular{ll}{
Package: \tab dave\cr
Type: \tab Package\cr
Version: \tab 2.0\cr
Date: \tab 2017-10-10\cr
License: \tab LGPL <= 2.0 \cr
}
The use of all functions included is explained in "Data Analysis in Vegetation Ecology"
(see reference below). Version 2.0 includes various new data frames, sspft and ssind, plant
functional types and indicator values respectively to be used in conjunction with sveg.
Also new is a somewhat longer time series, sn7veg and sn7sit and the new "Vraconnaz" time
series in vrveg and vrsit.
}
\author{
Otto Wildi, otto.wildi@wsl.ch
}
\references{
Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell, Chichester.

Wildi, O. 2017. Data Analysis in Vegetation Ecology. 3rd ed. CABI, Oxfordshire, Boston.
}
\keyword{ 
package, dave
}
\examples{
# A typical and probably the most complex function is Mtab() that re-arranges
# the rwos and columns within a vegetation data frame and through plotting it
# illustrates the presumably emerging pattern:
y.r<- 0.5 ; y.s<- 0.2                 # defining transformations used
k.r <- 3 ; k.s <- 4                   # row- and column numbers
ndiffs <- 18                          # no. of columns used to show pattern
o.Mt<-Mtabs(nveg,"mulva" ,y.r,y.s,k.r,k.s,ndiffs)
plot(o.Mt,method="normal")
# to see the original order simply replace "mulva" by "raw"
}
