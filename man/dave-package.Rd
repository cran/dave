\name{dave}      % HELP FILE FOR dave PACKAGE, 24. 01. 2013
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
Version: \tab 1.3\cr
Date: \tab 2013-01-24\cr
License: \tab LGPL <= 2.0 \cr
}
The use of all functions included is explained in "Data Analysis in Vegetation Ecology"
(see reference below)
}
\author{
Otto Wildi, otto.wildi@wsl.ch
}
\references{
Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell,
Chichester.
}
\keyword{ package }
\seealso{
Packages used include: vegan, labdsv, cluster, nnet, tree, rpart, MASS
and others.
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
