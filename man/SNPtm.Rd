\name{SNPtm}         % DESCRIPTION OF FUNCTION SNPtm, 23.10.2012
\alias{SNPtm}
\alias{SNPtm.default}
\alias{plot.SNPtm}
\alias{SNPtm2}
\title{
The temporal model of succession in the Swiss National Park
}
\description{
A dynamic model of succession in the Swiss Nationl Park using differential equations and numerial integration. 6 species guilds are considered. Typical simulation time is around 500yr. 
}
\usage{
SNPtm(trange, tsl, x6, r6,...)
SNPtm2(trange, tsl, x6, r6)

\method{SNPtm}{default}(trange, tsl, x6, r6,...)
\method{plot}{SNPtm}(x,...)
}
\arguments{
  \item{trange}{
Time range of simulation in yr
}
  \item{tsl}{
Time step length used for integration (no. of yr)
}
  \item{x6}{
Initial conditions of 6 guilds involved, cover percentage
}
  \item{r6}{
Growth rates of 6 guilds involved, increase in cover percentage per yr
}
  \item{x}{
An object of class "SNPtm"
}
  \item{\dots}{
Parameter out.seq, the plotting interval
}
}
\value{
An object of class "SNPtm" with at least the following items:

\item{n.time.steps }{Time step range covered by the model}
\item{time.step.length }{Time step length used for integration, no. of yr}
\item{time vector }{All time steps described by the results}
\item{veg.types }{The names of the vegetation types, i.e., the species}
\item{growth.rates }{The growth rates given upon input}
\item{initial.cond }{Initial conditions of 6 guilds involved, cover percentage}
\item{sim.data}{Simulated scores of all species (guilds) during simulation time}
}
\references{
Wildi, O. 2002. Modeling succession from pasture to forest in time and space. Community Ecology 3: 181--189.

Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell, Chichester.
}
\author{
Otto Wildi
}
\examples{
o.SNPtm<- SNPtm(trange=400,tsl=1.0,x6=NULL,r6=NULL)
plot(o.SNPtm)
}
\keyword{ multivariate }
\keyword{ model }
