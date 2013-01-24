\name{SNPsm}         % DESCRIPTION OF FUNCTION SNPsm, 23.10.2012
\alias{SNPsm}
\alias{SNPsm.default}
\alias{plot.SNPsm}
\alias{SNPsm2}
\title{
The spatial and temporal model of succession in the Swiss National Park
}
\description{
A dynamic model of succession on alp Stabelchod in the Swiss Nationl Park using differential equations and numerial integration. 6 species guilds are considered. Space is conceived as a grid of 30 times 40 cells. Typical simulation time is around 500yr. 
}
\usage{
SNPsm(trange,tsl,diff,r6,...)
SNPsm2(trange=100,tsl=5.0,diff=0.001,r6=NULL)

\method{SNPsm}{default}(trange, tsl, diff, r6, ...)
\method{plot}{SNPsm}(x, ...,out.seq=1,col=FALSE)
}
\arguments{
  \item{trange}{
Time range of simulation in yr
}
  \item{tsl}{
Time range of simulation in yr
}
  \item{out.seq}{
Time interval (yr) at which maps of the state are printed
}
  \item{diff}{
A diffusion coefficient driving random spatial propagation
}
  \item{r6}{
Growth rates of 6 guilds involved, increase in cover percentage per yr
}
  \item{\dots}{
Parameter out.seq, the plotting interval
}
  \item{x}{
An object of class "SNPsm"
}
  \item{col}{
A logical variable to suppress color printing 
}
}
\value{
An object of class "SNPsm" with at least the following items:

\item{n.time.steps }{Number of time steps used for numerical integration}
\item{imax }{Vertical grid count}
\item{jmax }{Horizontal grid count}
\item{time.step.length }{The time step length in yr}
\item{veg.types }{The names of the vegetation types, i.e., the species}
\item{vegdef }{A nspecies x nspecies matrix defining composition of vegetation types}
\item{growth.rates }{The growth rates given upon input}
\item{sim.data}{Simulated scores of all species (guilds) during simulation time}
\item{tmap}{The 30 x 40 grid map of types used as initial condition}
\item{igmap}{The same as tmap}
\item{frame}{A 30 x 40 grid showing initial forest edges, used for printing}
}
\references{
Wildi, O. 2002. Modeling succession from pasture to forest in time and space. Community Ecology 3: 181--189.

Wildi, O. 2013. Data Analysis in Vegetation Ecology. 2nd ed. Wiley-Blackwell, Chichester.
}
\author{
Otto Wildi
}
\examples{
r6=NULL           # imposes default growth rates
o.stSNP<- SNPsm(trange=100,tsl=10.0,diff=0.001,r6)
plot(o.stSNP,out.seq=50)
}
\keyword{ models }
\keyword{ multivariate }
