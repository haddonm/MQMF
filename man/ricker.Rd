% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fisheries.r
\name{ricker}
\alias{ricker}
\title{ricker one version of the Ricker stock recruitment}
\usage{
ricker(p, B)
}
\arguments{
\item{p}{a vector of length two of the a and b parameters}

\item{B}{a vector, possibly of length 1, of spawning biomass 
levels}
}
\value{
a vector of length = to B of predicted recruitments
}
\description{
ricker implements the Ricker stock recruitment
   equation where R = aBexp(-bxB), R is the recruitment, 
   a and b are the parameters and B is the spawning biomass. 
   a is recruits-per-spawner at low stock levels, and b
   relates to the decline in recruitment as spawning biomass
   increases.
}
\examples{
  B <- 1:10000
  rec <- ricker(c(10,0.0002),B)
  oldpar <- par(no.readonly=TRUE)
  plot1(B,rec,xlab="SpB",ylab="Recruitment",lwd=2)
  par(oldpar)
}
