% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{crw}
\alias{crw}
\title{a random walk Metropolis sampler using R}
\usage{
crw(sigma, x0, N)
}
\arguments{
\item{sigma}{the variances are used for the normal distribution}

\item{x0}{Initial value of x}

\item{N}{the number of samples}
}
\value{
a random sample of size
}
\description{
a random walk Metropolis sampler using R
}
\examples{
\dontrun{
N=2000
sigma=c(.05,.5,2,16)
x0=25
crw1=crw(sigma[1],x0,N)
crw2=crw(sigma[2],x0,N)
crw3=crw(sigma[3],x0,N)
crw4=crw(sigma[4],x0,N)
}
}
