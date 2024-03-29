% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/components.R
\name{getDetritusProduction}
\alias{getDetritusProduction}
\title{Detritus production rate}
\usage{
getDetritusProduction(
  params,
  n = params@initial_n,
  n_other = params@initial_n_other,
  rates = getRates(params)
)
}
\arguments{
\item{params}{MizerParams}

\item{n}{A matrix of current species abundances (species x size)}

\item{n_other}{Other dynamic components. Only \code{n_other$carrion} is used.}

\item{rates}{A list of rates as returned by \code{\link[=getRates]{getRates()}}}
}
\value{
A vector with named entries "external",
"feces" and "carrion", giving the rates at which carrion biomass
is produced by these sources in grams per year.
}
\description{
Gives a named vector with the rates at which different components of the
ecosystem produce detritus:
\enumerate{
\item biomass not assimilated by predators ("feces"),
\item decomposing carrion ("carrion"),
\item the pelagic zone ("external").
}
}
\details{
The function returns a vector with the individual contributions. These
can be summed with \code{sum()} to get the total production rate.
}
