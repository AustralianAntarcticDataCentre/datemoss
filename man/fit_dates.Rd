\name{fit_dates}
\alias{fit_dates}
\title{Estimate dates}
\usage{
  fit_dates(segments, f0, cal, mcmc.thin = 25,
    mcmc.iters = 2000, mcmc.burnin = 200, mcmc.sigma = 4)
}
\arguments{
  \item{segments}{dataframe: the segment data, as returned
  by read_segment_data()}

  \item{f0}{numeric: the initial estimate, as returned by
  estimate_initial()}

  \item{cal}{dataframe: the calibration data, as returned
  by read_calibration()}

  \item{mcmc.thin}{numeric: thinning parameter for MCMC
  fitting}

  \item{mcmc.iters}{numeric: number of iterations for MCMC
  fitting}

  \item{mcmc.burnin}{numeric: number of burn-in iterations
  for MCMC fitting}

  \item{mcmc.sigma}{numeric: sigma parameter for MCMC
  fitting}
}
\value{
  A vector of date estimates (specified as fractional
  years)
}
\description{
  Uses an MCMC method to estimate date span of each
  segment, given the measured d14C for each segment and the
  atmospheric d14C curve
}

