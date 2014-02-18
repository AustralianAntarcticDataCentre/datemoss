\name{estimate_initial}
\alias{estimate_initial}
\title{Initialize date estimates}
\usage{
  estimate_initial(segments, tmin, method = "random",
    cal = NULL)
}
\arguments{
  \item{segments}{dataframe: the segment data, as returned
  by read_segment_data()}

  \item{tmin}{numeric: the earliest allowable date (specify
  as a fractional year)}

  \item{method}{string: method to use for generating the
  initial estimates, one of "random" [default], "estimate",
  or "oxcal". Methods other than "random" are experimental}

  \item{cal}{dataframe: the calibration data, as returned
  by read_calibration(). Only needed for method "estimate"}
}
\value{
  A vector of date estimates (specified as fractional
  years)
}
\description{
  Initialize date estimates
}

