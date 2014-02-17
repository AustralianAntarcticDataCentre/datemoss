\name{read_calibration}
\alias{read_calibration}
\title{Read atmospheric 14C calibration data file}
\usage{
  read_calibration(filename = NULL, presmooth.width = NA,
    fudge.factor = NA)
}
\arguments{
  \item{filename}{string: the file to read. If NULL, the
  atmospheric data from Hua et al. (2013) is used. File is
  expected to be csv-format, with the columns "Year" and
  "PMC"}

  \item{presmooth.width}{numeric: (experimental) apply
  sliding mean to the PMC values, with this window width}

  \item{fudge.factor}{numeric: (experimental) scale the PMC
  values by this value}
}
\value{
  A data frame of results
}
\description{
  Read atmospheric 14C calibration data file
}
\examples{
cal=read_calibration()
}
\author{
  B. Raymond, S. Wotherspoon
}

