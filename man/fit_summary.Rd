\name{fit_summary}
\alias{fit_summary}
\title{Summarize fitted date estimates}
\usage{
  fit_summary(s, cal, segments = NULL)
}
\arguments{
  \item{s}{dataframe: fitted date estimates, as returned by
  fit_dates()}

  \item{cal}{dataframe: the calibration data, as returned
  by read_calibration()}

  \item{segments}{dataframe: the segment data, as returned
  by read_segment_data()}
}
\value{
  A dataframe. If segments is non-null, the summary values
  are added to that data frame
}
\description{
  Summarize fitted date estimates
}

