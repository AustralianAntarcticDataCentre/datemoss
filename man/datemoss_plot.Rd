\name{plot_initial}
\alias{plot_date_estimates}
\alias{plot_fit_summary}
\alias{plot_growth_rate}
\alias{plot_initial}
\title{Helper plotting routines}
\usage{
  plot_initial(cal, segments, f0)

  plot_date_estimates(s)

  plot_fit_summary(segments, cal)

  plot_growth_rate(growthrate, s)
}
\arguments{
  \item{segments}{dataframe: the segment data, as returned
  by read_segment_data()}

  \item{f0}{numeric: vector of initial date estimates as
  returned by estimate_initial()}

  \item{cal}{dataframe: the calibration data, as returned
  by read_calibration()}

  \item{s}{dataframe: fitted date estimates, as returned by
  fit_dates()}

  \item{growthrate}{dataframe: fitted growth rate
  estimates, as returned by get_growth_rate()}
}
\description{
  Helper plotting routines
}

