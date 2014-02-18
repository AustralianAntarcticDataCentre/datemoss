\name{get_growth_rate}
\alias{get_growth_rate}
\title{Extract growth rate estimates}
\usage{
  get_growth_rate(s, segments)
}
\arguments{
  \item{segments}{dataframe: the segment data, as returned
  by read_segment_data()}

  \item{s}{dataframe: fitted date estimates, as returned by
  fit_dates()}
}
\value{
  A dataframe of growth rate estimates (in units of
  mm/year)
}
\description{
  Extract growth rate estimates
}

