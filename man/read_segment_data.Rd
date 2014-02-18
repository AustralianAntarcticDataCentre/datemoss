\name{read_segment_data}
\alias{read_segment_data}
\title{Read sample data file}
\usage{
  read_segment_data(filename, sheetname = NULL,
    sample_column_names = c("Lab ID", "Moss species", "Sample ID"),
    columns_to_return = NULL)
}
\arguments{
  \item{filename}{string: the Excel file to read}

  \item{sheetname}{string: the name of the worksheet to
  read. If NULL, all worksheets are checked and the first
  one with a header row is used}

  \item{sample_column_names}{string: the column names that
  are used to identify data rows within the datasheet}

  \item{columns_to_return}{string: the columns to extract
  from the datasheet. If NULL, any column that does not
  have an NA header value is returned}
}
\value{
  A data frame of results
}
\description{
  Read sample data file
}

