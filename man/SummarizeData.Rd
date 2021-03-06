% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/SummarizeData.R
\name{SummarizeData}
\alias{SummarizeData}
\title{Summarize Data Vector}
\usage{
SummarizeData(x, na.rm = FALSE)
}
\arguments{
\item{x}{a numeric vector}

\item{na.rm}{option to remove NAs. Default is FALSE.}
}
\value{
A numeric vector with length of x, percent NA, percent 0, mean,
  standard deviation, standard error, variance, and quantile values.
}
\description{
\code{SummarizeData} returns summary statistics for a numeric vector.
}
\examples{
SummarizeData(1:10)
SummarizeData(c(1:10, NA))
SummarizeData(c(1:10, NA), na.rm = TRUE)
}
