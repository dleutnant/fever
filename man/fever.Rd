% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/RcppExports.R
\name{fever}
\alias{fever}
\title{Filter events within a series of data}
\usage{
fever(x, threshold, window)
}
\arguments{
\item{x}{The vector to be filtered.}

\item{threshold}{The threshold. Filter rule applied is x(i) > threshold.}

\item{window}{The search window.}
}
\value{
A 2-column matrix with start and end indices.
}
\description{
The function loops through a vector and filters events whose
values exceed a given threshold within a given window.
}
