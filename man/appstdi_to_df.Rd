% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/appstdi-to-df.R
\name{appstdi_to_df}
\alias{appstdi_to_df}
\title{APPSTDI to Data Frame}
\usage{
appstdi_to_df(appstdi)
}
\arguments{
\item{appstdi}{a character vector read in from an appstdi.lis file (via \code{readr::read_lines()})}
}
\value{
a data frame with one line per person. Multiple degree lines are dropped.
}
\description{
APPSTDI to Data Frame
}
