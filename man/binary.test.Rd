% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/binary.test.R
\name{binary.test}
\alias{binary.test}
\title{Binary

checks if a vector is empty or not, return a factor with optional descriptive names}
\usage{
binary.test(x, i, newname, emptyname = F, notemptyname = T)
}
\arguments{
\item{x}{an data frame to be analyzed. The first column must be pidm.}

\item{i}{the numerical index of the column to be analyzed.}

\item{newname}{a string for the name of the new column}

\item{emptyname}{a string to be used as the factor level for empty values. Defaults to FALSE.}

\item{not}{emptyname a string to be used as the factor level for values that are not empty. Defaults to TRUE}
}
\value{
Returns a data frame with the first column of the original data frame (hopefully pidm) and the new column.
}
\description{
Binary

checks if a vector is empty or not, return a factor with optional descriptive names
}
