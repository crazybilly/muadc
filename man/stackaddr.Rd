% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stackaddr.R
\name{stackaddr}
\alias{stackaddr}
\title{Stack Names Based on Mailing Address}
\usage{
stackaddr(df, segment)
}
\arguments{
\item{df}{a data frame of a mailing list}

\item{segment}{an optional segment to segment by}
}
\value{
a data frame with rows that share an address and a segement stacked, reducing mailing numbers.
}
\description{
Stack Names Based on Mailing Address
}
\details{
segment is currently mandatory--it should be eventually be made optional. df must include at least the following columns: \itemize{
\item pidm
\item addr1
\item addr2
\item addr3
\item city
\item st
\item zip
\item keyline1
\item keyline2
\item primdnrind
\item prefname
\item cmname
\item frms
\item frmp
\item nckp
\item preffirstname

}
}
