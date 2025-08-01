% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{\%or\%}
\alias{\%or\%}
\title{Supply Default Values}
\usage{
spec_val \%or\% default_val
}
\arguments{
\item{spec_val}{The specified value to use (assuming it's provided)}

\item{default_val}{The value to use as the default}
}
\value{
A value dependent on if the left-hand value is provided, or the
default value specified on the right hand side
}
\description{
Set the default value when not provided in a list
}
\keyword{internal}
