% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features-football.R
\name{football_directional_arrow}
\alias{football_directional_arrow}
\title{Football Directional Arrow (For Yard Lines)}
\usage{
football_directional_arrow(arrow_base = 0, arrow_length = 0)
}
\arguments{
\item{arrow_base}{The length of the base of the arrow}

\item{arrow_length}{The length of the arrow from tip to base}
}
\value{
A data frame of the bounding coordinates of the directional arrow
}
\description{
The directional arrows point towards the nearest goal line from the yardage
marker they are closest to. These arrows are described by their base (which
runs parallel to the goal line) and their length, which extends from the tip
to the base
}
\keyword{internal}
