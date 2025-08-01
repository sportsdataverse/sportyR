% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features-football.R
\name{football_endzone}
\alias{football_endzone}
\title{Football Endzone}
\usage{
football_endzone(field_width = 0, endzone_length = 0)
}
\arguments{
\item{field_width}{The width of the field}

\item{endzone_length}{The length of the endzone}
}
\value{
A data frame of the bounding box of the endzone
}
\description{
The endzones are the area beyond the goal line. Any offensive player who is
in legal possession of the ball while in the endzone, or who catches the ball
in the endzone, scores a touchdown for their team. This area stretches from
the back field boundary to the edge of the goal line closest to the center of
the field
}
\keyword{internal}
