% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features-soccer.R
\name{soccer_corner_defensive_marks}
\alias{soccer_corner_defensive_marks}
\title{Soccer Corner Defensive Mark}
\usage{
soccer_corner_defensive_marks(
  feature_thickness = 0,
  is_touchline = FALSE,
  is_goal_line = FALSE,
  depth = 0,
  separation_from_line = 0
)
}
\arguments{
\item{feature_thickness}{The thickness of the corner defensive marks}

\item{is_touchline}{A boolean indicating whether or not the corner defensive
marks should be along the touchline}

\item{is_goal_line}{A boolean indicating whether or not the corner defensive
marks should be along the goal line}

\item{depth}{The depth that the mark extends out of play}

\item{separation_from_line}{The distance from the back edge of the goal line
to the interior edge of the corner defensive mark}
}
\value{
A data frame containing the bounding coordinates of the corner
defensive marks
}
\description{
The corner defensive marks on the pitch are typically located 9.15 meters (10
yards) from the corner of the pitch. Defenders should be beyond these marks
(either more towards the goal or more towards the halfway line) during corner
kicks
}
\details{
The marks should be outside the field of play

The line thickness will be uniform for all features on the pitch
}
\keyword{internal}
