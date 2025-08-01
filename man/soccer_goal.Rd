% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features-soccer.R
\name{soccer_goal}
\alias{soccer_goal}
\title{Soccer Goal}
\usage{
soccer_goal(feature_thickness = 0, goal_width = 0, goal_depth = 0)
}
\arguments{
\item{feature_thickness}{The thickness of the goal}

\item{goal_width}{The interior width of the goal}

\item{goal_depth}{The depth to which the goal protrudes away from the back
edge of the goal line}
}
\value{
A data frame containing the bounding coordinates of the goal
}
\description{
The goal is located beyond each goal line. By rule, the goal posts must be
the same thickness as the goal line, and the posts must rest on the front
edge of the goal line
}
\details{
The line thickness will be uniform for all features on the pitch
}
\keyword{internal}
