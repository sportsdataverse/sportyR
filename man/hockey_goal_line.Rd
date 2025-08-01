% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features-hockey.R
\name{hockey_goal_line}
\alias{hockey_goal_line}
\title{Hockey Goal Line}
\usage{
hockey_goal_line(
  rink_length = 0,
  rink_width = 0,
  feature_radius = 0,
  feature_thickness = 0,
  x_anchor = 0
)
}
\arguments{
\item{rink_length}{The length of the rink}

\item{rink_width}{The width of the rink}

\item{feature_radius}{The radius of the corner of the rink}

\item{feature_thickness}{The thickness of the goal line}

\item{x_anchor}{the \code{x} coordinate used as the anchor point of the goal
line}
}
\value{
A data frame containing the bounding coordinates of the goal line
}
\description{
The goal lines are the lines over which a puck must cross (within the goal
frame) in order to be considered a goal. Its line thickness should be given
by 'minor_line_thickness' as this is a minor line on the ice surface.
}
\details{
This draws the right-side goal line (in TV view), starting with its left
edge. This also accounts for a perfectly rectangular goal line if a user
supplies a value that necessitates one. The line is rectangular in shape with
rounded ends, and usually red in color
}
\keyword{internal}
