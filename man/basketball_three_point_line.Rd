% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features-basketball.R
\name{basketball_three_point_line}
\alias{basketball_three_point_line}
\title{Basketball Three-Point Line}
\usage{
basketball_three_point_line(
  basket_center_to_baseline = 0,
  basket_center_to_corner_three = 0,
  line_thickness = 0,
  three_point_line_radius = 0
)
}
\arguments{
\item{basket_center_to_baseline}{The distance from the center of the basket
ring to the inner edge of the baseline}

\item{basket_center_to_corner_three}{The distance from the center of the
basket ring to the outer edge of the three-point line in the corner in the
court's specified units}

\item{line_thickness}{The thickness of the three-point line}

\item{three_point_line_radius}{The outer radius of the arc portion of the
three-point line}
}
\value{
A data frame of the bounding coordinates of the three-point line
}
\description{
An arc on the court, behind which any made basket counts as three points and
in front of which, any made basket will count as two points (see
\code{\link[=basketball_two_point_range]{basketball_two_point_range()}} for more information).
}
\details{
Start by getting the distance from the center of the basket to a corner
three-point shot. This is referred to as \code{start_y}

Next, get the starting angle with which to trace out the two-point range.
Taking the distance start_y to be a y coordinate, and the radius of the arc
of the three-point line to be a radius, we the sine of the starting angle is
given as \code{start_y / three_point_arc_radius}

As the TV-right angle of the start of the arc is what's drawn here, the
starting and ending angles need to be adjusted relative to 1 radian (the arc
opens to the right, like a \code{(} character)

The starting angle is therefore given as \code{1 - angle}, and the ending
angle is \code{1 + angle}
}
\keyword{internal}
