% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features-hockey.R
\name{hockey_goaltenders_restricted_area}
\alias{hockey_goaltenders_restricted_area}
\title{Hockey Goaltender's Restricted Area (Trapezoid)}
\usage{
hockey_goaltenders_restricted_area(
  rink_length = 0,
  feature_thickness = 0,
  short_base_width = 0,
  long_base_width = 0,
  x_anchor = 0
)
}
\arguments{
\item{rink_length}{The length of the rink}

\item{feature_thickness}{The thickness of the lines used to draw the
goaltender's restricted area}

\item{short_base_width}{The width of the base nearest the center line}

\item{long_base_width}{The width of the base nearest the boards behind the
goal}

\item{x_anchor}{the \code{x} coordinate used as the anchor point of the goal
line}
}
\value{
A data frame containing the bounding coordinates of the goaltender's
restricted area
}
\description{
The goaltender's restricted area marks where a goaltender is legally allowed
to handle the puck behind the net. This is often referred to as "the
trapezoid" as it is trapezoidal in shape. Its line thickness should be given
by 'minor_line_thickness' as this is a minor line on the ice surface
}
\details{
NOTE: This is not a requirement in all leagues, and may be omitted via the
"has_trapezoid" key in the \code{rink_params} passed to \code{geom_{league}}

This draws the goaltender's restricted area on the right side (in TV view) of
the ice surface. The figure is composed of lines that outline a trapezoid in
shape, and is usually red in color
}
\keyword{internal}
