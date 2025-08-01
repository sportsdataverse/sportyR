% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features-basketball.R
\name{basketball_substitution_line}
\alias{basketball_substitution_line}
\title{Basketball Substitution Line}
\usage{
basketball_substitution_line(
  line_thickness = 0,
  substitution_line_width = 0,
  drawn_direction = ""
)
}
\arguments{
\item{line_thickness}{The thickness of the substitution line}

\item{substitution_line_width}{The width of the substitution line, from top
to bottom when viewing the plot in TV view}

\item{drawn_direction}{A string indicating which way, in an un-rotated plot,
the line should be drawn when looking at the plot in TV View}
}
\value{
A data frame containing the bounding coordinates of the substitution
line
}
\description{
The substitution line is where players checking into the game wait for a
stoppage. Lines drawn on the top of the court should be drawn in a top-down
direction, and lines on the bottom of the court should be drawn in the
bottom-up direction
}
\keyword{internal}
