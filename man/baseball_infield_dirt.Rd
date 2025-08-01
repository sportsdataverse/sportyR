% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features-baseball.R
\name{baseball_infield_dirt}
\alias{baseball_infield_dirt}
\title{Baseball Infield Dirt}
\usage{
baseball_infield_dirt(
  home_plate_circle_radius = 0,
  foul_line_to_foul_grass = 0,
  pitchers_plate_distance = 0,
  infield_arc_radius = 0
)
}
\arguments{
\item{home_plate_circle_radius}{The radius of the circle around home plate}

\item{foul_line_to_foul_grass}{The distance from the outer edge of the foul
line to the inner edge of the grass in foul territory}

\item{pitchers_plate_distance}{The distance from the back tip of home plate
to the front edge of the pitcher's plate}

\item{infield_arc_radius}{The distance from the front edge of the pitcher's
plate to the back of the infield dirt}
}
\value{
A data frame that comprises the entirety of the infield dirt and dirt
circles around home plate
}
\description{
The dirt that comprises the infield. This includes the base paths, infield
arc, and home plate circle.
}
\details{
The home plate circle may be drawn over in other shapes as needed (example:
Detroit's Comerica Park has a home plate shaped dirt area as the home plate
"circle")
}
\keyword{internal}
