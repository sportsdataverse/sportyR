% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/coordinate-transformations.R
\name{rotate_coords}
\alias{rotate_coords}
\title{Rotate Coordinates}
\usage{
rotate_coords(df, angle = 90)
}
\arguments{
\item{df}{The data frame to rotate. It must have \code{x} and \code{y}
columns}

\item{angle}{the angle (in degrees) through which to rotate the coordinates}
}
\value{
The rotated data frame
}
\description{
Perform a mathematical rotation about (0, 0) of coordinates. This rotation is
given as x' = x \* cos(theta) - y \* sin(theta) y' = x \* sin(theta) + y \*
cos(theta)
}
\examples{
rotate_coords(data.frame(x = 0, y = 1))

}
\keyword{internal}
