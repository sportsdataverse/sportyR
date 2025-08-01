% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features-curling.R
\name{curling_centre_line}
\alias{curling_centre_line}
\title{Curling Centre Line}
\usage{
curling_centre_line(
  line_thickness = 0,
  tee_line_to_center = 0,
  centre_line_extension = 0
)
}
\arguments{
\item{line_thickness}{The thickness of the centre line}

\item{tee_line_to_center}{The distance between the tee lines. (See
\code{\link[=curling_tee_line]{curling_tee_line()}} for more information)}

\item{centre_line_extension}{The distance beyond the tee lines that the
centre line extends}
}
\value{
A data frame containing the bounding coordinates of the centre line
}
\description{
The centre line is the line that runs the full length of the curling sheet,
or the line \code{x = 0} in TV view
}
\keyword{internal}
