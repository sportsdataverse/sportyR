% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features-hockey.R
\name{hockey_odzone_faceoff_circle}
\alias{hockey_odzone_faceoff_circle}
\title{Hockey Non-Center Faceoff Circle}
\usage{
hockey_odzone_faceoff_circle(
  feature_radius = 0,
  feature_thickness = 0,
  hashmark_width = 0,
  hashmark_ext_spacing = 0
)
}
\arguments{
\item{feature_radius}{The radius of the faceoff circle}

\item{feature_thickness}{The thickness of the line of the non-centered
faceoff circle}

\item{hashmark_width}{The width of the hashmarks on the exterior of the
non-centered faceoff circle}

\item{hashmark_ext_spacing}{The external spacing between the hashmarks' outer
edges}
}
\value{
A data frame containing the bounding coordinates of the non-centered
faceoff circle
}
\description{
The non-centered faceoff circles are located in the offensive and defensive
zones of the ice, with one on each side of the x-axis when viewing the rink
in TV view. These circles differ from the center faceoff circle because they
have hash marks that extend towards the boards on each side of the circle
}
\details{
The non-centered faceoff circles are where faceoffs are taken after an icing
call or to start a powerplay. They differ from the center ice faceoff circle
because there are adjoining hash marks on these circles. It is also a
different color than the center ice faceoff circle, and the spot in the
center of it varies in size and form. Its line thickness should be given by
'minor_line_thickness' as this is a minor line on the ice surface
}
\keyword{internal}
