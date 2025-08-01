% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/features-football.R
\name{football_team_bench_area_outline}
\alias{football_team_bench_area_outline}
\title{Football Team Bench Area (Outline)}
\usage{
football_team_bench_area_outline(
  restricted_area_length = 0,
  restricted_area_width = 0,
  coaching_box_length = 0,
  coaching_box_width = 0,
  team_bench_length_field_side = 0,
  team_bench_length_back_side = 0,
  team_bench_width = 0,
  feature_thickness = 0
)
}
\arguments{
\item{restricted_area_length}{The length of the restricted area}

\item{restricted_area_width}{The width of the restricted area}

\item{coaching_box_length}{The length of the coaching box}

\item{coaching_box_width}{The width of the coaching box}

\item{team_bench_length_field_side}{The length of the side of the team bench
closest to the field}

\item{team_bench_length_back_side}{The length of the side of the team bench
furthest from the field}

\item{team_bench_width}{The width of the team bench}

\item{feature_thickness}{The thickness of the outline of the team bench area}
}
\value{
A data frame containing the bounding coordinates of the team bench
area's outline
}
\description{
The outline of the team bench area runs beyond the team bench, but is inside
of any field border that may run behind the team bench area (see
\code{\link[=football_field_border]{football_field_border()}} for more information on this feature)
}
\keyword{internal}
