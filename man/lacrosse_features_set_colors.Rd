% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/geom-lacrosse.R
\name{lacrosse_features_set_colors}
\alias{lacrosse_features_set_colors}
\title{Lacrosse Feature Colors}
\usage{
lacrosse_features_set_colors(
  plot_background = NULL,
  field_apron = "#195f0c",
  defensive_zone = "#195f0c",
  neutral_zone = "#195f0c",
  offensive_zone = "#195f0c",
  team_a_bench = "#a5acaf4d",
  team_b_bench = "#a5acaf4d",
  team_a_penalty_box = "#a5acaf4d",
  team_b_penalty_box = "#a5acaf4d",
  off_field_officials_box = "#a5acaf",
  boards = "#ffa500",
  end_line = "#ffffff",
  sideline = "#ffffff",
  center_line = "#ffffff",
  wing_line = "#ffffff",
  restraining_line = "#ffffff",
  defensive_area_line = "#ffffff",
  goal_line = "#ffffff",
  referee_crease = "#ffffff",
  referee_crease_fill = "#195f0c",
  goal_circle = "#ffffff",
  goal_circle_fill = "#195f0c",
  goal_arc = "#ffffff",
  goal_fan = "#ffffff",
  goal_fan_hash_mark = "#ffffff",
  goal_mouth_hash_mark = "#ffffff",
  goal_mouth = "#ffffff",
  below_goal_marking = "#ffffff",
  goal_frame = "#ffa500",
  goal_net = "#a5acaf4d",
  center_circle = "#ffffff",
  center_face_off_marker = "#ffffff",
  corner_face_off_marker = "#ffffff",
  change_area_outline = "#ffffff",
  change_area_fill = "#195f0c"
)
}
\arguments{
\item{plot_background}{A hexadecimal string representing the color to use for
this feature}

\item{field_apron}{A hexadecimal string representing the color to use for
this feature}

\item{defensive_zone}{A hexadecimal string representing the color to use for
this feature}

\item{neutral_zone}{A hexadecimal string representing the color to use for
this feature}

\item{offensive_zone}{A hexadecimal string representing the color to use for
this feature}

\item{team_a_bench}{A hexadecimal string representing the color to use for
this feature}

\item{team_b_bench}{A hexadecimal string representing the color to use for
this feature}

\item{team_a_penalty_box}{A hexadecimal string representing the color to use
for this feature}

\item{team_b_penalty_box}{A hexadecimal string representing the color to use
for this feature}

\item{off_field_officials_box}{A hexadecimal string representing the color to
use for this feature}

\item{boards}{A hexadecimal string representing the color to use for this
feature}

\item{end_line}{A hexadecimal string representing the color to use for this
feature}

\item{sideline}{A hexadecimal string representing the color to use for this
feature}

\item{center_line}{A hexadecimal string representing the color to use for
this feature}

\item{wing_line}{A hexadecimal string representing the color to use for this
feature}

\item{restraining_line}{A hexadecimal string representing the color to use
for this feature}

\item{defensive_area_line}{A hexadecimal string representing the color to use
for this feature}

\item{goal_line}{A hexadecimal string representing the color to use for this
feature}

\item{referee_crease}{A hexadecimal string representing the color to use for
this feature}

\item{referee_crease_fill}{A hexadecimal string representing the color to use
for this feature}

\item{goal_circle}{A hexadecimal string representing the color to use for
this feature}

\item{goal_circle_fill}{A hexadecimal string representing the color to use
for this feature}

\item{goal_arc}{A hexadecimal string representing the color to use for this
feature}

\item{goal_fan}{A hexadecimal string representing the color to use for this
feature}

\item{goal_fan_hash_mark}{A hexadecimal string representing the color to use
for this feature}

\item{goal_mouth_hash_mark}{A hexadecimal string representing the color to
use for this feature}

\item{goal_mouth}{A hexadecimal string representing the color to use for this
feature}

\item{below_goal_marking}{A hexadecimal string representing the color to use
for this feature}

\item{goal_frame}{A hexadecimal string representing the color to use for this
feature}

\item{goal_net}{A hexadecimal string representing the color to use for this
feature}

\item{center_circle}{A hexadecimal string representing the color to use for
this feature}

\item{center_face_off_marker}{A hexadecimal string representing the color to
use for this feature}

\item{corner_face_off_marker}{A hexadecimal string representing the color to
use for this feature}

\item{change_area_outline}{A hexadecimal string representing the color to use
for this feature}

\item{change_area_fill}{A hexadecimal string representing the color to use
for this feature}
}
\value{
A list of hexadecimal colors to use to color the features on the
resulting plot
}
\description{
Set the colors to be used for the plot. The values provided in the arguments
are the defaults, and, where specified, are the rule-book specified values.
}
\details{
Hexadecimal values are the passed vales to this function by default, but it
is also possible to use string-named values (e.g. \code{"dodgerblue"}) when
specifying.
}
\keyword{internal}
