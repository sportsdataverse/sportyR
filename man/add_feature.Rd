% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plot-helpers.R
\name{add_feature}
\alias{add_feature}
\title{Add Feature to Plot}
\usage{
add_feature(
  g,
  x_anchor,
  y_anchor,
  feature_df,
  feature_color,
  feature_outline_color = "#ffffff00",
  reflect_x = FALSE,
  reflect_y = FALSE,
  x_trans = 0,
  y_trans = 0,
  rotation = 0,
  group = NULL
)
}
\arguments{
\item{g}{The \code{ggplot2} instance onto which the feature will be added}

\item{x_anchor}{The anchor point along the \code{x} axis for the feature}

\item{y_anchor}{The anchor point along the \code{y} axis for the feature}

\item{feature_df}{The data frame containing the points to add to the feature}

\item{feature_color}{A hexadecimal string with which to color the feature
once added to the plot}

\item{feature_outline_color}{A hexadecimal string with which to color the
outline of the feature added to the plot. The default value is
\code{"#ffffff00"}, which is white with a 0\% alpha value. This results in
no outline being added, which is usually desirable, but may be overwritten
to prevent "seams" from appearing in the resulting plot}

\item{reflect_x}{Whether or not to reflect the feature over the \code{x} axis}

\item{reflect_y}{Whether or not to reflect the feature over the \code{y} axis}

\item{group}{A grouping to pass along to \code{\link[ggplot2:aes]{ggplot2::aes()}}. This is used
for speed in the NFL and NCAA Football plotting functions}
}
\value{
A \code{ggplot2} instance with the feature added to it
}
\description{
Add a surface's feature to a \code{ggplot2} instance
}
\keyword{internal}
