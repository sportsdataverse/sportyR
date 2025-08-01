% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/utils.R
\name{load_default_parameters}
\alias{load_default_parameters}
\title{Load Default Surface Parameters}
\usage{
load_default_parameters(
  league = "",
  display_range = "full",
  court_updates = list(),
  field_updates = list(),
  pitch_updates = list(),
  rink_updates = list(),
  sheet_updates = list(),
  color_updates = list(),
  rotation = 0,
  x_trans = 0,
  y_trans = 0,
  court_units = NULL,
  field_units = NULL,
  pitch_units = NULL,
  rink_units = NULL,
  sheet_units = NULL,
  xlims = NULL,
  ylims = NULL
)
}
\arguments{
\item{league}{The league to load into the global environment}

\item{display_range}{The display range to load into the global environment}

\item{court_updates}{The default \code{court_updates} to load into the global
environment. This will default to an empty list}

\item{field_updates}{The default \code{field_updates} to load into the global
environment. This will default to an empty list}

\item{pitch_updates}{The default \code{pitch_updates} to load into the global
environment. This will default to an empty list}

\item{rink_updates}{The default \code{rink_updates} to load into the global
environment. This will default to an empty list}

\item{sheet_updates}{The default \code{sheet_updates} to load into the global
environment. This will default to an empty list}

\item{color_updates}{The default \code{color_updates} to load into the global
environment. This will default to an empty list}

\item{rotation}{The default rotation to load into the global environment.
This will default to \code{0}}

\item{x_trans}{The default translation in the \code{x} direction to load into
the global environment. This will default to \code{0}}

\item{y_trans}{The default translation in the \code{y} direction to load into
the global environment. This will default to \code{0}}

\item{court_units}{The default units for a court-like surface. The default
will be \code{NULL}}

\item{field_units}{The default units for a field-like surface. The default
will be \code{NULL}}

\item{pitch_units}{The default units for a pitch-like surface. The default
will be \code{NULL}}

\item{rink_units}{The default units for a rink-like surface. The default will
be \code{NULL}}

\item{sheet_units}{The default units for a sheet-like surface. The default
will be \code{NULL}}

\item{xlims}{The default limits on the plot to use in the \code{x} direction.
The default will be \code{NULL}}

\item{ylims}{The default limits on the plot to use in the \code{y} direction.
The default will be \code{NULL}}
}
\value{
Nothing, but environment variables should be set
}
\description{
Load default parameters for a specified league. This should only be used when
debugging the package
}
\keyword{internal}
