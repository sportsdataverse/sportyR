% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cani-functions.R
\name{cani_plot_sport}
\alias{cani_plot_sport}
\title{Can I Plot Sport?}
\usage{
cani_plot_sport(sport_code)
}
\arguments{
\item{sport_code}{The case-insensitive sport name}
}
\value{
Nothing, but a message is sent to the console
}
\description{
Check to see if a sport can be plotted, and alert as to which league(s) are
plottable for the sport
}
\examples{
cani_plot_sport("basketball")
}
