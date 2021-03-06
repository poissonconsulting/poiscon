% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/diel.R
\name{diel_period}
\alias{diel_period}
\title{Diel period}
\usage{
diel_period(x, Lat = 49.49, Long = -117.3)
}
\arguments{
\item{x}{date/time}

\item{Lat}{a numeric element indicating the latitude.}

\item{Long}{a numeric element indicating the longitude.}
}
\value{
A factor with the levels Day and Night.
}
\description{
Calculates whether a date/time is Day or Night based on the
latitude and longitude. The default is for the Poisson Consulting Ltd
offices.
}
