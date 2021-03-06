% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/GC.R
\name{GC}
\alias{GC}
\alias{GC_activity}
\alias{GC_metrics}
\alias{GC_mmvs}
\title{GoldenCheetah (>v3.3) interface.}
\usage{
GC_activity(athlete.name, activity, port = 12021, format = TRUE)

GC_metrics(athlete.name, date.rng = NULL, port = 12021)

GC_mmvs(type = "watts", date.rng = NULL, port = 12021)
}
\arguments{
\item{athlete.name}{character; athlete of interest in the GoldenCheetah data
directory. Typically of the form "First Last".}

\item{activity}{character; file path to a GoldenCheetah activity(.json) file.
Typically located in "~/.goldencheetah/Athlete Name/activities/".}

\item{port}{http server port number. 12021 unless deliberatley changed in the
httpserver.ini file.}

\item{format}{format activity data to an object of class "cycleRdata".
Ensures compatibility with other functions in this package -- see
\code{\link{read_ride}}.}

\item{date.rng}{a vector of length two that can be converted to an object of
class \code{"Date"} via \code{\link{as.Date}}. Must be specified for
\code{GC_mmvs}; optional for \code{GC_metrics}.}

\item{type}{the type of maximal mean values to return. See details.}
}
\description{
Functions for interfacing R with
\href{https://github.com/GoldenCheetah/GoldenCheetah}{GoldenCheetah}.
Requires the \code{RCurl} package to be installed.
}
\details{
As of GoldenCheetah (GC) version 3.3, the application is ran with a
background restful web service api to ease integration with external analysis software
(such as R). When an instance of GoldenCheetah is running, or the application
is initiated from the command line with the '--server' option, these
functions can be used to interface with athlete data. Relevant documentation
can be found
\href{https://github.com/GoldenCheetah/GoldenCheetah/wiki/UG_Special-Topics_REST-API-documentation}{here}.

\code{GC_activity} behaves similarly to \code{\link{read_ride}} functions in
this package, importing data from saved GC .json files.

\code{GC_metrics} returns summary metrics for either: all available rides if
\code{date.rng = NULL}; or rides within a specified date range if dates are
given.

\code{GC_mmvs} retuns best maximal mean values for data specified in the
\code{type} argument. Possible options for \code{type} are: "watts", "hr",
"cad", "speed", "nm", "vam", "xPower", or "NP". See also \code{\link{mmv}}.
}

