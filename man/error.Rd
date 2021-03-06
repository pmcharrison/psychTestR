% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/state.R
\name{error}
\alias{error}
\title{Error}
\usage{
error(state)
}
\arguments{
\item{state}{Participant's \code{state} object.}
}
\description{
Accesses the \code{error} slot of the participant's \code{state} object.
This slot takes values of \code{NULL} or a character scalar.
When not \code{NULL}, the server will display its value as an error message
to the participant.
}
