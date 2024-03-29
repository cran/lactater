% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/method-bsln-plus.R
\name{bsln_plus_helper}
\alias{bsln_plus_helper}
\title{Bsln+ helper}
\usage{
bsln_plus_helper(.data, data_augmented, fit, model, plus)
}
\arguments{
\item{.data}{The raw data.}

\item{data_augmented}{The augmented data from the model.}

\item{fit}{The fit you would like to use for finding the lactate values associated to each one of the lactate thresholds.}

\item{model}{The model chosen in \code{fit},}

\item{plus}{The plus value to be added to the baseline value.}
}
\value{
The results (intensity, lactate, heart rate) from the lactate threshold method.
}
\description{
For internal use only and it won't be exported.
}
\keyword{internal}
