% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/plate.R
\name{plate}
\alias{plate}
\alias{plate.well}
\alias{plate.list}
\alias{plate.data.frame}
\title{Create a plate}
\usage{
plate(x, ...)

\method{plate}{well}(x, ...)

\method{plate}{list}(x, ...)

\method{plate}{data.frame}(x, name, ...)
}
\arguments{
\item{x}{A \code{well} or list of wells}

\item{...}{additional arguments passed to their respective methods}

\item{name}{the 'key' in 'key = value' for plate contents}
}
\value{
a \code{plate}
}
\description{
Create a plate
}
