% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{get_rx}
\alias{get_rx}
\title{Get Drug Names From RxCUI}
\usage{
get_rx(rx_cui)
}
\arguments{
\item{rx_cui}{Either a string or numeric RxNorm RxCUI to search for.}
}
\value{
The drug name; \code{NULL} if not successful.
}
\description{
Get Drug Names From RxCUI
}
\examples{
get_rx(1011485)
}
