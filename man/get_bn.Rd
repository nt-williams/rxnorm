% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/api.R
\name{get_bn}
\alias{get_bn}
\title{Get Drug Brand Names From RxCUI}
\usage{
get_bn(rx_cui)
}
\arguments{
\item{rx_cui}{Either a string or numeric RxNorm RxCUI to search for.}
}
\value{
The brand name(s); \code{NULL} if not successful or not applicable.
}
\description{
Get Drug Brand Names From RxCUI
}
\examples{
get_bn(1011485)
get_bn(7052)
}
