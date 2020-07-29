
#' Get Drug Names From RxCUI
#'
#' @param rx_cui Either a string or numeric RxNorm RxCUI to search for.
#'
#' @return A list of class \code{rxnorm} containing the following components:
#'
#' \item{name}{The drug name; \code{NULL} if not successful.}
#' \item{id}{The RxNorm RxCUI; \code{NULL} if not successful.}
#' \item{url}{The url used for accessing the REST API.}
#'
#' @export
#'
#' @examples
#' get_rx(1011485)
get_rx <- function(rx_cui) {
  # checks
  check_internet()
  # create url
  url <- paste0(base_url, "rxcui/", rx_cui)
  # get response
  res <- httr::GET(url)
  # check response status and return
  cnt <- parse_rx(res)
  class(cnt) <- "rxnorm"
  cnt
}

#' Get Drug Brand Names From RxCUI
#'
#' @param rx_cui Either a string or numeric RxNorm RxCUI to search for.
#'
#' @return A list of class \code{rxnorm} containing the following components:
#'
#' \item{name}{The brand name(s); \code{NULL} if not successful or not applicable.}
#' \item{id}{The RxNorm RxCUI(s) for the brand name; \code{NULL} if not successful or not applicable.}
#' \item{url}{The url used for accessing the REST API.}
#'
#' @export
#'
#' @examples
#' get_bn(1011485)
#' get_bn(7052)
get_bn <- function(rx_cui) {
  # checks
  check_internet()
  # create url
  url <- paste0(base_url, "rxcui/", rx_cui, "/related?tty=BN")
  # get response
  res <- httr::GET(url)
  # check response status and return
  cnt <- parse_bn(res)
  class(cnt) <- "rxnorm"
  cnt
}

#' Get WHO ATC/DDD Drug Class From RxCUI
#'
#' @param rx_cui Either a string or numeric RxNorm RxCUI to search for.
#' @param query_atc TODO
#'
#' @return A list of class \code{rxnorm} containing the following components:
#'
#' \item{name}{The brand name(s); \code{NULL} if not successful or not applicable.}
#' \item{id}{The RxNorm RxCUI(s) for the brand name; \code{NULL} if not successful or not applicable.}
#' \item{url}{The url used for accessing the REST API.}
#'
#' @export
#'
#' @examples
#' get_atc(1011485)
get_atc <- function(rx_cui, query_atc = c("none", "main", "thera", "pharma", "chem")) {
  # TODO query the WHO database for the specific drug categories
  # checks
  check_internet()
  # create url
  url <- paste0(atc_url, rx_cui, "&relaSource=ATC")
  # get response
  res <- httr::GET(url)
  # check response status and return
  cnt <- parse_atc(res)
  class(cnt) <- "rxnorm"
  cnt
}

