
#' Get Drug Names From the NLM RxNorm API
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
  cnt <- response_content(res)
  class(cnt) <- "rxnorm"
  cnt
}
