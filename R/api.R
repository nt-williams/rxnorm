
#' Obtain Drug Names From the NIM RxNorm API
#'
#' @param rxcui Either a string or numeric RxNorm RxCUI to search for.
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
#' search_rxcui(1011485)
search_rxcui <- function(rxcui) {
  # checks
  check_internet()
  # create url
  url <- paste0(base_url, "rxcui/", rxcui)
  # get response
  res <- httr::GET(url)
  # check response status and return
  cnt <- response_content(res)
  class(cnt) <- "rxnorm"
  cnt
}
