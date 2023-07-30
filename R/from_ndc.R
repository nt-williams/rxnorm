#' Find Active RxNorm From NDC
#'
#' @param ndc An NDC code as a character string.
#' @param local_host Run query locally using RxNav-in-a-box with Docker?
#'
#' @return If found, the active RxNorm RxCUI. Otherwise, \code{NA}.
#' @export
#'
#' @examples
#' from_ndc("00002143301")
from_ndc <- function(ndc, local_host = FALSE) {
  check_internet()
  url <- create_url(local_host, path_ndc, ndc = ndc)
  parse_ndc(httr::GET(url), "rxcui")
}

path_ndc <- "REST/ndcstatus"

parse_ndc <- function(x, param) {
  if (!check_status(x)) return(NA_character_)
  res <- filter_ndcStatus(httr::content(x, "parse"), param)
  if (res == "") return(NA_character_)
  res
}

filter_ndcStatus <- function(x, param) x$ndcStatus[[param]]
