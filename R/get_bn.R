#' Get Drug Brand Names From RxCUI
#'
#' @param rx_cui Either a string or numeric RxNorm RxCUI to search for.
#' @param local_host Run query locally using RxNav-in-a-box with Docker?
#'
#' @return The brand name(s); \code{NA_character_} if not successful or not applicable.
#'
#' @export
#'
#' @examples
#' get_bn(1011485)
#' get_bn(7052)
get_bn <- function(rx_cui, local_host = FALSE) {
  check_internet()
  url <- create_url(local_host, paste0("REST/RxTerms/rxcui/", rx_cui, "/allinfo"))
  parse_bn(httr::GET(url))
}

parse_bn <- function(x) {
  if (!check_status(x)) return(NA_character_)
  res <- httr::content(x, "parse")$rxtermsProperties
  if (is.null(res)) return(NA_character_)
  res <- filter_rxtermsProperties(res, "brandName")
  if (res == "") return(NA_character_)
  res
}

filter_rxtermsProperties <- function(x, property) x[[property]]
