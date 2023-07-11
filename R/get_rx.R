#' Get Drug Names From RxCUI
#'
#' @param rx_cui Either a string or numeric RxNorm RxCUI to search for.
#' @param local_host Run query locally using RxNav-in-a-box with Docker?
#'
#' @return The drug name; \code{NULL} if not successful.
#'
#' @export
#'
#' @examples
#' get_rx(1011485)
get_rx <- function(rx_cui, local_host = FALSE) {
  check_internet()
  url <- create_url(local_host, paste0("REST/rxcui/", rx_cui, "/historystatus"))
  check_null(parse_rx(httr::GET(url)))
}

parse_rx <- function(x) {
  if (!check_status(x)) return(NA_character_)
  httr::content(x, "parse")$rxcuiStatusHistory$attributes$name
}
