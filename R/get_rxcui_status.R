#' Get Status Code for a RxCUI
#'
#' @param rx_cui Either a string or numeric RxNorm RxCUI to search for.
#' @param local_host Run query locally using RxNav-in-a-box with Docker?
#'
#' @return The RxCUI status code.
#' @export
#'
#' @examples
#' get_rxcui_status(861819)
get_rxcui_status <- function(rx_cui, local_host = FALSE) {
  # https://lhncbc.nlm.nih.gov/RxNav/APIs/api-RxNorm.getRxcuiHistoryStatus.html
  check_internet()
  url <- create_url(local_host, paste0("REST/rxcui/", rx_cui, "/historystatus"))
  check_null(parse_rxcui_status(httr::GET(url)))
}

parse_rxcui_status <- function(x) {
  if (!check_status(x)) return(NA_character_)
  httr::content(x, "parse")$rxcuiStatusHistory$metaData$status
}

#' Get New RxCUI from Remapped RxCUI
#'
#' @param rx_cui Either a string or numeric RxNorm RxCUI to search for.
#' @param local_host Run query locally using RxNav-in-a-box with Docker?
#'
#' @return If found, the remapped RxNorm RxCUI. Otherwise, \code{NA}.
#' @export
#'
#' @examples
#' get_remapped_rxcui(197523)
get_remapped_rxcui <- function(rx_cui, local_host = FALSE) {
  check_internet()
  url <- create_url(local_host, paste0("REST/rxcui/", rx_cui, "/historystatus"))
  check_null(parse_rxcui_remapped(httr::GET(url)))
}

parse_rxcui_remapped <- function(x) {
  if (!check_status(x)) return(NA_character_)
  httr::content(x, "parse")$rxcuiStatusHistory$derivedConcepts$remappedConcept[[1]]$remappedRxCui
}

#' Get New RxCUI from Non-dispensable RxCUI
#'
#' @param rx_cui Either a string or numeric RxNorm RxCUI to search for.
#' @param local_host Run query locally using RxNav-in-a-box with Docker?
#'
#' @return If found, the first related RxNorm RxCUI with a quantified factor. Otherwise, \code{NA}.
#' @export
#'
#' @examples
#' get_quantified_rxcui(1734477)
get_quantified_rxcui <- function(rx_cui, local_host = FALSE) {
  check_internet()
  url <- create_url(local_host, paste0("REST/rxcui/", rx_cui, "/historystatus"))
  check_null(parse_rxcui_quantified(httr::GET(url)))
}

parse_rxcui_quantified <- function(x) {
  if (!check_status(x)) return(NA_character_)
  httr::content(x, "parse")$rxcuiStatusHistory$derivedConcepts$quantifiedConcept[[1]]$quantifiedRxcui
}

#' @export
#'
#' @examples
#' get_scd_rxcui("861765")
get_scd_rxcui <- function(rx_cui, local_host = FALSE) {
  check_internet()
  url <- create_url(local_host, paste0("REST/rxcui/", rx_cui, "/historystatus"))
  check_null(parse_rxcui_scd(httr::GET(url)))
}

parse_rxcui_scd <- function(x) {
  if (!check_status(x)) return(NA_character_)
  httr::content(x, "parse")$rxcuiStatusHistory$derivedConcepts$scdConcept$scdConceptRxcui
}

