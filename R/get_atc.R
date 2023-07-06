#' Get WHO ATC/DDD Drug Class From RxCUI
#'
#' @param rx_cui Either a string or numeric RxNorm RxCUI to search for.
#' @param query_atc Level to parse ATC code at. Options are "none" (default), "first", "second", "third", "fourth".
#' @param local_host Run query locally using RxNav-in-a-box with Docker?
#'
#' @return If \code{query_atc} is "none", the raw ATC code(s), otherwise the
#'   parsed ATC code(s); \code{NULL} if not successful.
#'
#' @export
#'
#' @examples
#' get_atc(861819)
#' get_atc(6809)
#' get_atc(861819, "first")
#' get_atc(861819, "second")
#' get_atc(861819, "third")
#' get_atc(861819, "fourth")
get_atc <- function(rx_cui, query_atc = c("none", "first", "second", "third", "fourth"), local_host = FALSE) {
  check_internet()
  url <- create_url(local_host, path_atc, rxcui = rx_cui, relasource = "ATCPROD")
  check_null(parse_atc(httr::GET(url), rx_cui, rela, match.arg(query_atc)))
}

path_atc <- "REST/rxclass/class/byRxcui.json"

parse_atc <- function(x, rx_cui, relasource, query) {
  if (!check_status(x)) {
    return(NA_character_)
  }

  res <- httr::content(x, "parse")$rxclassDrugInfoList$rxclassDrugInfo
  atc <- eval_atc_query(res, rx_cui, relasource)

  if (is.na(atc)) return(NA_character_)

  if (query == "none") {
    return(atc)
  }

  get_who(atc, query)
}

eval_atc_query <- function(from_api, rx_cui, relasource) {
  res <- filter_rxclassDrugInfo_rxcui(from_api, rx_cui) |>
    filter_rxClassDrugInfo_relaSource("ATCPROD")

  if (length(res) == 0) return(NA_character_)

  sapply(res, function(x) x$rxclassMinConceptItem$classId)
}

filter_rxclassDrugInfo_rxcui <- function(rxclassDrugInfo, rxcui) {
  rxclassDrugInfo[sapply(rxclassDrugInfo, function(x) x$minConcept$rxcui == rxcui)]
}

filter_rxClassDrugInfo_relaSource <- function(rxclassDrugInfo, relaSource) {
  rxclassDrugInfo[sapply(rxclassDrugInfo, function(x) x$relaSource == relaSource)]
}
