
#' Get Drug Names From RxCUI
#'
#' @param rx_cui Either a string or numeric RxNorm RxCUI to search for.
#'
#' @return The drug name; \code{NULL} if not successful.
#'
#' @export
#'
#' @examples
#' get_rx(1011485)
get_rx <- function(rx_cui) {
  check_internet()
  parse_rx(httr::GET(paste0(base_url, "rxcui/", rx_cui)))
}

#' Get Drug Brand Names From RxCUI
#'
#' @param rx_cui Either a string or numeric RxNorm RxCUI to search for.
#'
#' @return The brand name(s); \code{NULL} if not successful or not applicable.
#'
#' @export
#'
#' @examples
#' get_bn(1011485)
#' get_bn(7052)
get_bn <- function(rx_cui) {
  check_internet()
  parse_bn(httr::GET(paste0(base_url, "rxcui/", rx_cui, "/related?tty=BN")))
}

#' Get WHO ATC/DDD Drug Class From RxCUI
#'
#' @param rx_cui Either a string or numeric RxNorm RxCUI to search for.
#' @param query_atc Level to parse ATC code at. Options are "none" (default), "first",
#'   "second", "third", "fourth".
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
get_atc <- function(rx_cui, query_atc = c("none", "first", "second", "third", "fourth")) {
  check_internet()
  parse_atc(httr::GET(paste0(atc_url, rx_cui, "&relaSource=ATC")), rx_cui, match.arg(query_atc))
}

#' Parse WHO ATC/DDD Drug Class
#'
#' @param atc An WHO ATC/DDD ATC code.
#' @param query Level to parse ATC code at. Options are "first" (default),
#'   "second", "third", "fourth".
#' @param subsetted Has ATC/DDD query level subsetting already occurred? The default is FALSE.
#'
#' @return The parsed ATC code.
#' @export
#'
#' @examples
#' get_who("R06AE")
get_who <- function(atc,
                    query = c("first", "second", "third", "fourth"),
                    subsetted = FALSE) {
  if (is.null(atc) || is.na(atc)) {
    return(NA_character_)
  }

  q <- match.arg(query)

  if (isFALSE(subsetted)) {
    atc <- subset_atc(atc, q)
  }

  out <- lapply(paste0(who_url, atc, "&showdescription=no"), function(x) {
    parse_who(httr::GET(x), q)
  })

  check_common(out)
}

get_history <- function(url, concept = NULL) {
  check_internet()
  parse_history(httr::GET(paste0(url, "/historystatus")), concept)
}
