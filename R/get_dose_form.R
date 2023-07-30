#' Get Dose Form From RxCUI
#'
#' @param rx_cui Either a string or numeric RxNorm RxCUI to search for.
#' @param local_host Run query locally using RxNav-in-a-box with Docker?
#'
#' @return The dose form; \code{NULL} if not successful.
#'
#' @export
#'
#' @examples
#' get_dose_form(1011485)
get_dose_form <- function(rx_cui, local_host = FALSE) {
  check_internet()
  url <- create_url(local_host, paste0("REST/rxcui/", rx_cui, "/historystatus"))
  check_null(parse_dose_form(httr::GET(url)))
}

parse_dose_form <- function(x) {
  if (!check_status(x)) return(NA_character_)
  out <- sapply(httr::content(x, "parse")$rxcuiStatusHistory$definitionalFeatures$doseFormConcept,
                function(x) x$doseFormName)
  unique(unlist(out))
}
