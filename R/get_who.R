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

  check_null(check_common(out))
}

who_url <- "https://www.whocc.no/atc_ddd_index/?code="

parse_who <- function(x, query) {
  cnt <- rvest::html_nodes(httr::content(x, "parse"), "b a")
  tolower(rvest::html_text(cnt[[translate_query(query)]]))
}

translate_query <- function(x) {
  switch(x,
         first = 1,
         second = 2,
         third = 3,
         fourth = 4)
}

#' Subset WHO ATC Codes
#'
#' Subset ATC codes into the minimal code based on the level of query.
#'
#' @param atc WHO ATC code.
#' @param query The level to subset the code at. Options are "first" (default), "second", "third", "fourth".
#'
#' @return The subsetted code.
#' @export
#'
#' @examples
#' subset_atc("R06AE", "first")
#' subset_atc("R06AE", "second")
subset_atc <- function(atc, query = c("first", "second", "third", "fourth")) {
  i <- switch(match.arg(query),
              first = c(1, 1),
              second = c(1, 3),
              third = c(1, 4),
              fourth = c(1, 5))
  check_common(substr(atc, i[1], i[2]))
}
