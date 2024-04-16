create_url <- function(local, path, ...) {
  query <- list(...)
  url <- list()
  class(url) <- "url"

  url$scheme <- ifelse(local, "http", "https")
  url$hostname <- ifelse(local, "localhost", "rxnav.nlm.nih.gov")
  url$port <- NULL
  if (local) url$port <- "4000"
  url$path <- path

  if (length(query) != 0) {
    url$query <- list()
    for (x in names(query)) {
      url$query[[x]] <- query[[x]]
    }
  }
  httr::build_url(url)
}

check_internet <- function() {
  stopifnot("No internet connection" = curl::has_internet())
}

check_status <- function(x) {
  httr::status_code(x) == "200"
}

check_common <- function(who) {
  unique(unlist(who))
}

check_null <- function(x) {
  if (is.null(x)) return(NA_character_)
  x
}
