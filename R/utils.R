
base_url <- "https://rxnav.nlm.nih.gov/REST/"

check_internet <- function() {
  stopifnot("No internet connection" = curl::has_internet())
}

check_status <- function(x) {
  httr::status_code(x) == "200"
}

response_content <- function(x) {
  if (check_status(x)) {
    res <- httr::content(x)
  } else {
    res <- NULL
  }

  list(name = res$idGroup$name,
       id   = res$idGroup$rxnormId[[1]],
       url  = x$url)
}
