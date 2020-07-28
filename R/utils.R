
base_url <- "https://rxnav.nlm.nih.gov/REST/"
atc_url <- "https://rxnav.nlm.nih.gov/REST/rxclass/class/byRxcui.json?rxcui="

check_internet <- function() {
  stopifnot("No internet connection" = curl::has_internet())
}

check_status <- function(x) {
  httr::status_code(x) == "200"
}

parse_rx <- function(x) {
  if (check_status(x)) {
    res <- httr::content(x)
  } else {
    res <- NULL
  }

  list(name = res$idGroup$name,
       id   = res$idGroup$rxnormId[[1]],
       url  = x$url)
}

parse_bn <- function(x) {
  if (check_status(x)) {
    res <- httr::content(x)
  } else {
    res <- NULL
  }

  list(name = unlist(lapply(res$relatedGroup$conceptGroup[[1]]$conceptProperties, function(x) x$name)),
       id   = unlist(lapply(res$relatedGroup$conceptGroup[[1]]$conceptProperties, function(x) x$rxcui)),
       url  = x$url)
}

parse_atc <- function(x) {
  if (check_status(x)) {
    res <- httr::content(x)
  } else {
    res <- NULL
  }

  list(name = res$rxclassDrugInfoList$rxclassDrugInfo[[1]]$rxclassMinConceptItem$classId,
       id   = NULL,
       url  = x$url)
}
