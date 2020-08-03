
base_url <- "https://rxnav.nlm.nih.gov/REST/"
atc_url <- "https://rxnav.nlm.nih.gov/REST/rxclass/class/byRxcui.json?rxcui="
who_url <- "https://www.whocc.no/atc_ddd_index/?code="

check_internet <- function() {
  stopifnot("No internet connection" = curl::has_internet())
}

check_status <- function(x) {
  httr::status_code(x) == "200"
}

parse_rx <- function(x) {
  if (check_status(x)) {
    res <- httr::content(x, "parse")
  } else {
    res <- NULL
  }

  list(name = res$idGroup$name,
       id   = res$idGroup$rxnormId[[1]],
       url  = x$url)
}

parse_bn <- function(x) {
  if (check_status(x)) {
    res <- httr::content(x, "parse")
  } else {
    res <- NULL
  }

  list(name = unlist(lapply(res$relatedGroup$conceptGroup[[1]]$conceptProperties, function(x) x$name)),
       id   = unlist(lapply(res$relatedGroup$conceptGroup[[1]]$conceptProperties, function(x) x$rxcui)),
       url  = x$url)
}

parse_atc <- function(x, query) {
  if (!check_status(x)) {
    list(name = NULL,
         id   = NULL,
         url  = x$url)
  }

  res <- httr::content(x, "parse")
  atc <- res$rxclassDrugInfoList$rxclassDrugInfo[[1]]$rxclassMinConceptItem$classId

  if (query == "none") {
    list(name = atc,
         id   = NULL,
         url  = x$url)
  } else {
    list(name = tolower(get_who(atc, query)),
         id   = atc,
         url  = x$url)
  }
}

parse_who <- function(x, query) {
  cnt <- rvest::html_nodes(httr::content(x, "parse"), "b a")
  rvest::html_text(cnt[[translate_query(query)]])
}

translate_query <- function(x) {
  switch(x,
         first = 1,
         second = 2,
         third = 3,
         fourth = 4)
}
