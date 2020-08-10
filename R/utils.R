
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

  return(res$idGroup$name)
}

parse_bn <- function(x) {
  if (check_status(x)) {
    res <- httr::content(x, "parse")
  } else {
    res <- NULL
  }

  return(unlist(lapply(res$relatedGroup$conceptGroup[[1]]$conceptProperties, function(x) x$name)))
}

parse_atc <- function(x, rx_cui, query) {
  if (!check_status(x)) {
    return(NULL)
  }

  atc <- evaluate_atc_rxcui(httr::content(x, "parse"), rx_cui)

  if (query == "none") {
    return(atc)
  } else {
    return(tolower(get_who(atc, query)))
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

evaluate_atc_rxcui <- function(res, rx_cui) {
  check <- unlist(lapply(res$rxclassDrugInfoList$rxclassDrugInfo, function(x) x$minConcept$rxcui == rx_cui))

  if (any(check)) {
    return(res$rxclassDrugInfoList$rxclassDrugInfo[[which(check)]]$rxclassMinConceptItem$classId)
  }

  return(unlist(lapply(.$rxclassDrugInfoList$rxclassDrugInfo, function(x) x$rxclassMinConceptItem$classId)))
}

check_common <- function(who) {
  unique(unlist(who))
}
