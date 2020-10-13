
base_url <- "https://rxnav.nlm.nih.gov/REST/"
atc_url <- "https://rxnav.nlm.nih.gov/REST/rxclass/class/byRxcui.json?rxcui="
who_url <- "https://www.whocc.no/atc_ddd_index/?code="
ndc_url <- "https://rxnav.nlm.nih.gov/REST/ndcstatus?ndc="

check_internet <- function() {
  stopifnot("No internet connection" = curl::has_internet())
}

check_status <- function(x) {
  httr::status_code(x) == "200"
}

parse_rx <- function(x) {
  if (!check_status(x)) {
    return(NA_character_)
  }

  res <- httr::content(x, "parse")
  check <- res$idGroup$name

  if (is.null(check)) {
    return(get_history(x$url, "name"))
  }

  check
}

parse_bn <- function(x) {
  if (!check_status(x)) {
    return(NA_character_)
  }

  res <- httr::content(x, "parse")
  unlist(lapply(res$relatedGroup$conceptGroup[[1]]$conceptProperties,
                function(x) x$name))
}

parse_atc <- function(x, rx_cui, query) {
  if (!check_status(x)) {
    return(NA_character_)
  }

  res <- httr::content(x, "parse")
  atc <- evaluate_atc_rxcui(res, rx_cui)

  if (is.null(atc)) {
    new_rxcui <- get_history(paste0(base_url, "rxcui/", rx_cui), "rxcui")
  } else {
    new_rxcui <- NULL
  }

  if (!is.null(new_rxcui)) {
    return(get_atc(new_rxcui[[1]], query))
  }

  if (query == "none") {
    return(check_null(atc))
  }

  return(check_null(get_who(atc, query)))
}

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

evaluate_atc_rxcui <- function(res, rx_cui) {
  check <-
    unlist(
      lapply(res$rxclassDrugInfoList$rxclassDrugInfo, function(x)
        x$minConcept$rxcui == rx_cui)
    )

  if (any(check)) {
    return(get_matched_rxcui_atc(res, check))
  }

  return(get_unmatched_atc(res))
}

check_common <- function(who) {
  unique(unlist(who))
}

check_null <- function(x) {
  if (is.null(x)) {
    return(NA_character_)
  }

  return(x)
}

parse_history <- function(x, concept = NULL) {
  if (!check_status(x)) {
    return(NA_character_)
  }

  res <- httr::content(x, "parse")

  check <- switch(concept,
                  rxcui = get_derived_rxcui(res),
                  name  = get_derived_name(res))

  if (is.null(check) & concept == "name") {
    return(check_null(get_historical_name(res)))
  }

  return(check)
}

get_matched_rxcui_atc <- function(res, check) {
  unique(unlist(
    lapply(res$rxclassDrugInfoList$rxclassDrugInfo[which(check)],
           function(x)
             x$rxclassMinConceptItem$classId)
  ))
}

get_unmatched_atc <- function(res) {
  unique(unlist(
    lapply(res$rxclassDrugInfoList$rxclassDrugInfo,
           function(x)
             x$rxclassMinConceptItem$classId)
  ))
}

get_derived_rxcui <- function(x) {
  check <-
    unlist(
      lapply(x$rxcuiStatusHistory$derivedConcepts$remappedConcept,
             function(x)
               x$remappedRxCui)
    )

  if (is.null(check)) {
    check <- x$rxcuiStatusHistory$derivedConcepts$scdConcept$scdConceptRxcui
  }

  check
}

get_derived_name <- function(x) {
  check <-
    x$rxcuiStatusHistory$derivedConcepts$remappedConcept[[1]]$remappedName

  if (is.null(check)) {
    check <- x$rxcuiStatusHistory$derivedConcepts$scdConcept$scdConceptName
  }

  check
}

get_historical_name <- function(x) {
  x$rxcuiStatusHistory$attributes$name
}

#' Subset WHO ATC Codes
#'
#' Subset ATC codes into the minimal code based on the level of query.
#'
#' @param atc A WHO ATC code.
#' @param query The level to subset the code at. Options are "first" (default),
#'  "second", "third", "fourth".
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

parse_ndc <- function(x) {
  if (!check_status(x)) {
    return(NA_character_)
  }

  res <- httr::content(x, "parse")
  check <- res$ndcStatus$rxcui

  if (check == "") {
    return(NA_character_)
  }

  check
}
