get_history <- function(url, concept = NULL) {
  check_internet()
  check_null(parse_history(httr::GET(paste0(url, "/historystatus")), concept))
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
