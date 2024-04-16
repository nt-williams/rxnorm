path_approx <- "REST/approximateTerm"

#' Find active approximate terms and return their RxCUI
#'
#' See https://lhncbc.nlm.nih.gov/RxNav/news/RxNormApproxMatch.html.
#'
#' @param string String to search for.
#' @param n Number of RxCUI to return
#' @param minscore The minimum similarity index for a result to be returned.
#' @param local_host Run query locally using RxNav-in-a-box with Docker?
#'
#' @return The RxCUI; \code{NULL} if not successful.
#' @export
#'
#' @examples
#' find_approx_rxcui("FENTANYL 10MCG/ML SOLN INJ PCA 30ML")
find_approx_rxcui <- function(string, n = 1, minscore = 10, local_host = FALSE) {
  check_internet()
  url <- create_url(local_host, path_approx, term = string, option = 1)
  check_null(parse_approx_rxcui(httr::GET(url), n = n, minscore = minscore))
}

parse_approx_rxcui <- function(x, n, minscore) {
  if (!check_status(x)) return(NA_character_)
  res <- httr::content(x, "parse")$approximateGroup$candidate
  res <- res[purrr::map_lgl(res, function(x) as.numeric(x$score) >= minscore)]
  purrr::map_chr(res, "rxcui")[1:n]
}
