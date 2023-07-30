#' Get Strength of Active Ingredients for a RxCUI
#'
#' @param rx_cui Either a string or numeric RxNorm RxCUI to search for.
#' @param local_host Run query locally using RxNav-in-a-box with Docker?
#'
#' @return A \code{data.frame} of strength information.
#' @export
#'
#' @examples
#' get_rxcui_strength(861819)
get_rxcui_strength <- function(rx_cui, local_host = FALSE) {
  check_internet()
  url <- create_url(local_host, paste0("REST/rxcui/", rx_cui, "/historystatus"))
  check_null(parse_rxcui_strength(httr::GET(url)))
}

parse_rxcui_strength <- function(x) {
  if (!check_status(x)) return(NA_character_)
  ingredients <- httr::content(x, "parse")$rxcuiStatusHistory$definitionalFeatures$ingredientAndStrength
  Reduce(rbind, lapply(ingredients, function(x) {
    as.data.frame(x[c("activeIngredientName",
                      "numeratorValue",
                      "numeratorUnit",
                      "denominatorValue",
                      "denominatorUnit")])
  }))
}
