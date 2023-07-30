#' Find NDC status code
#'
#' @param ndc An NDC code as a character string.
#' @param local_host Run query locally using RxNav-in-a-box with Docker?
#'
#' @return NDC status code.
#' @export
#'
#' @examples
#' get_ndc_status("00002143301")
get_ndc_status <- function(ndc, local_host = FALSE) {
  check_internet()
  url <- create_url(local_host, path_ndc, ndc = ndc)
  parse_ndc(httr::GET(url), "status")
}
