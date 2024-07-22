# This function is adapted from forked repository (https://github.com/maia-sh/tidypubmed)
#' Parse Grant
#'
#' @param nodes article node set
#'
#' @return A tibble
#'
#' @examples
#' 
#'
#' @export


# Extract the Grant nodes
pubmed_grants <- function(nodes){
  z <- lapply(nodes, function(x) xml2::xml_find_all(x, ".//Grant"))
  x <- lapply(z, function(x) tibble::enframe( xml2::xml_text(xml2::xml_find_first(x, ".//GrantID") ), "n"))
  x <- bind_rows(x, .id="pmid")
  x$pmid <- as.integer(x$pmid)
  x$grantid <- xml_text_first(z, "//GrantID")
  x$country <- xml_text_first(z,"//Country")
  x$funder <- xml_text_first(z, "//Agency")
  x$acronym <- xml_text_first(z,"//Acronym")
  x
}

