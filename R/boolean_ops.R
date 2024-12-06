# union()
#' Join two or more objects together as one
#' @param object an openscad object
#' @export
union <- function(object) {
  output <- paste0("union(){\n", paste(object, collapse = "\n"), "}\n")
  structure(output, class = c("openscad", "character"))
}



# difference()
#' Remove one object from another
#' @param object an openscad object
#' @export
difference <- function(object) {
  output <- paste0("difference(){\n", paste(object, collapse = "\n"), "}\n")
  structure(output, class = c("openscad", "character"))
}



# intersection()
#' intersection()
#' @param object an openscad object
#' @export
intersection <- function(object) {
  output <- paste0("intersection(){\n", paste(object, collapse = "\n"), "}\n")
  structure(output, class = c("openscad", "character"))
}
