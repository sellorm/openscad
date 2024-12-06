# translate([x,y,z])
#' translate an objects position in 3D space
#' @param object an openscad object to translate
#' @param x amount to move the object by on the x axis
#' @param y amount to move the object by on the y axis
#' @param z amount to move the object by on the z axis
#' @export
translate <- function(object, x = 0, y = 0, z = 0) {
  output <- paste0("translate([", x, ",", y, ",", z, "]){\n", object, "}\n")
  structure(output, class = c("openscad", "character"))
}

# rotate([x,y,z])
# rotate(a, [x,y,z])
#' Rotate on object in 3D space
#' @param object an openscad object to translate
#' @param x amount to rotate the object by on the x axis
#' @param y amount to rotate the object by on the y axis
#' @param z amount to rotate the object by on the z axis
#' @export
rotate <- function(object, x = 0, y = 0, z = 0) {
  output <- paste0("rotate([", x, ",", y, ",", z, "]){\n", object, "}\n")
  structure(output, class = c("openscad", "character"))
}



# hull()
#' Wraps objects to create a hull
#' @param object An openscad object
#' @export
hull <- function(object) {
  output <- paste0("hull(){\n", paste(object, collapse = "\n"), "}\n")
  structure(output, class = c("openscad", "character"))
}



# resize([x,y,z],auto,convexity)
#' resize a 3D object
#' @param object object to resize
#' @param x amount to resize on the x axis
#' @param y amount to resize on the y axis
#' @param z amount to resize on the z axis
#' @param auto If the 'auto' parameter is set to true, it auto-scales any 0-dimensions to match
#' @param convexity convexity level
#' @export
resize <- function(object, x = 0, y = 0, z = 0, auto = FALSE, convexity = 10) {
  if (isFALSE(auto)) {
    auto_value <- ""
  } else {
    auto_value <- ", auto=true"
  }
  output <- paste0(
    "resize([", x, ",", y, ",", z, "]",
    auto_value,
    ", convexity=", convexity,
    "){\n", object, "}\n"
  )
  structure(output, class = c("openscad", "character"))
}




# TODO: ...
# scale([x,y,z])
# mirror([x,y,z])
# multmatrix(m)
# color("colorname",alpha)
# color("#hexvalue")
# color([r,g,b,a])
# offset(r|delta,chamfer)
# minkowski(convexity)
