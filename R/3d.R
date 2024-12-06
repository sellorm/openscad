# sphere(radius | d=diameter)
#' sphere
#' @param radius the radius of the sphere
#' @export
sphere <- function(radius) {
  output <- paste0("sphere(", radius, ");\n")
  structure(output, class = c("openscad", "character"))
}

# cube(size, center)
# cube([width,depth,height], center)
#' cube
#' @param size the size of the cube. Use a single value for a cube or a vector of three values (x,y,z) for a cuboid
#' @param center center the cube on the x,y,z origin
#' @export
cube <- function(size, center = FALSE) {
  if (center) {
    center_text <- "true"
  } else {
    center_text <- "false"
  }
  if (length(size) == 1) {
    dimension <- list(x = size, y = size, z = size)
  } else if (length(size) == 3) {
    dimension <- list(x = size[1], y = size[2], z = size[3])
  } else {
    stop("Size must be expressed as either single number used as the length of each side, or as a vector of three numbers representing the dimensions width, depth and height")
  }
  output <- paste0(
    "cube([", dimension["x"],
    ",", dimension["y"],
    ",", dimension["z"],
    "], center=", center_text, ");\n"
  )
  structure(output, class = c("openscad", "character"))
}

# linear_extrude(height,center,convexity,twist,slices)
#' Linear extrusion operations
#'
#' Takes a 2 dimensional object as input and generates a 3d object as output
#' @param object a 2D openSCAD object
#' @param height the height of the extrusion
#' @param center Center on the origin
#' @param convexity convexity level
#' @param twist degrees of twist
#' @param slices number of intermediate points along the Z axis of the extrusion
#' @param scale scale
#' @export
linear_extrude <- function(object, height, center = FALSE, convexity = 10, twist = 0, slices = NA, scale = 1) {
  if (!is.na(slices)) {
    slices_value <- paste0(" slices=", slices, ", ")
  } else {
    slices_value <- ""
  }
  center_value <- if (isTRUE(center)){
    ", center=true"
  } else {
    ""
  }
  output <- paste0(
    "linear_extrude(height=", height,
    center_value,
    ", convexity=", convexity,
    ", twist=", twist,
    slices_value,
    ", scale=", scale,
    "){\n", object, "}\n"
  )
  structure(output, class = c("openscad", "character"))
}


# cylinder(h,r|d,center)
# cylinder(h,r1|d1,r2|d2,center)
#' create a cylinder
#' @param height height
#' @param radius radius
#' @param radius2 secondary radius for cone-like 3D shape
#' @param center bool
#' @export
cylinder <- function(height, radius, radius2=NA, center=FALSE){
  center_text <- if ( isFALSE(center) ){
    ""
  } else {
    ", center=true"
  }
  radius2_text <- if ( is.na(radius2)){
    paste0(", r2 = ", radius)
  } else {
    paste0(", r2 = ", radius2)
  }
  output <- paste0("cylinder(h = ", height, ", r1 = ", radius, radius2_text, center_text, ");\n")
  structure(output, class=c("openscad", "character"))
}

# TODO:
# polyhedron(points, faces, convexity)
# import("….ext", convexity)
# rotate_extrude(angle,convexity)
# surface(file = "….ext",center,convexity)
