# circle(radius | d=diameter)
#' create a 2D circle
#' @param radius the radius of the circle
#' @export
circle <- function(radius) {
  output <- paste0("circle(", radius, ");\n")
  structure(output, class = c("openscad", "character"))
}
# square(size,center)
# square([width,height],center)
# polygon([points])
# polygon([points],[paths])
# text(t, size, font,
#      halign, valign, spacing,
#      direction, language, script)
# import("….ext", convexity)
# projection(cut)
