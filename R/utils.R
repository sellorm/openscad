#' Checks the availability of OpenSCAD
#' @export
is_available <- function() {
  file.exists(locate())
}



#' Get the location of the OpenSCAD binary for the current system
#' @export
locate <- function() {
  # TODO: Fix the Windows path
  openscad_path <- switch(Sys.info()["sysname"],
    Darwin  = "/Applications/OpenSCAD.app/Contents/MacOS/OpenSCAD",
    Windows = "c:/Program Files/OpenSCAD",
    Linux   = system("which openscad"),
    "default"
  )

  if (openscad_path == "default") {
    stop("No OpenSCAD path for this system. Please check and try again.")
  }

  return(openscad_path)
}



#' Convert a topographical matrix to SCAD
#' @param topology a matrix containing topological values (like R's built-in volcano)
#' @param block_size The size of the squares in the SCAD file
#' @param scaling a scaling factor to increase of decrease the vertical scale of the blocks
#' @param flatten Where all defined values in the topology are above x, subtract x from those values
#' @param base_thickness Adds additional depth to the base
#' @export
topo_matrix <- function(topology, block_size = 5, scaling = 1, flatten = FALSE, base_thickness = 0) {
  if (!"matrix" %in% class(topology)) {
    stop("Input should be a matrix, like the `volcano` built-in dataset.")
  }
  if (flatten) {
    flatten_amount <- min(topology) - block_size
  } else {
    flatten_amount <- 0
  }
  output <- ""
  for (topo_y in 1:nrow(topology)) {
    for (topo_x in 1:ncol(topology)) {
      depth <- topology[topo_y, topo_x] - flatten_amount + base_thickness
      output <- c(
        output,
        translate(
          cube(
            c(block_size, block_size, depth * scaling)
          ),
          x = block_size * (topo_x - 1),
          y = -block_size * (topo_y - 1)
        )
      )
    }
  }
  structure(output, class = c("openscad", "character"))
}



#' Write scad object out to file
#' @param object an openscad object to write to the filesystem
#' @param filename the filename to write
#' @param overwrite Controls whether to overwrite an existing file
#' @export
write_scad <- function(object, filename, overwrite = FALSE) {
  # TODO: Append .scad to the filename if appropriate
  if (!"openscad" %in% class(object)) {
    stop("Input should be an openscad object.")
  }
  if (file.exists(filename) && !overwrite) {
    stop("File already exists: ", filename)
  }
  writeLines(object, con = filename)
}



#' Convert a SCAD file to an STL file
#' @param scadfile A scad file to convert to STL
#' @param outfile the STL file to write out
#' @export
scad_to_stl <- function(scadfile, outfile) {
  if (!is_available()) {
    stop("OpenSCAD does not appear to be installed. Please install and try again")
  }
  warning("Generating. This may take some time")
  # TODO: We should be able to generate stl from either an object, or a file
  # TODO: Check the input is openscad object
  # TODO: Append .stl to the filename if appropriate
  # TODO: avoid accidental file overwrites
  # TODO: open rendered stl if possible (eg Preview.app)
  system2(locate(), c("-o", outfile, scadfile))
  invisible(file.path(outfile))
}

#' Convert a model file to an STL file
#' @param model A model to convert to STL
#' @param outfile the STL file to write out
#' @export
model_to_stl <- function(model, outfile) {
  if (!is_available()) {
    stop("OpenSCAD does not appear to be installed. Please install and try again")
  }
  warning("Generating. This may take some time")
  output_temp <- tempfile(fileext = ".scad")
  # TODO: We should be able to generate stl from either an object, or a file
  # TODO: Check the input is openscad object
  # TODO: Append .stl to the filename if appropriate
  # TODO: avoid accidental file overwrites
  # TODO: open rendered stl if possible (eg Preview.app)
  write_scad(model, output_temp)
  system2(locate(), c("-o", outfile, output_temp))
  unlink(output_temp)
  invisible(file.path(outfile))
}



#' Open the given file in the OpenSCAD editor
#' @param filename the name of the SCAD file to open in the OpenSCAD editor
#' @export
open_in_OpenSCAD <- function(filename) {
  # TODO: can we open an object instead of just a filename?
  if (!is_available()) {
    stop("OpenSCAD does not appear to be installed. Please install and try again")
  }
  system2(locate(), filename)
}



#' Generate a preview of the given SCAD file
#' @param filename filename from which to generate a preview
#' @export
preview <- function(filename) {
  if (!is_available()) {
    stop("OpenSCAD does not appear to be installed. Please install and try again")
  }
  png_temp <- tempfile(fileext = ".png")
  system2(locate(), c(
    "--autocenter",
    "--viewall",
    "--preview",
    "-o",
    png_temp,
    filename
  ))
  magick::image_read(png_temp)
  unlink(png_temp)
}
