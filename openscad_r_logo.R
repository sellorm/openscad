library(openscad)

# ovals -------------------------------------------------------------------
oval_outer <- circle(10) |>
  linear_extrude(height = 3) |>
  resize(x = 31, y = 19)

oval_inner <- circle(10) |>
  linear_extrude(height = 3) |>
  resize(x = 24, y = 14, z = 0) |>
  translate(x = 2, y = -1)

oval_final <- difference(c(oval_outer, oval_inner))

# r upright ---------------------------------------------------------------
r_upright <- cube(c(5, 17, 6)) |>
  translate(x = -3, y = -13)

# r loop ------------------------------------------------------------------
r_loop_outer <- hull(c(
  circle(5) |>
    linear_extrude(6) |>
    translate(x = 7, y = -1),
  cube(c(5, 6, 6)) |>
    translate(x = -3, y = -2),
  cube(c(5, 6, 6)) |>
    translate(x = -3, y = -6)
))
r_loop_inner <- hull(c(
  circle(1.5) |>
    linear_extrude(6) |>
    translate(x = 6, y = -1.2),
  cube(c(2, 2, 6)) |>
    translate(x = -3, y = -1.7),
  cube(c(2, 2, 6)) |>
    translate(x = -3, y = -2.7)
))
r_loop <- difference(c(r_loop_outer, r_loop_inner))

# r leg -------------------------------------------------------------------
top_left <- cube(c(0.1, 0.1, 6)) |>
  translate(x = 14, y = -13)
top_right <- cube(c(0.1, 0.1, 6)) |>
  translate(x = 8.5, y = -13)
bottom_left <- cube(c(1, 1, 6)) |>
  translate(x = 7, y = -5)
bottom_right <- cube(c(1, 1, 6)) |>
  translate(x = 3.5, y = -5)

r_leg <- hull(c(top_left, top_right, bottom_left, bottom_right))

# hoop --------------------------------------------------------------------
hoop <- difference(c(cylinder(1, 2), cylinder(1, 1))) |>
  translate(x = 2, y = 10, z = 2)

# join model --------------------------------------------------------------
model <- paste0(
  "$fn=100;\n",
  oval_final,
  r_upright,
  r_loop,
  r_leg,
  hoop,
  collapse = "\n"
)
model_final <- structure(model, class = c("openscad", "character"))

# Write final model -------------------------------------------------------
write_scad(model_final, "r-logo.scad", overwrite = TRUE)
model_to_stl(model_final, "r-logo.stl")
