#' Options to control cube appearance.
#'
#' @param x_pos initial position of x-axis slice.
#' @param y_pos initial position of y-axis slice.
#' @param z_pos initial position of z-axis slice.
#' @param ... currently ignored.
#'
#' @export cubeOptions
#' @name cubeOptions
cubeOptions = function(x_pos = 1, y_pos = 1, z_pos = 1, ...) {
  list(
    x_pos = x_pos
    , y_pos = y_pos
    , z_pos = z_pos
    , ...
  )
}
