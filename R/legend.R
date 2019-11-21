#' Options to control legend appearance.
#'
#' @param width width of the legend in pixels.
#' @param height height of the legend in pixels.
#' @param ... currently ignored.
#'
#' @export legendOptions
#' @name legendOptions
legendOptions = function(width = NULL, height = NULL, ...) {
  list(
    width = width
    , height = height
    , ...
  )
}


## legend for plainview, slideview, cubeview ==============================
rasterLegend <- function(key) {
  clrkey = draw.colorkey(key = key, draw = FALSE)
  clrkey$framevp$x = grid::unit(0.5, "npc")
  clrkey$framevp$justification = 'left'
  clrkey$framevp$valid.just = c(0.5, 0.5)
  clrkey$framevp$layout$just = 'left'
  clrkey$framevp$layout$valid.just = c(1, 0.5)
  clrkey$framevp$clip = FALSE
  grid::grid.draw(clrkey)
}
