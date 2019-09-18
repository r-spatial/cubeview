#' View a RasterStack or RasterBrick as 3-dimensional data cube.
#'
#' @description
#' Creates a 3D data cube view of a RasterStack/Brick.
#' Slices through each dimension (x/y/z) are mapped to the visible sides of the cube.
#' The cube can be freely rotated. Zooming and panning can be used to focus on
#' different areas of the cube.
#'
#' See Details for information on how to control the location of the slices and all
#' other available keyborad and mouse guestures to control the cube.
#'
#' @param x a file name, stars object, RasterStack or RasterBrick
#' @param at the breakpoints used for the visualisation. See
#' \code{\link{levelplot}} for details.
#' @param col.regions either a palette function or a vector of colors to be
#'   used for palette generation.
#' @param na.color color for missing values.
#' @param legend logical. Whether to plot a legend.
#' @param ... additional arguments passed on to \link[stars]{read_stars}.
#'
#' @details
#' The location of the slices can be controlled by keys: \cr
#' x-axis: LEFT / RIGHT arrow key \cr
#' y-axis: DOWN / UP arrow key \cr
#' z-axis: PAGE_DOWN / PAGE_UP key \cr
#'
#' \emph{Other controls:} \cr
#' Press and hold left mouse-button to rotate the cube. \cr
#' Press and hold right mouse-button to move the cube. \cr
#' Spin mouse-wheel or press and hold middle mouse-button and
#' move mouse down/up to zoom the cube. \cr
#' Press space bar to show/hide slice position guides. \cr
#'
#' \emph{Note:} \cr
#' In RStudio cubeView may show a blank viewer window. In this case open the view in
#' a web-browser (RStudio button at viewer: "show in new window").
#'
#' \emph{Note:} \cr
#' Because of key focus issues key-press-events may not always
#' recognised within RStudio on Windows. In this case open the view in
#' a web-browser (RStudio button at viewer: "show in new window").
#'
#' @author
#' Stephan Woellauer and Tim Appelhans
#'
#' @examples
#' if (interactive()) {
#'   library(raster)
#'   library(stars)
#'
#'   ## directly from file
#'   kili_data <- system.file("extdata", "kiliNDVI.tif", package = "cubeview")
#'   cubeview(kili_data)
#'
#'   ## stars object
#'   kili_strs = read_stars(kili_data)
#'   cubeview(kili_strs)
#'
#'   ## rsater stack (also works with brick)
#'   kili_rstr <- stack(kili_data)
#'   cubeview(kili_rstr)
#'
#'   ## use different color palette and set breaks
#'   clr <- viridisLite::viridis
#'   cubeview(kili_data, at = seq(-0.15, 0.95, 0.1), col.regions = clr)
#' }
#'
#' @importFrom raster as.matrix ncol nrow nlayers
#' @importFrom lattice level.colors do.breaks draw.colorkey
#' @importFrom grDevices col2rgb png dev.off rgb
#' @importFrom base64enc base64encode
#' @importFrom htmltools htmlDependency
#' @importFrom htmlwidgets createWidget sizingPolicy shinyWidgetOutput shinyRenderWidget
#' @importFrom viridisLite inferno
#'
#' @export cubeview
#' @name cubeview
cubeview = function(x, ...) UseMethod("cubeview")

#' @name cubeview
#' @export
cubeview.character = function(x,
                              at,
                              col.regions = viridisLite::inferno,
                              na.color = "#BEBEBE",
                              legend = TRUE,
                              ...) {

  if (!file.exists(x[1])) stop(sprintf("cannot find file %s", x))

  strs = stars::read_stars(x, along = "band", ...)
  cubeview(strs,
           ...,
           at = at,
           col.regions = col.regions,
           na.color = na.color,
           legend = legend)
}

#' @name cubeview
#' @export
cubeview.stars <- function(x,
                           at,
                           col.regions = viridisLite::inferno,
                           na.color = "#BEBEBE",
                           legend = TRUE,
                           ...) {

  stopifnot(inherits(x, "stars"))

  if (!is.function(col.regions) &
      (is.character(col.regions) | is.numeric(col.regions))) {
    col.regions = colorRampPalette(col2Hex(col.regions))
  }

  ar = unclass(x[[1]]) # raw data matrix/array
  dms = unname(dim(ar))
  if (length(dms) == 2) {
    v = ar[, rev(seq_len(dms[2]))]
  } else {
    v = do.call(rbind, lapply(rev(seq(dms[3])), function(i) {
      ar[, rev(seq_len(dim(ar)[2])), i]
    }))
  }
  rng = range(v, na.rm = TRUE)
  if (missing(at)) at <- lattice::do.breaks(rng, 256)

  # cuts <- cut(v, at, include.lowest = TRUE, labels = FALSE)
  #
  # n = 12L
  # m = grDevices::colorRamp(col.regions(n))( (1:n)/n )
  #
  # cols = colourvalues::colour_values_rgb(
  #   cuts,
  #   palette = m,
  #   na_colour = na.color,
  #   include_alpha = TRUE
  # )

  cols <- lattice::level.colors(v,
                                at = at,
                                col.regions)
  cols[is.na(cols)] = na.color
  # cols = col2Hex(cols, alpha = TRUE)
  cols = grDevices::col2rgb(cols, alpha = TRUE)

  x_size <- dms[1]
  z_size <- dms[2]
  y_size <- ifelse(length(dms) == 2, 1, dms[3])

  leg_fl <- NULL

  if (legend) {
    ## unique temp dir
    dir <- tempfile()
    dir.create(dir)

    # if (missing(at)) at <- lattice::do.breaks(rng, 256)
    leg_fl <- paste0(dir, "/legend_", createId(), ".png")
    grDevices::png(leg_fl, height = 200, width = 80, units = "px",
        bg = "transparent", pointsize = 14, antialias = "none")
    rasterLegend(
      list(
        col = col.regions,
        at = at,
        height = 0.9,
        space = "right"
      )
    )
    grDevices::dev.off()
  }


  cubeViewRaw(red = cols[1, ],
              green = cols[2, ],
              blue = cols[3, ],
              x_size = x_size,
              y_size = y_size,
              z_size = z_size,
              leg_fl = leg_fl)

}

#' @name cubeview
#' @export
cubeview.Raster = function(x,
                           at,
                           col.regions = viridisLite::inferno,
                           na.color = "#BEBEBE",
                           legend = TRUE,
                           ...) {

  if (!raster::inMemory(x)) {
    fls = sapply(
      lapply(
        x@layers,
        methods::slot,
        name = "file"
      ),
      methods::slot,
      "name"
    )
    if (all(duplicated(fls)[2:length(fls)])) {
      x = fls[1]
    } else {
      x = stars::read_stars(fls, along = "band", ...)
    }
    cubeview(x,
             ...,
             at = at,
             col.regions = col.regions,
             na.color = na.color,
             legend = legend)
  } else {
    x = stars::st_as_stars(x)
    cubeview(x,
             ...,
             at = at,
             col.regions = col.regions,
             na.color = na.color,
             legend = legend)
  }
}



# #' View a cube of 3-dimensional data filled with points (voxels).
# #'
# #' A variation of Hovmoeller diagram: Each voxel is colored with a RGB-color (or grey) value.
# #'
# #' @param x_size integer. size of x-dimension
# #'
# #' @param y_size integer. size of y-dimension
# #'
# #' @param z_size integer. size of z-dimension
# #'
# #' @param grey optional integer vector with 0 <= value <= 255.
# #'
# #' @param red optional integer vector with 0 <= value <= 255.
# #'
# #' @param green optional integer vector with 0 <= value <= 255.
# #'
# #' @param blue optional integer vector with 0 <= value <= 255.
# #'
# #' @details
# #'
# #' The cube faces show a selectable layer of data within the cube.
# #'
# #' The visible layers are alterable by keys:
# #'
# #' x-axis: LEFT / RIGHT arrow key
# #'
# #' y-axis: DOWN / UP arrow key
# #'
# #' z-axis: PAGE_DOWN / PAGE_UP key
# #'
# #' Note: Because of key focus issues key-press-events are not always
# #' recognised within RStudio at Windows.
# #' In this case open the view in a web-browser (RStudio button: "show in new window").
# #'
# #'
# #' Press and hold left mouse-button to rotate the cube.
# #'
# #' Press and hold right mouse-button to move the cube.
# #'
# #' Spin mouse-wheel or press and hold middle mouse-button and move mouse
# #' down/up to zoom the cube.
# #'
# #' Press SPACE to toggle showing cross section lines on the cube.
# #'
# #' The color resp. grey vectors contain sequentially values of each voxel.
# #' So each vector is length == x_size * y_size * z_size.
# #' Color component values overwrite grey values.
# #'
# #' Sequence of coordinates (x,y,z) for values in vectors:
# #'
# #' (1,1,1), (2,1,1), (3,1,1), ... (1,2,1), (2,2,1), (3,2,1), ... (1,1,2), (2,1,2), (3,1,2), ...
# #'
# #'
# #' @author
# #' Stephan Woellauer
# #'
# #' @import htmlwidgets
# #'
# #' @export
cubeViewRaw <- function(grey = NULL,
                        red = NULL,
                        green = NULL,
                        blue = NULL,
                        x_size,
                        y_size,
                        z_size,
                        width = NULL,
                        height = NULL,
                        leg_fl = NULL) {

  total_size <- x_size*y_size*z_size

  object_list <- list(x_size = x_size,
                      y_size = y_size,
                      z_size = z_size,
                      legend = !is.null(leg_fl))

  if(!is.null(grey)) {
    if(length(grey)!=total_size) {
      stop("length of grey vector not correct: ", length(grey), " should be ", total_size)
    }
    object_list <- c(object_list, list(grey=base64enc::base64encode(as.raw(as.integer(grey)))))
  }

  if(!is.null(red)) {
    if(length(red)!=total_size) {
      stop("length of red vector not correct: ", length(red), " should be ", total_size)
    }
    object_list <- c(object_list, list(red=base64enc::base64encode(as.raw(as.integer(red)))))
  }

  if(!is.null(green)) {
    if(length(green)!=total_size) {
      stop("length of green vector not correct: ", length(green), " should be ", total_size)
    }
    object_list <- c(object_list, list(green=base64enc::base64encode(as.raw(as.integer(green)))))
  }

  if(!is.null(blue)) {
    if(length(blue)!=total_size) {
      stop("length of blue vector not correct: ", length(blue), " should be ", total_size)
    }
    object_list <- c(object_list, list(blue=base64enc::base64encode(as.raw(as.integer(blue)))))
  }

  deps <- list()

  if(!is.null(leg_fl)) {
    images_dir <- dirname(leg_fl)
    legend_file <- basename(leg_fl)
    attachments <- list(legend=legend_file)
    dep1 <- htmltools::htmlDependency(name = "images", version = "1", src = c(file = images_dir), attachment = attachments, all_files = FALSE)
    deps <- list(dep1)
  }

  # create widget
  htmlwidgets::createWidget(
    name = 'cubeView',
    x = object_list,
    width = width,
    height = height,
    package = 'cubeview',
    sizingPolicy = htmlwidgets::sizingPolicy(padding = 0, browser.fill = TRUE),
    dependencies = deps
  )
}

#' Widget output/render function for use in Shiny
#'
#' @param outputId Output variable to read from
#' @param width,height the width and height of the map
#' (see \code{\link{shinyWidgetOutput}})
#'
#' @examples
#' if (interactive()) {
#'   library(shiny)
#'   library(raster)
#'
#'   kili_data <- system.file("extdata", "kiliNDVI.tif", package = "cubeview")
#'   kiliNDVI <- stack(kili_data)
#'
#'   cube = cubeView(kiliNDVI)
#'
#'   ui = fluidPage(
#'     cubeViewOutput("cube", width = 300, height = 300)
#'   )
#'
#'   server = function(input, output, session) {
#'     output$cube <- renderCubeView(cube)
#'   }
#'
#'   shinyApp(ui, server)
#'
#' }
#'
#' @name cubeViewOutput
#' @export
cubeViewOutput <- function(outputId, width = '100%', height = '400px'){
  htmlwidgets::shinyWidgetOutput(outputId, 'cubeView',
                                 width = width, height = height,
                                 package = 'cubeview')
}

#' @param expr An expression that generates an HTML widget
#' @param env The environment in which to evaluate expr
#' @param quoted Is expr a quoted expression (with quote())?
#' This is useful if you want to save an expression in a variable
#'
#' @rdname cubeViewOutput
#' @export
renderCubeView <- function(expr, env = parent.frame(), quoted = FALSE) {
  if (!quoted) { expr <- substitute(expr) } # force quoted
  htmlwidgets::shinyRenderWidget(expr, cubeViewOutput, env, quoted = TRUE)
}


## cubeview ===============================================================

#' @name cubeview
#' @export
cubeView <- cubeview

