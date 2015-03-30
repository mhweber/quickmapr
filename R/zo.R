#' Zooms out on current plot
#' 
#' Interactively zoom out on the current plot. Requires a 
#' \code{qmap} object. A single zoom out results, with the location clicked at the 
#' center, and the extent of the \code{qmap} object is changed.  
#' 
#' @param qmap_obj A qmap object.  Optional, but performs better with larger 
#'                  data sets.
#' @param zoom_perc A proportion to determine the zoom level.  The x and y 
#'                  axes are increased by this amount.  Default is 0.5.     
#' @param ... arguments to be passed to zoom::out.zoom(...)
#' @return NULL
#' @export
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' qmap(list(lake,buffer,elev))
#' zo()
#' }
zo <- function(qmap_obj = NULL, zoom_perc = 0.5, ...) {
  if (zoom_perc >= 1 || zoom_perc < 0) {
    stop("Argument, zoom_perc, needs to be between 0 and 1")
  }
  if (is.null(qmap_obj)) {
    stop("A 'qmap' object is required.  Create with 'quickmapr::qmap().'")
  } else {
    continue <- 0
    obj <- paste(substitute(qmap_obj))
    message("Click on plot to zoom in. Press 'Esc' to exit.")
    n <- 1
    loc <- 1
    while (!is.null(loc)) {
      if (n == 1) {
        loc <- locator(1)
        qmap_obj <- zoom_it(qmap_obj, loc, zoom_perc, out = TRUE)
        n <- 2
      } else {
        qmap_obj <- zoom_it(qmap_obj, loc, zoom_perc, out = TRUE)
        loc <- locator(1)
      }
    }
    assign(obj, qmap_obj, envir = parent.frame())
  }
} 
