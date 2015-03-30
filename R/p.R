#' Pan the current plot
#' 
#' Interactively reposition the current plot.  Requires a \code{qmap} object.
#' Each click repositions the map with the location clicked as the center.
#' The extent of the \code{qmap} object is changed.    
#' 
#' @param qmap_obj A qmap object.  Optional, but performs better with larger 
#'                  data sets. 
#' @param ... arguments to be passed to zoom::move.to.click.zoom(...)
#' @return NULL
#' @export
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' x<-qmap(list(lake,buffer,elev))
#' p()
#' ## Or
#' p(x)
#' }
p <- function(qmap_obj = NULL, ...) {
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
        qmap_obj <- zoom_it(qmap_obj, loc, 1, pan = TRUE)
        n <- 2
      } else {
        qmap_obj <- zoom_it(qmap_obj, loc, 1, pan = TRUE)
        loc <- locator(1)
      }
    }
    assign(obj, qmap_obj, envir = parent.frame())
  }
} 
