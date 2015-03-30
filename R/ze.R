#' Zooms in on extent
#' 
#' Select a bounding box interactively and zoom to that extent. Requires a 
#' \code{qmap} object. A single zoom in to the selected
#' extent results and the extent of the \code{qmap} object is changed.  
#' 
#' @param qmap_obj A qmap object.  Optional, but performs better with larger 
#'                  data sets.
#' @param ... arguments to be passed to zoom::sq.zoom()
#' @return NULL
#' @export
#' 
#' @examples
#' \dontrun{
#' data(lake)
#' qmap(list(lake,buffer,elev))
#' ze()
#' }
ze <- function(qmap_obj = NULL, ...) {
  if (is.null(qmap_obj)) {
    stop("A 'qmap' object is required.  Create with 'quickmapr::qmap().'")
  } else {
    obj <- paste(substitute(qmap_obj))
    message("Select 2 points to define the zoom extent.")
    qmap_obj$map_extent <- bbox(SpatialPoints(locator(2)))
    assign(obj, qmap_obj, envir = parent.frame())
    return(plot.qmap(qmap_obj))
  }
  
} 
