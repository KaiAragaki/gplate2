new_plate <- function(wells = list()) {
  stopifnot(is.list(wells))
  structure(wells, class = "plate")
}

#' Create a plate
#' @param x A `well` or list of wells
#' @param ... additional arguments passed to their respective methods
#' @return a `plate`
#' @export
plate <- function(x, ...) {
  UseMethod("plate")
}

#' @rdname plate
#' @export
plate.well <- function(x, ...) {
  new_plate(wells = list(x))
}

#' @rdname plate
#' @export
plate.list <- function(x, ...) {
  new_plate(wells = x)
}
