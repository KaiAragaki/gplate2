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

#' @rdname plate
#' @param name the 'key' in 'key = value' for plate contents
#' @export
plate.data.frame <- function(x, name, ...) {
  wells <- data_frame_to_wells(x, name)
  new_plate(wells = wells)
}

#' @export
t.plate <- function(x, ...) {
  lapply(x, \(well) {
    position(well) <- rev(position(well))
    well
  }) |>
    plate()
}

vec_to_wells <- function(vector, names, row = 1) {
  content <- lapply(vector, list)
  content <- mapply(stats::setNames, content, names, SIMPLIFY = FALSE)
  mapply(
    well,
    row = row, col = seq_along(vector),
    content = content,
    SIMPLIFY = FALSE
  )
}

data_frame_to_wells <- function(df, names) {
  out <- sapply(
    seq_len(ncol(df)),
    \(i) vec_to_wells(df[i, ], names, row = i),
    simplify = FALSE
  )
  names(out) <- NULL
  do.call(c, out)
}

# Cbind, rbind wells?
