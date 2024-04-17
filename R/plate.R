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
#' @export
plate.data.frame <- function(x, name, ...) {
  wells <- data_frame_to_wells(x, name)
  new_plate(wells = wells)
}


vec_to_wells <- function(vector, name, x = 1) {
  stopifnot(length(name) == 1) # required for setting name in second lapply
  ## Could get around this with mapply I think
  content <- vector |>
    lapply(list) |>
    lapply(\(x) {
      names(x) <- name
      x
    })
  mapply(
    well,
    x = x, y = rev(seq_len(length(vector))),
    content = content,
    SIMPLIFY = FALSE
  )
}

data_frame_to_wells <- function(df, name) {
  out <- sapply(
    seq_len(ncol(df)),
    \(i) vec_to_wells(df[, i], name, x = i),
    simplify = FALSE
  )
  names(out) <- NULL
  do.call(c, out)
}

# Cbind, rbind wells?
