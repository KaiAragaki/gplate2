new_well <- function(x = is.double(), y = is.double(), content = list()) {
  stopifnot(is.double(x), is.double(y), is.list(content))
  structure(list(position = c(x, y), content = content), class = "well")
}

#' Create a new well
#' @export
#' @param x,y the x and y position of the well, where the bottom left hand well
#'   is 1, 1
#' @param content a named list of key = value pairs containing well data
#'
#' @examples
#' my_well <- well(
#'  1, 1,
#'  content =
#'    list(
#'      data = iris,
#'      dox = list(time = 1, time_unit = "hr", conc = 100, conc_unit = "ng/mL")
#'    )
#')
well <- function(x, y, content = list()) {
  if (is.null(content)) content <- list()
  well <- new_well(x, y, content)
  validate_well(well)
}

validate_well <- function(well) {
  check_names(well)
  well
}

check_names <- function(well) {
  if (length(content(well)) == 0) return()

  content_names <- names(content(well))
  if (any(content_names == "") || is.null(content_names))
    stop("All items of `content` must be named")
}

#' Get and set the position of a well
#' @param x A `well`
#' @param value A numeric vector
#' @export
#' @return For getting, a vector with two values. For setting, a well.
#' @details Coordinates are from the bottom left of the plate, starting at (1,
#'   1)
position <- function(x) {
  UseMethod("position")
}

#' @export
position.well <- function(x) {
  x$position
}

#' @rdname position
#' @export
`position<-` <- function(x, value) {
  UseMethod("position<-")
}

#' @export
`position<-.well` <- function(x, value) {
  x$position <- value
  x
}

#' Get or set the contents of a well
#' @export
#' @param x A `well`
#' @param value A named list - what to supply the list
#' @return For getting, a named list. For setting, a well.
content <- function(x) {
  UseMethod("content")
}

#' @rdname content
#' @export
content.well <- function(x) {
  x$content
}

#' @rdname content
#' @export
`content<-` <- function(x, value) {
  UseMethod("content<-")
}

#' @rdname content
#' @export
`content<-.well` <- function(x, value) {
  x$content <- value
  x
}

#' Append to content in a well
#' @param x A well
#' @param ... Arguments passed on to their repsective methods
#' @return A `well`
#' @export
update_well <- function(x, ...) {
  UseMethod("update_well")
}

#' @param content_new A named list of content to be added to the well
#' @param overwrite If multiple contents have the same name, should it be
#'   overwritten?
#' @rdname update_well
#' @export
update_well.well <- function(x, content_new, overwrite = TRUE, ...) {
  content_old <- content(x)
  if (update_will_overwrite(content_old, content_new) && !overwrite)
    stop("New contents will overwrite old and overwrite = FALSE")
  content(x) <- content_new
  x
}

update_will_overwrite <- function(content_old, content_new) {
  any(names(content_old) %in% names(content_new))
}

#' Select contents from a well
#' @param x the well to select
#' @param ... arguments passed on to their respective methods
#' @return A `well`
#' @export
select_well <- function(x, ...) {
  UseMethod("select_well")
}

#' @param select Columns to select. Uses tidyselect.
#' @rdname select_well
#' @export
select_well.well <- function(x, select, ...) {
  indices <- tidyselect::eval_select(rlang::enquo(select), content(x))
  # If everything is removed, will output named integer(0).
  # Convert to empty list to align with constructor with no content
  content(x) <- if (length(indices) == 0) {
    list()
  } else {
    content(x)[indices]
  }
  x
}
