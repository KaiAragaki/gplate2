# Well
#
# Does a well have a location? Or does it make sense for a superstructure to
# orchestrate location?
#
# A well should contain a list of key-pair values that describe its contents
#
# Few validators for correctness - don't get in the way

new_well <- function(x = is.double(), y = is.double(), content = list()) {
  stopifnot(is.double(x), is.double(y), is.list(content))
  structure(list(position = c(x, y), content = content), class = "well")
}

#' @export
position <- function(x) {
  UseMethod("position")
}

#' @export
position.well <- function(x) {
  x$position
}

#' @export
content <- function(x) {
  UseMethod("content")
}

#' @export
content.well <- function(x) {
  x$content
}

#' @export
`content<-` <- function(x, value) {
  UseMethod("content<-")
}

#' @export
`content<-.well` <- function(x, value) {
  x$content <- value
  x
}

#' @export
update_well <- function(x, ...) {
  UseMethod("update_well")
}

#' @export
update_well.well <- function(x, content_new, overwrite = TRUE) {
  content_old <- content(x)
  if (update_will_overwrite(content_old, content_new) && !overwrite)
    stop("New contents will overwrite old and overwrite = FALSE")
  content(x) <- content_new
  x
}

update_will_overwrite <- function(content_old, content_new) {
  any(names(content_old) %in% names(content_new))
}

#' @export
select_well <- function(x, ...) {
  UseMethod("select_well")
}

#' @export
select_well.well <- function(x, select) {
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
