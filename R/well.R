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
#' my_well <- new_well(
#'  1, 1,
#'  content =
#'    list(
#'      data = iris,
#'      dox = list(time = 1, time_unit = "hr", conc = 100, conc_unit = "ng/mL")
#'    )
#')
well <- function(x, y, content) {
  new_well(x, y, content)
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
