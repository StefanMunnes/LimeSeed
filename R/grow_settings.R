#' Build survey settings and create Survey object
#'
#' Constructs a limonaid Survey R6 object from the `settings` element of a
#' LimeSeed sprout produced by [lime_sprout()](R/lime_sprout.R:11).
#'
#' @param sprout named list as returned by [lime_sprout()](R/lime_sprout.R:11)
#'   containing a `settings` list of arguments accepted by `limonaid::Survey$new`.
#'
#' @return A `limonaid::Survey` R6 object.
#'
#' @details The `sprout$settings` list is flattened with `unlist(...,
#'   use.names = TRUE)` and passed via `do.call()` to `Survey$new`. Typical
#'   fields include `title`, `language`, `additional_languages`, and other
#'   LimeSurvey survey settings supported by limonaid. A short message is
#'   emitted containing the created survey's title if available.
#'
#' @seealso [lime_sprout()](R/lime_sprout.R:11), [grow_structure()](R/grow_structure.R:8),
#'   [grow_quota()](R/grow_quota.R:1)
#' @import limonaid
#' @export
grow_settings <- function(sprout) {
  settings <- lapply(sprout$settings, unlist, use.names = TRUE)

  tree <- do.call(Survey$new, settings)

  title <- tree$settings$title[[1]]

  if (!is.null(title)) {
    message("Survey successfully created: ", title)
  } else {
    message("Survey successfully created!")
  }

  return(tree)
}
