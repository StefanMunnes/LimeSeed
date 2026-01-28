library(yaml)
library(limonaid)

# Helper functions to fill empty values on the fly
`%||%` <- function(x, y) if (is.null(x)) y else x


process_text <- function(input, default = "", language = NULL) {
  #' Process a YAML text field with optional language selection
  #'
  #' Helper to extract text from a YAML list element. If the input is
  #' \code{NULL}, a default value is returned. If the input contains
  #' multiple elements (e.g. one per language), either the full vector
  #' is returned or, if a language is specified, only the value for that
  #' language is returned.
  #'
  #' @param input A value or list element (typically from parsed YAML)
  #'   containing one or more character values, optionally named by
  #'   language codes. May be \code{NULL}.
  #' @param default A character scalar to return when \code{input} is
  #'   \code{NULL}. Defaults to \code{""}.
  #' @param language Optional character scalar giving the name of the
  #'   element to return when \code{input} has length greater than one
  #'   (e.g. a language code such as \code{"en"} or \code{"de"}). If
  #'   \code{NULL}, all values are returned.
  #'
  #' @return
  #' If \code{input} is \code{NULL}, returns \code{default}.
  #' If \code{input} has length 1, returns that value as-is.
  #' If \code{input} has length &gt; 1 and \code{language} is \code{NULL},
  #' returns a (possibly named) vector of all values.
  #' If \code{input} has length &gt; 1 and \code{language} is non-\code{NULL},
  #' returns the element corresponding to \code{language}.
  #'
  #' @examples
  #' # Single value
  #' process_text("Hello")
  #'
  #' # Multilingual value from YAML-like structure
  #' txt &lt;- list(en = "Hello", de = "Hallo")
  #' process_text(txt)
  #' process_text(txt, language = "de")
  #'
  #' # Handle NULL with a default
  #' process_text(NULL, default = "N/A")
  #'
  #' @keywords internal

  if (is.null(input)) {
    return(default)
  }

  if (length(names(input)) > 0) {
    # value <- unlist(input, use.names = TRUE) |>
    #   sapply(function(x) {
    #     gsub("\\{(.+)\\}", "\\{html_entity_decode\\(\\1\\)\\}", x)
    #   })

    value <- unlist(input, use.names = TRUE)

    return(value)
  }

  # value <- input |>
  #   sapply(function(x) {
  #     gsub("\\{(.+)}", "\\{html_entity_decode\\(\\1\\)\\}", x)
  #   })

  value <- input

  return(value)
}
