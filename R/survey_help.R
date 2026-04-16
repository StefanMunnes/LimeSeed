# limesurvey_help.R — Discovery helpers for LimeSurvey survey authoring
# ─────────────────────────────────────────────────────────────────────────────
#
# Prefix convention:
#   lsh_   public discovery functions   (this file)
#   lst_   template generators          (future file)
#
# Public API:
#   lsh_types(type, details, lang)    describe question types
#   lsh_options(types, search, lang)  describe attribute options
#
# Pipe workflow:
#   lsh_types("F")                      one type, full detail
#   lsh_types(c("F", "N"))              several types
#   lsh_types("array")                  label / search term
#   lsh_types("all", details = TRUE)    detailed table of all types
#   lsh_types(c("F", "N")) |> lsh_options()   options for F and N
#   lsh_options(search = "time")        search all options
#
# Requires: survey_defs.R (LS_QUESTION_TYPES, LS_Q_OPTIONS, LS_TYPES,
#           LS_LABEL_TO_CODE, LS_ALL_TYPE_CODES)

# ══ Internal utilities ════════════════════════════════════════════════════════

# Horizontal rule
.lsh_rule <- function(w = 60) strrep("\u2500", w)

# Left-aligned fixed-width column
.lsh_pad <- function(x, w) formatC(as.character(x), width = -w, flag = "-")

# Truncate string to n chars, appending … if cut
.lsh_trunc <- function(x, n) {
  if (nchar(x) > n) paste0(substr(x, 1, n - 1), "\u2026") else x
}

# Format a valid-values spec into a readable string
.lsh_fmt_valid <- function(v, max_len = 20) {
  if (is.null(v)) {
    return("any")
  }
  if (is.function(v)) {
    return("(see docs)")
  }
  s <- paste(as.character(v), collapse = " | ")
  .lsh_trunc(s, max_len)
}

# Extract language-aware description from a type or option spec.
# Falls back to English, then to "".
.lsh_desc <- function(spec, lang = "en") {
  d <- spec$description
  if (is.null(d)) {
    return("")
  }
  trimws(as.character(d[[lang]] %||% d[["en"]] %||% ""))
}

# Wrap a character vector of items into lines no wider than `width`.
# All continuation lines are indented to the same column as the first item.
.lsh_wrap <- function(items, prefix, sep = ", ", width = 80) {
  if (length(items) == 0) {
    return(prefix)
  }
  indent <- strrep(" ", nchar(prefix))
  cur <- prefix
  lines <- character(0)
  for (i in seq_along(items)) {
    chunk <- if (i < length(items)) paste0(items[[i]], sep) else items[[i]]
    if (nchar(cur) + nchar(chunk) > width && nchar(cur) > nchar(prefix)) {
      lines <- c(lines, cur)
      cur <- paste0(indent, chunk)
    } else {
      cur <- paste0(cur, chunk)
    }
  }
  paste(c(lines, cur), collapse = "\n")
}


# ══ S3 class: lsh_types_result ═══════════════════════════════════════════════
#
# A character vector of resolved LS type codes with a lightweight print method.
#
# Design rationale for the compact print:
#   When lsh_types() is called, the full formatted output is printed immediately
#   — that is the intended discovery moment.  If the user stores the result
#   (`x <- lsh_types("array")`), typing `x` in the console should show a quick
#   reminder of what is stored, not re-flood the console with the same output.
#   The one-liner also hints at the two natural next steps: pass to lsh_types()
#   for full detail, or pipe into lsh_options() for option lookup.

.lsh_types_result <- function(codes) {
  structure(codes, class = c("lsh_types_result", "character"))
}

#' @export
print.lsh_types_result <- function(x, ...) {
  codes <- as.character(x)
  if (length(codes) == 0) {
    cat("<lsh_types_result: (empty)>\n")
    return(invisible(x))
  }
  labels <- vapply(
    codes,
    function(c) LS_QUESTION_TYPES[[c]]$labels[[1L]],
    character(1L)
  )
  entries <- paste(sprintf("[%s] %s", codes, labels), collapse = "  ")
  cat(sprintf("<lsh_types_result [%d]>  %s\n", length(codes), entries))
  cat(
    "  \u2192 lsh_types(x) for full details  |  x |> lsh_options() for options\n"
  )
  invisible(x)
}


# ══ Type resolution ═══════════════════════════════════════════════════════════
#
# Three-stage resolution applied per token:
#   1. Exact LS type code     ("F", "L", ":")
#   2. Exact label match      ("array", "radio") — case-insensitive
#   3. Substring search       across all labels and en/de descriptions

.lsh_resolve_types <- function(types, quiet = FALSE) {
  if (identical(types, "all")) {
    return(LS_ALL_TYPE_CODES)
  }

  codes <- character(0)
  not_found <- character(0)

  for (t in as.character(types)) {
    t_trim <- trimws(t)
    t_low <- tolower(t_trim)

    if (t_trim %in% LS_ALL_TYPE_CODES) {
      codes <- c(codes, t_trim)
      next
    }
    hits <- Filter(
      function(code) {
        spec <- LS_QUESTION_TYPES[[code]]
        any(grepl(
          t_low,
          tolower(c(
            spec$labels,
            spec$description$en %||% "",
            spec$description$de %||% ""
          )),
          fixed = TRUE
        ))
      },
      LS_ALL_TYPE_CODES
    )

    if (length(hits) > 0) {
      codes <- c(codes, hits)
    } else {
      not_found <- c(not_found, t_trim)
    }
  }

  if (!quiet && length(not_found) > 0) {
    warning(
      "No type found for: ",
      paste(sprintf("'%s'", not_found), collapse = ", "),
      ".\nUse lsh_types() to browse all available types.",
      call. = FALSE
    )
  }
  unique(codes)
}


# ══ lsh_types() — print helpers ═══════════════════════════════════════════════

# Print the full detail block for one question type.
.lsh_one_type <- function(code, lang) {
  spec <- LS_QUESTION_TYPES[[code]]
  type_spec <- LS_TYPES[[code]]
  desc <- .lsh_desc(spec, lang)

  # Header: [code]  primary label  —  description (language-aware)
  desc_part <- if (nchar(desc) > 0) paste0("  \u2014  ", desc) else ""
  cat("\n[", code, "]  ", spec$labels[[1L]], desc_part, "\n", sep = "")

  # All labels on one line (wrapped if long)
  cat(.lsh_wrap(spec$labels, prefix = "  Labels  : ", sep = " | "), "\n")

  req <- if (length(type_spec$requires) == 0L) {
    "(none)"
  } else {
    paste(type_spec$requires, collapse = " + ")
  }
  cat("  Requires: ", req, "\n", sep = "")
  cat("  Quota   : ", if (type_spec$quota) "YES" else "no", "\n", sep = "")

  opts <- names(type_spec$options)
  if (length(opts) > 0L) {
    cat(
      .lsh_wrap(opts, prefix = sprintf("  Options : %d \u2014 ", length(opts))),
      "\n"
    )
  } else {
    cat("  Options : (none beyond core fields)\n")
  }
}

# Detail view: one or a few specific types
.lsh_print_types_detail <- function(codes, lang) {
  n <- length(codes)
  header <- if (n == 1L) {
    "LimeSurvey Question Type"
  } else {
    sprintf("LimeSurvey Question Types  [%d matched]", n)
  }
  cat("\n", header, "\n", .lsh_rule(nchar(header)), "\n", sep = "")
  for (code in codes) {
    .lsh_one_type(code, lang)
  }
  cat("\n", .lsh_rule(50L), "\n", sep = "")
  cat("Pipe into lsh_options() to see attribute options.\n\n")
}

# Grouped view: all types organised by category
.lsh_print_types_grouped <- function(codes, lang) {
  cat("\nAll LimeSurvey Question Types\n", .lsh_rule(50L), "\n", sep = "")

  # Any code not covered by LS_TYPE_GROUPS lands in "Other"
  ungrouped <- setdiff(codes, unlist(LS_TYPE_GROUPS, use.names = FALSE))
  groups <- if (length(ungrouped) > 0L) {
    c(LS_TYPE_GROUPS, list(Other = ungrouped))
  } else {
    LS_TYPE_GROUPS
  }

  for (group_name in names(groups)) {
    group_codes <- intersect(groups[[group_name]], codes)
    if (length(group_codes) == 0L) {
      next
    }
    cat(
      "\n\u2500\u2500 ",
      group_name,
      " ",
      .lsh_rule(max(2L, 40L - nchar(group_name))),
      "\n",
      sep = ""
    )
    for (code in group_codes) {
      .lsh_one_type(code, lang)
    }
  }

  cat("\n", .lsh_rule(50L), "\n", sep = "")
  cat("Pipe into lsh_options() to see attribute options.\n\n")
}

# Compact view: one row per type — code | primary label | description
# When all types are shown, group headers are printed before each category.
.lsh_print_types_compact <- function(codes, lang) {
  w_code <- 6L
  w_label <- 28L
  is_all <- setequal(codes, LS_ALL_TYPE_CODES)

  cat("LimeSurvey Question Types\n", .lsh_rule(70L), "\n", sep = "")
  cat(.lsh_pad("Code", w_code), .lsh_pad("Label", w_label), "Description\n")
  cat(.lsh_rule(70L), "\n")

  .one_compact_row <- function(code) {
    spec <- LS_QUESTION_TYPES[[code]]
    desc <- .lsh_trunc(.lsh_desc(spec, lang), 40L)
    cat(
      .lsh_pad(sprintf("[%s]", code), w_code),
      .lsh_pad(spec$labels[[1L]], w_label),
      desc,
      "\n"
    )
  }

  if (is_all) {
    ungrouped <- setdiff(codes, unlist(LS_TYPE_GROUPS, use.names = FALSE))
    groups <- if (length(ungrouped) > 0L) {
      c(LS_TYPE_GROUPS, list(Other = ungrouped))
    } else {
      LS_TYPE_GROUPS
    }
    for (group_name in names(groups)) {
      group_codes <- intersect(groups[[group_name]], codes)
      if (length(group_codes) == 0L) {
        next
      }
      cat("\n  # ", group_name, "\n", sep = "")
      for (code in group_codes) {
        .one_compact_row(code)
      }
    }
  } else {
    for (code in codes) {
      .one_compact_row(code)
    }
  }

  cat("\n")
}


# ══ lsh_options() — print helpers ═════════════════════════════════════════════

.lsh_print_options <- function(codes, opt_names, lang) {
  is_all <- setequal(codes, LS_ALL_TYPE_CODES)

  # Header
  header <- if (is_all) {
    "All LimeSurvey Attribute Options"
  } else if (length(codes) == 1L) {
    sprintf(
      "Attribute Options for [%s] %s",
      codes,
      LS_QUESTION_TYPES[[codes]]$labels[[1L]]
    )
  } else {
    paste0(
      "Attribute Options for: ",
      paste(sprintf("[%s]", codes), collapse = " ")
    )
  }
  cat("\n", header, "\n", .lsh_rule(max(40L, nchar(header))), "\n\n", sep = "")

  if (length(opt_names) == 0L) {
    cat("(no options match)\n\n")
    return(invisible(NULL))
  }

  # Column widths — Option column adapts to the longest name in this result set
  w_name <- min(38L, max(nchar(opt_names))) + 2L
  w_dflt <- 12L
  w_valid <- 20L
  w_desc <- 60L

  # Table header
  cat(
    .lsh_pad("Option", w_name),
    .lsh_pad("Default", w_dflt),
    .lsh_pad("Valid", w_valid),
    .lsh_pad("Description", w_desc),
    "Types\n"
  )
  cat(.lsh_rule(w_name + w_dflt + w_valid + w_desc + 8L), "\n")

  for (nm in opt_names) {
    opt <- LS_Q_OPTIONS[[nm]]

    dflt <- if (!is.null(opt$default)) {
      sprintf("'%s'", opt$default)
    } else {
      "(none)"
    }
    lang_star <- if (isTRUE(opt$language)) " \u2605" else ""
    valid_str <- paste0(
      .lsh_fmt_valid(opt$valid, max_len = w_valid - 1L),
      lang_star
    )
    desc_str <- .lsh_trunc(.lsh_desc(opt, lang), w_desc)
    types_str <- if (is.null(opt$ls_type)) {
      "all"
    } else {
      paste(opt$ls_type, collapse = " ")
    }

    cat(
      .lsh_pad(nm, w_name),
      .lsh_pad(dflt, w_dflt),
      .lsh_pad(valid_str, w_valid),
      .lsh_pad(desc_str, w_desc),
      types_str,
      "\n"
    )
  }

  cat("\n", .lsh_rule(40L), "\n", sep = "")
  cat(
    "\u2605 = multilingual: supply as named list, e.g.  {en: '...', de: '...'}\n\n"
  )
}

# Build the data.frame returned invisibly by lsh_options()
.lsh_opts_df <- function(opt_names, lang = "en") {
  rows <- lapply(opt_names, function(nm) {
    opt <- LS_Q_OPTIONS[[nm]]
    data.frame(
      option = nm,
      default = if (!is.null(opt$default)) {
        as.character(opt$default)
      } else {
        NA_character_
      },
      valid = .lsh_fmt_valid(opt$valid, max_len = 200L),
      description = .lsh_desc(opt, lang),
      applies_to = if (is.null(opt$ls_type)) {
        "all"
      } else {
        paste(opt$ls_type, collapse = " ")
      },
      multilingual = isTRUE(opt$language),
      stringsAsFactors = FALSE
    )
  })
  if (length(rows) == 0L) {
    return(data.frame(
      option = character(),
      default = character(),
      valid = character(),
      description = character(),
      applies_to = character(),
      multilingual = logical(),
      stringsAsFactors = FALSE
    ))
  }
  do.call(rbind, rows)
}


# ══ Public functions ═══════════════════════════════════════════════════════════

#' Describe LimeSurvey question types
#'
#' Resolves type codes, label synonyms, or search terms and prints a structured
#' overview of each matched type.  Returns the matched type codes invisibly as
#' an \code{lsh_types_result} object, which can be piped into
#' \code{\link{lsh_options}()}.
#'
#' @details
#' **Resolution order** (applied per token in \code{types}):
#' \enumerate{
#'   \item Exact LS type code (\code{"F"}, \code{"L"}, \code{":"}, …)
#'   \item Substring search across all labels and type descriptions (en/de)
#' }
#' When \code{types = "all"} (the default), types are grouped by category
#' (List/Radio, Text, Array, Multiple Choice, Special/Masked).
#'
#' @param types Character vector. Type codes, labels, search terms, or
#'   \code{"all"} (default).
#' @param details Logical. If \code{TRUE} (default), print a detailed version
#'   with requieremens and options instead a one-row-per-type table.
#'   Useful if just one or few types are specified.
#' @param lang Character. Language for type descriptions: \code{"en"}
#'   (default) or \code{"de"}.  Falls back to English when the requested
#'   language has no description.
#'
#' @return An \code{lsh_types_result} object (character vector of resolved LS
#'   type codes) returned \strong{invisibly}.  Assigning the result gives a
#'   plain character vector; printing it shows a compact one-liner.  Pipe into
#'   \code{\link{lsh_options}()} for full option details.
#'
#' @seealso \code{\link{lsh_options}}
#' @export
#'
#' @examples
#' lsh_types()                              # all types, compact and grouped
#' lsh_types("all", details = TRUE)         # detailed table
#' lsh_types("F")                           # one type, compact table
#' lsh_types(c("F", "N"))                   # two types
#' lsh_types("array")                       # label / search term
#' lsh_types("text", lang = "de")           # German descriptions
#' lsh_types(c("F", "N")) |> lsh_options()  # pipe to options
lsh_types <- function(types = "all", details = FALSE, lang = "en") {
  codes <- .lsh_resolve_types(types)

  if (length(codes) == 0L) {
    message(
      "No types matched for: ",
      paste(sprintf("'%s'", types), collapse = ", ")
    )
    return(invisible(.lsh_types_result(character(0L))))
  }

  if (!details) {
    .lsh_print_types_compact(codes, lang)
  } else if (identical(types, "all") || setequal(codes, LS_ALL_TYPE_CODES)) {
    .lsh_print_types_grouped(codes, lang)
  } else {
    .lsh_print_types_detail(codes, lang)
  }

  invisible(.lsh_types_result(codes))
}


#' Describe LimeSurvey attribute options
#'
#' Lists the attribute options available for the selected question type(s),
#' with their defaults, valid values, language-aware descriptions, and the full
#' set of types each option applies to.  Accepts the output of
#' \code{\link{lsh_types}()} directly via the pipe.
#'
#' @details
#' \code{types} is resolved by the same three-stage logic as
#' \code{\link{lsh_types}()}: exact code → label match → substring search.
#' The output table always includes Option, Default, Valid, Description, and
#' Types columns.  The \code{search} argument filters the option list by
#' substring in both option names and descriptions (in all languages).
#'
#' @param types One of:
#'   \itemize{
#'     \item An \code{lsh_types_result} object piped from \code{lsh_types()}
#'     \item \code{"all"} (default) — all options across all types
#'     \item Type code(s) or label(s): \code{"F"}, \code{c("F", "N")},
#'       \code{"array"}, …
#'   }
#' @param search Character. Optional substring filter applied to option names
#'   and descriptions (both en and de).  Works regardless of \code{types}.
#' @param lang Character. Language for option descriptions: \code{"en"}
#'   (default) or \code{"de"}.
#'
#' @return A \code{data.frame} with columns \code{option}, \code{default},
#'   \code{valid}, \code{description}, \code{applies_to}, \code{multilingual},
#'   returned \strong{invisibly}.
#'
#' @seealso \code{\link{lsh_types}}
#' @export
#'
#' @examples
#' lsh_options()                              # all options
#' lsh_options("F")                           # options for array type
#' lsh_options(c("F", "N"))                   # union of options for F and N
#' lsh_options("F", search = "validation")    # filter within F's options
#' lsh_options(search = "time")               # search all options
#' lsh_options(search = "random", lang = "de")  # German descriptions
#' lsh_types(c("F", "N")) |> lsh_options()   # pipe from lsh_types
lsh_options <- function(types = "all", search = NULL, lang = "en") {
  # Resolve types input — pipe-compatible with lsh_types_result
  codes <- if (inherits(types, "lsh_types_result")) {
    as.character(types)
  } else {
    .lsh_resolve_types(types)
  }

  if (length(codes) == 0L) {
    message("No matching types. Use lsh_types() to browse available types.")
    return(invisible(.lsh_opts_df(character(0L), lang)))
  }

  # Union of all applicable options for the selected types
  opt_names <- unique(unlist(
    lapply(codes, function(code) names(LS_TYPES[[code]]$options)),
    use.names = FALSE
  ))

  # Apply search filter — names and descriptions in both languages
  if (!is.null(search)) {
    s_low <- tolower(trimws(search))
    opt_names <- Filter(
      function(nm) {
        opt <- LS_Q_OPTIONS[[nm]]
        any(grepl(
          s_low,
          tolower(c(
            nm,
            opt$description$en %||% "",
            opt$description$de %||% ""
          )),
          fixed = TRUE
        ))
      },
      opt_names
    )

    if (length(opt_names) == 0L) {
      message(sprintf(
        "No options matching '%s' found for the selected type(s).",
        search
      ))
      return(invisible(.lsh_opts_df(character(0L), lang)))
    }
  }

  .lsh_print_options(codes, opt_names, lang)
  invisible(.lsh_opts_df(opt_names, lang))
}
# ══ lsh_settings() — print helpers ════════════════════════════════════════════

.lsh_print_settings <- function(df, is_all, lang) {
  header <- if (is_all) {
    sprintf("All LimeSurvey Survey Settings (%s)", toupper(lang))
  } else {
    sprintf("LimeSurvey Survey Settings [%d matched]", nrow(df))
  }

  cat("\n", header, "\n", .lsh_rule(max(40L, nchar(header))), "\n\n", sep = "")

  if (nrow(df) == 0L) {
    cat("(no settings match)\n\n")
    return(invisible(NULL))
  }

  # Calculate dynamic column widths based on content
  w_name <- max(15L, max(nchar(df$setting))) + 2L
  w_dflt <- max(7L, max(nchar(df$default))) + 2L
  w_valid <- max(10L, max(nchar(df$valid))) + 2L
  w_desc <- 40L # Leave the rest of the space for descriptions

  cat(
    .lsh_pad("Setting", w_name),
    .lsh_pad("Default", w_dflt),
    .lsh_pad("Valid", w_valid),
    "Description\n"
  )
  cat(.lsh_rule(w_name + w_dflt + w_valid + 20L), "\n")

  for (i in seq_len(nrow(df))) {
    cat(
      .lsh_pad(df$setting[i], w_name),
      .lsh_pad(df$default[i], w_dflt),
      .lsh_pad(df$valid[i], w_valid),
      .lsh_trunc(df$description[i], w_desc), # Assuming .lsh_trunc exists in survey_help.R
      "\n"
    )
  }

  cat("\n")
}

#' Describe LimeSurvey survey settings
#'
#' Lists the available survey-level settings, displaying their default values,
#' restricted valid options, and descriptions.
#'
#' @param settings Character vector. Setting names to display, search terms,
#'   or \code{"all"} (default). If a term is not an exact setting name, it is
#'   used as a substring search against names and descriptions.
#' @param lang Character. Language for the descriptions (\code{"en"} or \code{"de"}).
#'
#' @return A \code{data.frame} with columns \code{setting}, \code{default},
#'   \code{valid}, and \code{description}, returned \strong{invisibly}.
#'
#' @export
#'
#' @examples
#' lsh_settings()
#' lsh_settings("format")                # Exact match
#' lsh_settings(c("format", "email"))    # Exact match + search fallback
#' lsh_settings("email", lang = "de")    # Search with German descriptions
lsh_settings <- function(settings = "all", lang = "en") {
  all_settings <- names(LS_SETTINGS)
  is_all <- identical(settings, "all")

  target_settings <- character(0L)

  if (is_all) {
    target_settings <- all_settings
  } else {
    for (term in as.character(settings)) {
      # 1. Check for exact match
      if (term %in% all_settings) {
        target_settings <- c(target_settings, term)
      } else {
        # 2. Fall back to search in names and descriptions
        s_low <- tolower(trimws(term))
        matches <- Filter(
          function(nm) {
            spec <- LS_SETTINGS[[nm]]
            any(grepl(
              s_low,
              tolower(c(
                nm,
                spec$description$en %||% "",
                spec$description$de %||% ""
              )),
              fixed = TRUE
            ))
          },
          all_settings
        )

        if (length(matches) > 0L) {
          target_settings <- c(target_settings, matches)
        } else {
          warning(
            sprintf("No setting or description matched: '%s'", term),
            call. = FALSE
          )
        }
      }
    }
    # Remove any duplicates if search terms overlapped
    target_settings <- unique(target_settings)
  }

  # Build the data frame
  rows <- lapply(target_settings, function(nm) {
    spec <- LS_SETTINGS[[nm]]

    # Format the 'valid' column, handling our .is_num function gracefully
    v_raw <- spec$valid
    v_str <- if (is.function(v_raw)) {
      "<numeric>"
    } else {
      .lsh_fmt_valid(v_raw, max_len = 100L)
    }

    data.frame(
      setting = nm,
      default = as.character(spec$default %||% ""),
      valid = v_str,
      description = spec$description[[lang]] %||% "",
      stringsAsFactors = FALSE
    )
  })

  df <- if (length(rows) > 0L) {
    do.call(rbind, rows)
  } else {
    data.frame(
      setting = character(),
      default = character(),
      valid = character(),
      description = character(),
      stringsAsFactors = FALSE
    )
  }

  .lsh_print_settings(df, is_all, lang)
  invisible(df)
}
