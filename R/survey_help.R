# survey_help.R — Documentation and scaffold helpers for LimeSeed surveys
# ─────────────────────────────────────────────────────────────────────────────
#
# Naming convention
# ─────────────────
#   Info / discovery  (print overview + return data.frame invisibly):
#     ls_settings()        — survey-level settings with defaults + valid values
#     ls_questions()       — question types: codes, labels, requirements, options
#     ls_options()         — attribute options for one or more question types
#
#   Scaffold generators  (print YAML + return character vector invisibly):
#     ls_settings_build()  — copy-paste `settings:` block
#     ls_question_build()  — copy-paste question block(s) for one or more types
#     ls_quota_build()     — copy-paste `quota:` entry
#
# All print output goes through cat() so it can be captured via
# capture.output() or written to a file with sink().
#
# Requires: survey_defs.R  (LS_QUESTION_TYPES, LS_Q_OPTIONS, LS_TYPES,
#           LS_LABEL_TO_CODE, LS_ALL_TYPE_CODES, SETTINGS_DEFAULTS,
#           SETTINGS_VALID, Q_CORE_FIELDS, LS_QUOTA_OPTIONS)


# ══ Internal helpers ══════════════════════════════════════════════════════════

# Accumulate printed lines in a buffer while cat()-ing them immediately.
# Returns a list with p() (print one line) and get() (retrieve all lines).
.ls_buf <- function() {
  buf <- character(0)
  list(
    p   = function(x = "") { buf <<- c(buf, x); cat(x, "\n", sep = "") },
    get = function() buf
  )
}

# Resolve the `type` argument → unique character vector of LS type codes.
# Accepts: "all", a single code/label, or a character vector of codes/labels.
.ls_resolve_types <- function(type) {
  if (identical(type, "all")) return(LS_ALL_TYPE_CODES)
  codes <- character(0)
  for (t in as.character(type)) {
    t_low <- tolower(trimws(t))
    if (t %in% LS_ALL_TYPE_CODES) {
      codes <- c(codes, t)
    } else if (t_low %in% names(LS_LABEL_TO_CODE)) {
      codes <- c(codes, LS_LABEL_TO_CODE[[t_low]])
    } else {
      warning(
        "'", t, "' is not a known type code or label — skipped.\n",
        "  Use ls_questions() to browse all available types.",
        call. = FALSE
      )
    }
  }
  unique(codes)
}

# Compact display of a valid-values spec.
.ls_fmt_valid <- function(v) {
  if (is.null(v))     return("any")
  if (is.function(v)) return("(see docs)")
  s <- as.character(v)
  if (length(s) > 8) paste0(paste(s[1:8], collapse = " | "), " | \u2026")
  else               paste(s, collapse = " | ")
}

# Left-pad string to exactly w characters.
.lrpad <- function(x, w) formatC(as.character(x), width = -w, flag = "-")

# Horizontal rule of width w using char.
.rule <- function(w = 68, char = "\u2500") strrep(char, w)

# n levels of YAML indentation (2 spaces each).
.ind <- function(n) strrep("  ", n)

# One YAML line: <indent><key>: <val>  # <comment>
.yl <- function(n, key, val = NULL, comment = NULL) {
  core <- paste0(.ind(n), key, if (!is.null(val)) paste0(": ", val) else ":")
  if (!is.null(comment)) core <- paste0(core, "  # ", comment)
  core
}

# Wrap x in single-quotes (YAML scalar quoting).
.yq <- function(x) paste0("'", x, "'")

# Canonical (first) label for a type code.
.type_label <- function(code) LS_QUESTION_TYPES[[code]]$labels[[1]]


# ══ ls_settings ══════════════════════════════════════════════════════════════

#' List available LimeSurvey survey settings
#'
#' Prints all settings keys recognised by [build_lsdf()] with their package
#' defaults.  When `detail = TRUE`, constrained fields also show their allowed
#' values.  Use `search` to filter by a substring of the field name.
#'
#' @param detail Logical. Show valid-value constraints for each field
#'   (default `FALSE`).
#' @param search Character. Optional case-insensitive substring filter on field
#'   names.
#' @return Invisibly, a `data.frame` with columns `field`, `default`, `valid`.
#' @seealso [ls_settings_build()]
#' @export
#'
#' @examples
#' ls_settings()
#' ls_settings(detail = TRUE)
#' ls_settings(search = "email")
ls_settings <- function(detail = FALSE, search = NULL) {
  buf <- .ls_buf()
  p   <- buf$p

  fields <- names(SETTINGS_DEFAULTS)

  if (!is.null(search)) {
    fields <- fields[grepl(search, fields, ignore.case = TRUE)]
    if (length(fields) == 0) {
      p(paste0("# No settings match '", search, "'."))
      return(invisible(
        data.frame(field = character(), default = character(), valid = character(),
                   stringsAsFactors = FALSE)
      ))
    }
  }

  w_field <- max(nchar(fields)) + 2L
  w_def   <- 18L

  p("# LimeSurvey Survey Settings")
  p(paste0("# ", .rule()))
  p("# Use these keys inside the `settings:` block of your LimeSeed YAML.")
  p("# Required keys not listed here: language, titles")
  p(paste0("# ", .rule()))
  p()

  hdr <- paste0(
    "  ", .lrpad("field", w_field),
    .lrpad("default", w_def),
    if (detail) "valid values" else ""
  )
  p(hdr)
  p(paste0("  ", .rule(nchar(trimws(hdr)) + 4)))

  rows <- lapply(fields, function(field) {
    dflt      <- SETTINGS_DEFAULTS[[field]]
    valid     <- SETTINGS_VALID[[field]]
    dflt_str  <- if (is.null(dflt) || identical(dflt, "")) '""' else paste0('"', dflt, '"')
    valid_str <- if (!is.null(valid)) paste(valid, collapse = " | ") else ""
    valid_col <- if (detail && !is.null(valid)) paste0("[", valid_str, "]") else ""

    p(paste0("  ", .lrpad(field, w_field), .lrpad(dflt_str, w_def), valid_col))

    data.frame(field = field, default = dflt_str, valid = valid_str,
               stringsAsFactors = FALSE)
  })

  p()
  p("# Tip: ls_settings_build() generates a YAML scaffold ready to paste.")

  invisible(do.call(rbind, rows))
}


# ══ ls_questions ══════════════════════════════════════════════════════════════

#' List available LimeSurvey question types
#'
#' Prints an overview of all (or selected) question types: their LS type code,
#' primary label, additional label synonyms, structural requirements, and
#' whether the type supports quota membership.
#'
#' When `detail = TRUE`, each type entry is followed by a table of all
#' applicable attribute options with defaults and valid values.
#'
#' @param type Character. `"all"` (default), a single type code or label, or a
#'   character vector of codes/labels.
#' @param detail Logical. If `TRUE`, also print applicable options per type.
#' @param search Character. Optional case-insensitive substring filter applied
#'   to type codes, labels, and descriptions.
#' @return Invisibly, a `data.frame` with columns `code`, `label`, `labels`,
#'   `requires`, `quota`, `n_options`.
#' @seealso [ls_options()], [ls_question_build()]
#' @export
#'
#' @examples
#' ls_questions()
#' ls_questions("L")
#' ls_questions(c("L", "M", "F"), detail = TRUE)
#' ls_questions(search = "array")
ls_questions <- function(type = "all", detail = FALSE, search = NULL) {
  buf <- .ls_buf()
  p   <- buf$p

  codes <- .ls_resolve_types(type)

  if (!is.null(search)) {
    codes <- Filter(function(code) {
      spec     <- LS_QUESTION_TYPES[[code]]
      haystack <- c(code, spec$labels,
                    spec$description$en %||% "",
                    spec$description$de %||% "")
      any(grepl(search, haystack, ignore.case = TRUE))
    }, codes)
    if (length(codes) == 0) {
      p(paste0("# No question types match '", search, "'."))
      return(invisible(data.frame()))
    }
  }

  p("# LimeSurvey Question Types")
  p(paste0("# ", .rule()))

  # ── compact table ────────────────────────────────────────────────────────
  if (!detail) {
    w_code  <-  6L
    w_label <- 28L
    w_req   <- 28L

    hdr <- paste0(
      .lrpad("Code", w_code),
      .lrpad("Primary label", w_label),
      .lrpad("Requires", w_req),
      "Quota   Aliases"
    )
    p(hdr)
    p(.rule(nchar(hdr)))

    rows <- lapply(codes, function(code) {
      spec    <- LS_QUESTION_TYPES[[code]]
      label   <- spec$labels[[1]]
      n_alias <- length(spec$labels) - 1L
      req_str <- if (length(spec$requires %||% character(0)) == 0) "(none)"
                 else paste(spec$requires, collapse = " + ")
      quota   <- if (isTRUE(spec$quota)) "YES    " else "no     "

      p(paste0(
        .lrpad(paste0("[", code, "]"), w_code),
        .lrpad(label, w_label),
        .lrpad(req_str, w_req),
        quota,
        if (n_alias > 0) paste0("+", n_alias) else ""
      ))

      data.frame(code = code, label = label,
                 labels   = paste(spec$labels, collapse = " | "),
                 requires = req_str,
                 quota    = isTRUE(spec$quota),
                 n_options = length(LS_TYPES[[code]]$options),
                 stringsAsFactors = FALSE)
    })

    p()
    p("# Use ls_questions(detail = TRUE) or ls_options('<code>') for attribute details.")
    p("# Use ls_question_build('<code>') to generate a YAML scaffold.")

  # ── detailed view — one block per type ──────────────────────────────────
  } else {
    rows <- lapply(codes, function(code) {
      spec      <- LS_QUESTION_TYPES[[code]]
      type_spec <- LS_TYPES[[code]]
      label     <- spec$labels[[1]]
      req_str   <- if (length(type_spec$requires) == 0) "(none)"
                   else paste(type_spec$requires, collapse = " + ")
      opts      <- type_spec$options

      dash_len  <- max(2L, 60L - nchar(label) - nchar(code) - 7L)
      p()
      p(paste0("[", code, "] ", label, "  ", .rule(dash_len)))
      p(paste0("  Labels   : ", paste(spec$labels, collapse = " | ")))
      p(paste0("  Requires : ", req_str))
      p(paste0("  Quota    : ", if (type_spec$quota) "YES" else "no"))
      if (!is.null(spec$description$en))
        p(paste0("  Info     : ", spec$description$en))

      if (length(opts) == 0) {
        p("  Options  : (none beyond core fields)")
      } else {
        p(paste0("  Options (", length(opts), "):"))
        w_opt <- min(30L, max(nchar(names(opts)))) + 2L
        for (opt_name in names(opts)) {
          opt      <- opts[[opt_name]]
          dflt_str <- if (!is.null(opt$default)) paste0("default=", .yq(opt$default))
                      else "no default"
          valid_str <- .ls_fmt_valid(opt$valid)
          lang_tag  <- if (isTRUE(opt$language)) " \u2605" else ""
          p(paste0("    ", .lrpad(opt_name, w_opt),
                   .lrpad(dflt_str, 22L), "valid: ", valid_str, lang_tag))
        }
      }

      data.frame(code = code, label = label,
                 labels   = paste(spec$labels, collapse = " | "),
                 requires = req_str,
                 quota    = type_spec$quota,
                 n_options = length(opts),
                 stringsAsFactors = FALSE)
    })

    p()
    p(paste0("# ", .rule()))
    p("# \u2605 = multilingual field: provide as a named list per language")
    p("# Use ls_question_build('<code>') to generate a YAML scaffold.")
  }

  invisible(do.call(rbind, rows))
}


# ══ ls_options ════════════════════════════════════════════════════════════════

#' List attribute options for one or more question types
#'
#' Prints the attribute keys applicable to the given type(s) — the fields
#' that can be set alongside `type:` and `questionTexts:` in a question block.
#' Each entry shows its default value (if any), allowed values, and whether it
#' is a multilingual field.
#'
#' @param type Character. A type code or label, a character vector, or `"all"`
#'   to show the full option registry.
#' @param search Character. Optional case-insensitive substring filter on option
#'   names.
#' @return Invisibly, a `data.frame` with columns `option`, `applies_to`,
#'   `default`, `valid`, `multilingual`.
#' @seealso [ls_questions()], [ls_question_build()]
#' @export
#'
#' @examples
#' ls_options("L")
#' ls_options(c("S", "T", "U"))
#' ls_options(search = "time_limit")
ls_options <- function(type = "all", search = NULL) {
  buf <- .ls_buf()
  p   <- buf$p

  codes <- .ls_resolve_types(type)

  # Collect the union of applicable options for the selected types
  if (identical(type, "all")) {
    all_opts <- LS_Q_OPTIONS
  } else {
    all_opts <- list()
    for (code in codes) {
      for (nm in names(LS_TYPES[[code]]$options)) {
        if (is.null(all_opts[[nm]])) all_opts[[nm]] <- LS_Q_OPTIONS[[nm]]
      }
    }
  }

  if (!is.null(search)) {
    all_opts <- all_opts[grepl(search, names(all_opts), ignore.case = TRUE)]
    if (length(all_opts) == 0) {
      p(paste0("# No options match '", search, "' for the selected type(s)."))
      return(invisible(data.frame()))
    }
  }

  type_str <- if (identical(type, "all")) "all types"
              else paste(vapply(codes, .type_label, character(1)), collapse = ", ")

  p(paste0("# Question Attribute Options \u2014 ", type_str))
  p(paste0("# ", .rule()))
  p("# These keys can be added inside any matching question block in your YAML.")
  p()

  w_opt  <- min(34L, max(nchar(names(all_opts)))) + 2L
  w_dflt <- 16L

  hdr <- paste0(.lrpad("option", w_opt), .lrpad("default", w_dflt), "valid values")
  p(hdr)
  p(.rule(nchar(hdr)))

  rows <- lapply(names(all_opts), function(opt_name) {
    opt       <- all_opts[[opt_name]]
    dflt_str  <- if (!is.null(opt$default)) .yq(opt$default) else "(none)"
    valid_str <- .ls_fmt_valid(opt$valid)
    lang_tag  <- if (isTRUE(opt$language)) " \u2605" else ""
    applies   <- if (is.null(opt$ls_type)) "all" else paste(opt$ls_type, collapse = " ")

    p(paste0(.lrpad(opt_name, w_opt), .lrpad(dflt_str, w_dflt), valid_str, lang_tag))

    data.frame(option = opt_name, applies_to = applies,
               default = dflt_str, valid = valid_str,
               multilingual = isTRUE(opt$language),
               stringsAsFactors = FALSE)
  })

  p()
  p("# \u2605 = multilingual: supply as a named list  e.g.  field: {en: '...', de: '...'}")
  if (!identical(type, "all"))
    p("# Tip: ls_options() without arguments shows the full registry.")

  invisible(do.call(rbind, rows))
}


# ══ ls_question_build ═════════════════════════════════════════════════════════

#' Generate a copy-paste YAML scaffold for one or more question types
#'
#' Prints a ready-to-use YAML skeleton that can be pasted directly into the
#' `structure:` \u203a `<GroupCode>:` block of a LimeSeed YAML file.  Each
#' scaffold is preceded by a short comment describing the type and its requirements.
#'
#' @param type Character. A type code or label, a character vector, or `"all"`
#'   to generate scaffolds for every registered type.
#' @param options Character. Controls the breadth of included fields:
#'   \describe{
#'     \item{`"required"`}{`type` + `questionTexts` + structural fields
#'       (`answerOptions` / `subquestions`) as required by the type.}
#'     \item{`"defaults"`}{Everything in `"required"` plus the most commonly
#'       set core fields with their default values (`mandatory`, `other`,
#'       `relevance`).}
#'     \item{`"all"`}{Everything in `"defaults"` plus every applicable
#'       type-specific attribute option, commented out with its default value
#'       and valid values for easy discovery.}
#'   }
#' @param langs Character vector of language codes for multilingual output
#'   (e.g. `c("en", "de")`), or `NULL` for single-language placeholders.
#' @param code Character. Placeholder question code in the scaffold
#'   (default `"QCODE"`).  When multiple types are requested, the type code is
#'   appended automatically (e.g. `QCODE_L`, `QCODE_M`).
#' @return Invisibly, a character vector of the printed YAML lines.
#' @seealso [ls_questions()], [ls_options()], [ls_settings_build()]
#' @export
#'
#' @examples
#' # Single type — minimal scaffold
#' ls_question_build("L")
#'
#' # Single type — all options shown (commented)
#' ls_question_build("L", options = "all")
#'
#' # Multilingual scaffold
#' ls_question_build("L", langs = c("en", "de"))
#'
#' # Multiple types at once
#' ls_question_build(c("S", "L", "M", "F"))
#'
#' # Capture output to a file
#' lines <- capture.output(ls_question_build("F", options = "defaults", langs = c("en", "de")))
#' writeLines(lines, "question_scaffold.yaml")
ls_question_build <- function(
  type    = "all",
  options = "required",
  langs   = NULL,
  code    = "QCODE"
) {
  options <- match.arg(options, c("required", "defaults", "all"))
  codes   <- .ls_resolve_types(type)

  if (length(codes) == 0) {
    message("No valid types to scaffold.")
    return(invisible(character(0)))
  }

  buf <- .ls_buf()
  p   <- buf$p

  multilang   <- !is.null(langs) && length(langs) > 1
  multi_types <- length(codes) > 1

  for (type_code in codes) {
    spec      <- LS_QUESTION_TYPES[[type_code]]
    type_spec <- LS_TYPES[[type_code]]
    label     <- spec$labels[[1]]
    requires  <- type_spec$requires
    req_str   <- if (length(requires) == 0) "(none)" else paste(requires, collapse = " + ")

    # ── type header comment ─────────────────────────────────────────────────
    p()
    p(paste0("# [", type_code, "] ", label))
    p(paste0("#    Requires : ", req_str,
             "   |   Quota : ", if (type_spec$quota) "YES" else "no"))
    if (!is.null(spec$description$en))
      p(paste0("#    ", spec$description$en))
    p(paste0("#    Synonyms : ", paste(spec$labels, collapse = " | ")))

    # ── question code ────────────────────────────────────────────────────────
    # Sanitise the code: replace characters invalid in YAML plain scalars
    qcode <- if (multi_types) paste0(code, "_", type_code) else code
    qcode <- gsub("[^A-Za-z0-9_]", "X", qcode)   # e.g. "!" → "X"
    p(paste0(qcode, ":"))

    # ── type ─────────────────────────────────────────────────────────────────
    p(.yl(1, "type", .yq(label)))

    # ── questionTexts ─────────────────────────────────────────────────────────
    if (multilang) {
      p(.yl(1, "questionTexts"))
      for (lang in langs)
        p(.yl(2, lang, .yq(paste0("Your question text? (", lang, ")"))))
    } else {
      p(.yl(1, "questionTexts", .yq("Your question text here?")))
    }

    # ── helpTexts (shown only in "all" mode) ─────────────────────────────────
    if (options == "all") {
      if (multilang) {
        p(paste0(.ind(1), "# helpTexts:"))
        for (lang in langs)
          p(paste0(.ind(2), "# ", lang, ": '(optional tooltip text)'"))
      } else {
        p(paste0(.ind(1), "# helpTexts: '(optional tooltip text)'"))
      }
    }

    # ── core fields added in "defaults" and "all" modes ───────────────────────
    if (options %in% c("defaults", "all")) {
      p(.yl(1, "mandatory", .yq("N"), "Y | N | S(oft)"))
      p(.yl(1, "relevance", .yq("1"), "ExpressionScript condition — 1 = always show"))

      if ("other" %in% names(type_spec$options))
        p(.yl(1, "other", .yq("N"), "Y | N — enable a free-text 'Other' option"))
    }

    # ── subquestions (required by the type) ────────────────────────────────────
    if ("subquestions" %in% requires) {
      p(.yl(1, "subquestions"))
      for (i in 1:2) {
        sq <- paste0("SQ", i)
        if (multilang) {
          p(.yl(2, sq))
          p(.yl(3, "subquestionTexts"))
          for (lang in langs)
            p(.yl(4, lang, .yq(paste0("Subquestion ", i, " (", lang, ")"))))
        } else {
          p(.yl(2, sq, .yq(paste0("Subquestion ", i))))
        }
      }
    }

    # ── answerOptions (required by the type) ────────────────────────────────────
    if ("answerOptions" %in% requires) {
      p(.yl(1, "answerOptions"))
      n_opts <- switch(type_code, "1" = 2L, ";" = 2L, ":" = 2L, "H" = 2L, 3L)

      for (i in seq_len(n_opts)) {
        a_code <- paste0("A", i)
        if (multilang) {
          p(.yl(2, a_code))
          p(.yl(3, "optionTexts"))
          for (lang in langs)
            p(.yl(4, lang, .yq(paste0("Option ", i, " (", lang, ")"))))
        } else {
          p(.yl(2, a_code, .yq(paste0("Option ", i))))
        }
      }
    }

    # ── type-specific options (commented out) in "all" mode ────────────────────
    if (options == "all") {
      # Skip fields already written above or handled as core Q-row columns
      skip <- c(Q_CORE_FIELDS, "mandatory", "other", "relevance",
                "random_group", "other_replace_text")
      extra_opts <- setdiff(names(type_spec$options), skip)

      if (length(extra_opts) > 0) {
        p(paste0(.ind(1), "# \u2500\u2500 type-specific options [", type_code, "] ",
                 .rule(max(2L, 38L - nchar(type_code)))))
        for (opt_name in extra_opts) {
          opt       <- type_spec$options[[opt_name]]
          dflt_str  <- if (!is.null(opt$default)) .yq(opt$default) else "''"
          valid_str <- .ls_fmt_valid(opt$valid)
          lang_flag <- if (isTRUE(opt$language)) " [multilingual]" else ""
          p(paste0(.ind(1), "# ",
                   .lrpad(paste0(opt_name, ":"), 32L),
                   .lrpad(dflt_str, 10L),
                   "# valid: ", valid_str, lang_flag))
        }
      }
    }
  }

  p()
  invisible(buf$get())
}


# ══ ls_settings_build ═════════════════════════════════════════════════════════

#' Generate a copy-paste YAML scaffold for the settings block
#'
#' Prints a ready-to-use `settings:` block that can be pasted at the top of a
#' LimeSeed YAML file.  The required keys (`language`, `titles`) are always
#' included.  When `detail = TRUE`, every optional setting is appended as a
#' commented-out line showing its default value and any valid-value constraint.
#'
#' @param lang Character. Primary language code (default `"en"`).
#' @param extra_langs Character vector of additional language codes, or `NULL`.
#' @param detail Logical. If `TRUE`, include all optional settings as comments.
#' @return Invisibly, a character vector of the printed YAML lines.
#' @seealso [ls_settings()]
#' @export
#'
#' @examples
#' ls_settings_build()
#' ls_settings_build("de", extra_langs = "en")
#' ls_settings_build(detail = TRUE)
#' ls_settings_build("en", extra_langs = c("de", "fr"), detail = TRUE)
ls_settings_build <- function(lang = "en", extra_langs = NULL, detail = FALSE) {
  buf <- .ls_buf()
  p   <- buf$p

  all_langs <- c(lang, extra_langs)
  multilang <- length(all_langs) > 1

  p("settings:")
  p(.yl(1, "language", .yq(lang)))

  if (multilang)
    p(.yl(1, "additional_languages", .yq(paste(extra_langs, collapse = " "))))

  # titles
  p()
  if (multilang) {
    p(.yl(1, "titles"))
    for (l in all_langs) p(.yl(2, l, .yq("My Survey Title")))
  } else {
    p(.yl(1, "titles", .yq("My Survey Title")))
  }

  # descriptions (shown when multilingual)
  if (multilang) {
    p(.yl(1, "descriptions"))
    for (l in all_langs) p(.yl(2, l, .yq("")))
  }

  # welcomeTexts / endTexts (shown in all mode)
  p()
  if (multilang) {
    p(paste0(.ind(1), "# welcomeTexts:"))
    for (l in all_langs) p(paste0(.ind(2), "# ", l, ": ''"))
    p(paste0(.ind(1), "# endTexts:"))
    for (l in all_langs) p(paste0(.ind(2), "# ", l, ": ''"))
  } else {
    p(paste0(.ind(1), "# welcomeTexts: ''"))
    p(paste0(.ind(1), "# endTexts: ''"))
  }

  if (detail) {
    p()
    p(paste0(.ind(1), "# \u2500\u2500 Optional settings (defaults shown) ", .rule(32)))

    skip_auto <- c("language", "additional_languages")
    for (field in setdiff(names(SETTINGS_DEFAULTS), skip_auto)) {
      dflt      <- SETTINGS_DEFAULTS[[field]]
      valid     <- SETTINGS_VALID[[field]]
      dflt_str  <- if (is.null(dflt) || identical(dflt, "")) '""'
                   else paste0('"', dflt, '"')
      valid_tag <- if (!is.null(valid))
                     paste0("  # valid: ", paste(valid, collapse = " | "))
                   else ""
      p(paste0(.ind(1), "# ", .lrpad(field, 28L), dflt_str, valid_tag))
    }
  } else {
    p()
    p(paste0(.ind(1), "# Tip: ls_settings_build(detail = TRUE) appends all optional settings."))
    p(paste0(.ind(1), "# Tip: ls_settings(detail = TRUE)       lists them with valid values."))
  }

  p()
  invisible(buf$get())
}


# ══ ls_quota_build ════════════════════════════════════════════════════════════

#' Generate a copy-paste YAML scaffold for a quota definition
#'
#' Prints a ready-to-use quota entry that can be pasted under the top-level
#' `quota:` key of a LimeSeed YAML file.  Required fields (`limit`, `members`)
#' are always included.  When `detail = TRUE`, optional fields are appended as
#' commented-out lines with their defaults and valid values.
#'
#' @param name Character. Placeholder quota name (default `"quota_name"`).
#' @param qst_code Character. Placeholder question code for the `members` block
#'   (default `"QCODE"`).
#' @param langs Character vector of language codes for multilingual text fields,
#'   or `NULL` for single-language placeholders.
#' @param detail Logical. If `TRUE`, include optional quota fields as comments.
#' @return Invisibly, a character vector of the printed YAML lines.
#' @seealso [ls_question_build()]
#' @export
#'
#' @examples
#' ls_quota_build()
#' ls_quota_build("gender_female", qst_code = "Gender", detail = TRUE)
#' ls_quota_build(langs = c("en", "de"), detail = TRUE)
ls_quota_build <- function(
  name     = "quota_name",
  qst_code = "QCODE",
  langs    = NULL,
  detail   = FALSE
) {
  buf <- .ls_buf()
  p   <- buf$p

  multilang <- !is.null(langs) && length(langs) > 1

  p("# \u2500\u2500 Quota definition \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500")
  p("# Place this under the top-level `quota:` key in your LimeSeed YAML.")
  p("# Only question types with Quota = YES support quota membership.")
  p("# Use ls_questions() to check — look for YES in the Quota column.")
  p()
  p("quota:")
  p(paste0(.ind(1), name, ":"))
  p(.yl(2, "limit", "50", "integer \u2265 0 — max completes before quota triggers"))
  p(.yl(2, "members"))
  p(.yl(3, qst_code))
  p(paste0(.ind(4), "- 'A1'  # answer / subquestion code that counts toward this quota"))
  p(paste0(.ind(4), "- 'A2'  # add as many codes as needed"))

  if (detail) {
    p()
    p(paste0(.ind(2), "# \u2500\u2500 optional fields \u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500\u2500"))
    for (opt_name in setdiff(names(LS_QUOTA_OPTIONS), c("limit", "members"))) {
      opt      <- LS_QUOTA_OPTIONS[[opt_name]]
      is_lang  <- isTRUE(opt$language)

      if (is_lang) {
        if (multilang) {
          p(paste0(.ind(2), "# ", opt_name, ":"))
          for (l in langs)
            p(paste0(.ind(3), "# ", l, ": ''"))
        } else {
          p(paste0(.ind(2), "# ", opt_name, ": ''"))
        }
      } else {
        dflt_str  <- if (!is.null(opt$default)) as.character(opt$default) else "''"
        valid_str <- .ls_fmt_valid(opt$valid)
        p(paste0(.ind(2), "# ",
                 .lrpad(paste0(opt_name, ":"), 18L),
                 .lrpad(dflt_str, 6L),
                 "# valid: ", valid_str))
      }
    }
  }

  p()
  invisible(buf$get())
}
