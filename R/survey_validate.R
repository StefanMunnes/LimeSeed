# survey_validate.R — Pre-build validation of a LimeSeed survey definition
# ─────────────────────────────────────────────────────────────────────────────
# Collect ALL issues in one pass, then report at once.
# "error"   → build aborted (after full output)
# "warning" → build continues (after full output)
#
# Reporting strategy
# ──────────────────
# warning() and stop() silently truncate messages beyond getOption("warning.length")
# (default: 1000 characters). To guarantee the full issue list is always visible,
# all detail is printed line-by-line via message() — which never truncates —
# before any warning() or stop() is called. warning()/stop() then carry only a
# short count summary used by tryCatch() callers; the console user has already
# seen everything.
#
# resolve_question_type() lives in survey_defs.R (shared with survey_build.R).

# ══ Public API ════════════════════════════════════════════════════════════════

#' Validate a LimeSeed survey definition
#'
#' Checks a seed for structural and content issues before building.  All issues
#' are collected in a single pass and reported at once — nothing is silently
#' swallowed or truncated.
#'
#' Accepts the same input formats as [load_seed()]: a file path, folder path,
#' named list of paths, or a pre-loaded seed list.
#'
#' @section Output behaviour:
#' Full issue detail is always printed via [message()] — one line per issue —
#' so the console output is never truncated regardless of how many issues are
#' found.  `warning()` and `stop()` carry only a short count summary.
#'
#' After printing:
#' \itemize{
#'   \item **Errors** call [stop()] with a short count summary.
#'     Build cannot proceed.
#'   \item **Warnings only** call [warning()] with a short count summary.
#'     The issues data frame is still returned.
#'   \item **No issues** emit a single pass message and return a zero-row
#'     data frame.
#' }
#'
#' @section Return value:
#' Always returns a **data frame of issues** invisibly — including when
#' validation passes (zero-row data frame).  This lets you capture, filter,
#' and export results without re-running validation.
#'
#' The data frame has three character columns:
#' \describe{
#'   \item{`severity`}{`"error"` or `"warning"`.}
#'   \item{`location`}{Dot-path to the offending element,
#'     e.g. `"structure$G1$Q2$hidden"`.}
#'   \item{`message`}{Human-readable description of the issue.}
#' }
#'
#' @param seed Survey definition in any format accepted by [load_seed()].
#' @param file Optional file path (`character(1)`).  When supplied, the full
#'   validation report is written as plain text in addition to being printed
#'   on the console.  The parent directory must already exist.
#' @param stop_on_error Logical (default `TRUE`).  Set to `FALSE` to demote
#'   errors to warnings so the function always returns without calling
#'   [stop()] — useful in scripts that want to collect all issues
#'   programmatically before deciding how to proceed.
#'
#' @return A data frame of issues (columns: `severity`, `location`, `message`),
#'   returned invisibly.  Zero rows means validation passed.
#'
#' @seealso [load_seed()], [build_lsdf()], [seed_to_tsv()]
#'
#' @examples
#' \dontrun{
#' # ── Standalone use ───────────────────────────────────────────────────────
#'
#' # Validate and let errors stop execution
#' validate_seed("path/to/survey.yaml")
#'
#' # Capture the issues data frame for programmatic inspection
#' issues <- validate_seed("path/to/survey.yaml")
#' issues[issues$severity == "error", ]
#'
#' # Write a persistent plain-text report to disk
#' validate_seed("path/to/survey.yaml", file = "reports/validation.txt")
#'
#' # Export issues as CSV
#' issues <- validate_seed("path/to/survey.yaml", stop_on_error = FALSE)
#' write.csv(issues, "reports/validation.csv", row.names = FALSE)
#'
#' # Collect all issues without stopping — useful in CI / batch scripts
#' issues <- validate_seed("path/to/survey.yaml", stop_on_error = FALSE)
#' if (any(issues$severity == "error")) {
#'   # handle errors programmatically
#' }
#'
#' # ── Pipeline use ─────────────────────────────────────────────────────────
#'
#' # validate_seed() load seed and stops the pipe on errors, passes through on warnings
#'
#' validate_seed("path/to/survey.yaml") |>
#'   build_lsdf() |>
#'   write_lsdf("output/survey.tsv")
#' }
#'
#' @export
validate_seed <- function(seed, file = NULL, stop_on_error = TRUE) {
  seed <- load_seed(seed)
  settings <- resolve_settings(seed$settings)

  issues <- collect_issues(seed, settings)
  df <- issues_to_df(issues)

  report_issues(issues, file = file, stop_on_error = stop_on_error)

  invisible(df)
}


# ══ Internal workhorse ════════════════════════════════════════════════════════

#' Collect all validation issues without printing or stopping
#'
#' Pure collection step — no side effects.  Called by [validate_seed()].
#'
#' @return A list of named lists, each with `severity`, `location`, `message`.
#' @keywords internal
collect_issues <- function(seed, settings) {
  issues <- list()

  add <- function(severity, location, msg) {
    issues[[length(issues) + 1L]] <<-
      list(severity = severity, location = location, message = msg)
  }

  validate_settings(seed$settings, add)
  validate_structure(seed$structure, settings, add)

  if (!is.null(seed$quota)) {
    validate_quota(seed$quota, seed$structure, add)
  }

  issues
}


#' Convert a raw issues list to a tidy data frame
#' @keywords internal
issues_to_df <- function(issues) {
  if (length(issues) == 0) {
    return(data.frame(
      severity = character(0),
      location = character(0),
      message = character(0),
      stringsAsFactors = FALSE
    ))
  }
  data.frame(
    severity = vapply(issues, `[[`, character(1), "severity"),
    location = vapply(issues, `[[`, character(1), "location"),
    message = vapply(issues, `[[`, character(1), "message"),
    stringsAsFactors = FALSE
  )
}


# ══ Settings ══════════════════════════════════════════════════════════════════

#' @keywords internal
validate_settings <- function(raw, add) {
  if (is.null(raw$language) || nchar(trimws(raw$language)) == 0) {
    add("error", "settings$language", "is required.")
  }

  if (is.null(raw$titles)) {
    add("error", "settings$titles", "is required.")
  }

  yn_fields <- names(Filter(
    function(v) identical(v, "Y") || identical(v, "N"),
    SETTINGS_DEFAULTS
  ))
  for (f in yn_fields) {
    v <- raw[[f]]
    if (!is.null(v) && !as.character(v) %in% c("Y", "N")) {
      add(
        "warning",
        paste0("settings$", f),
        paste0("should be \"Y\" or \"N\", got: \"", v, "\".")
      )
    }
  }

  num_fields <- names(Filter(is.numeric, SETTINGS_DEFAULTS))
  for (f in num_fields) {
    v <- raw[[f]]
    if (!is.null(v) && is.na(suppressWarnings(as.numeric(v)))) {
      add(
        "warning",
        paste0("settings$", f),
        paste0("should be numeric, got: \"", v, "\".")
      )
    }
  }

  str_fields <- names(Filter(function(v) identical(v, ""), SETTINGS_DEFAULTS))
  for (f in str_fields) {
    v <- raw[[f]]
    if (!is.null(v) && !is.character(v)) {
      add(
        "warning",
        paste0("settings$", f),
        paste0("should be character, got class: \"", class(v), "\".")
      )
    }
  }

  for (f in names(SETTINGS_VALID)) {
    v <- raw[[f]]
    if (
      !is.null(v) && !as.character(v) %in% as.character(SETTINGS_VALID[[f]])
    ) {
      add(
        "warning",
        paste0("settings$", f),
        paste0(
          "should be one of: ",
          paste(SETTINGS_VALID[[f]], collapse = ", "),
          ". Got: \"",
          v,
          "\"."
        )
      )
    }
  }
}


# ══ Structure ════════════════════════════════════════════════════════════════

#' @keywords internal
validate_structure <- function(structure, settings, add) {
  primary_lang <- settings$language
  seen_grps <- character(0)
  seen_codes <- character(0)

  if (length(structure) == 0) {
    add("warning", "structure", "Structure contains no groups.")
    return(invisible(NULL))
  }

  for (grp_code in names(structure)) {
    grp_data <- structure[[grp_code]]
    qst_codes <- setdiff(names(grp_data), "groupOptions")
    loc_g <- paste0("structure$", grp_code)

    if (grp_code %in% seen_grps) {
      add(
        "error",
        loc_g,
        paste0("Group code '", grp_code, "' is used more than once.")
      )
    }
    seen_grps <- c(seen_grps, grp_code)

    if (length(qst_codes) == 0) {
      add("warning", loc_g, paste0("Group '", grp_code, "' has no questions."))
      next
    }

    for (qst_code in qst_codes) {
      qst_data <- grp_data[[qst_code]]
      loc_q <- paste0(loc_g, "$", qst_code)

      if (qst_code %in% seen_codes) {
        add(
          "error",
          loc_q,
          paste0("Question code '", qst_code, "' is used more than once.")
        )
      }
      seen_codes <- c(seen_codes, qst_code)

      if (!grepl("^[A-Za-z][A-Za-z0-9]*$", qst_code)) {
        add(
          "warning",
          loc_q,
          paste0(
            "Code '",
            qst_code,
            "' should start with a letter and contain only letters and digits."
          )
        )
      }

      ls_type <- resolve_question_type(qst_code, qst_data)
      if (is.null(ls_type)) {
        add(
          "error",
          loc_q,
          paste0(
            "Could not resolve type '",
            qst_data$type %||% qst_data$lsType %||% "",
            "'. Use a valid type label or a direct LS type code."
          )
        )
        next
      }

      type_spec <- LS_TYPES[[ls_type]]

      if (nchar(get_text(qst_data$questionTexts, primary_lang)) == 0) {
        add(
          "warning",
          loc_q,
          paste0("No question text in primary language ('", primary_lang, "').")
        )
      }

      for (req in type_spec$requires) {
        if (length(qst_data[[req]] %||% list()) == 0) {
          add(
            "error",
            loc_q,
            paste0(
              "Type '",
              ls_type,
              "' ('",
              paste(type_spec$labels, collapse = "/"),
              "') requires '",
              req,
              "'."
            )
          )
        }
      }

      for (struct_key in c("answerOptions", "subquestions")) {
        if (
          !struct_key %in% type_spec$requires &&
            length(qst_data[[struct_key]] %||% list()) > 0
        ) {
          add(
            "warning",
            paste0(loc_q, "$", struct_key),
            paste0(
              "Type '",
              ls_type,
              "' ('",
              paste(type_spec$labels, collapse = "/"),
              "') does not use '",
              struct_key,
              "'. It will be ignored."
            )
          )
        }
      }

      user_extras <- qst_data[!names(qst_data) %in% Q_CORE_FIELDS]

      for (opt_name in names(user_extras)) {
        opt_spec <- type_spec$options[[opt_name]]

        if (is.null(opt_spec)) {
          if (opt_name %in% names(LS_Q_OPTIONS)) {
            add(
              "warning",
              paste0(loc_q, "$", opt_name),
              paste0(
                "'",
                opt_name,
                "' is not valid for type '",
                ls_type,
                "'. It will be ignored."
              )
            )
          } else {
            add(
              "warning",
              paste0(loc_q, "$", opt_name),
              paste0(
                "'",
                opt_name,
                "' is not a known LimeSurvey attribute. ",
                "It will be written to the TSV but may be ignored."
              )
            )
          }
          next
        }

        valid_spec <- opt_spec$valid
        if (!is.null(valid_spec)) {
          user_val <- user_extras[[opt_name]]
          vals_to_check <- if (is.list(user_val)) unlist(user_val) else user_val

          for (v in vals_to_check) {
            ok <- tryCatch(
              if (is.function(valid_spec)) {
                isTRUE(valid_spec(v))
              } else {
                as.character(v) %in% as.character(valid_spec)
              },
              error = function(e) FALSE
            )
            if (!ok) {
              add(
                "warning",
                paste0(loc_q, "$", opt_name),
                paste0("Value \"", v, "\" is not valid for '", opt_name, "'.")
              )
            }
          }
        }
      }

      mn <- qst_data$min_num_value_n
      mx <- qst_data$max_num_value_n
      if (
        !is.null(mn) &&
          !is.null(mx) &&
          suppressWarnings(as.numeric(mn)) > suppressWarnings(as.numeric(mx))
      ) {
        add(
          "error",
          loc_q,
          paste0("min_num_value_n (", mn, ") > max_num_value_n (", mx, ").")
        )
      }

      check_dupes <- function(items, label) {
        dupes <- names(items)[duplicated(names(items))]
        if (length(dupes)) {
          add(
            "error",
            paste0(loc_q, "$", label),
            paste0(
              "Duplicate codes: ",
              paste(unique(dupes), collapse = ", "),
              "."
            )
          )
        }
      }
      check_dupes(qst_data$answerOptions %||% list(), "answerOptions")
      check_dupes(qst_data$subquestions %||% list(), "subquestions")
    }
  }
}


# ══ Quota ════════════════════════════════════════════════════════════════════

#' @keywords internal
validate_quota <- function(quotas, structure, add) {
  if (is.null(quotas)) {
    return(invisible(NULL))
  }

  if (!is.list(quotas) || is.null(names(quotas))) {
    add("error", "quota", "must be a named list.")
    return(invisible(NULL))
  }

  valid_codes_for <- list()
  for (grp in structure) {
    for (q_code in setdiff(names(grp), "groupOptions")) {
      qdata <- grp[[q_code]]
      valid_codes_for[[q_code]] <- c(
        names(qdata$answerOptions %||% list()),
        names(qdata$subquestions %||% list())
      )
    }
  }

  for (q_name in names(quotas)) {
    q <- quotas[[q_name]]
    loc_q <- paste0("quota$", q_name)

    for (field_name in names(LS_QUOTA_OPTIONS)) {
      spec <- LS_QUOTA_OPTIONS[[field_name]]
      val <- q[[field_name]]

      if (isTRUE(spec$required) && is.null(val)) {
        add(
          "error",
          loc_q,
          paste0("missing required field: '", field_name, "'.")
        )
        next
      }

      if (!is.null(val) && !is.null(spec$valid)) {
        ok <- tryCatch(
          if (is.function(spec$valid)) {
            isTRUE(spec$valid(val))
          } else {
            as.character(val) %in% as.character(spec$valid)
          },
          error = function(e) FALSE
        )
        if (!ok) {
          severity <- if (isTRUE(spec$required)) "error" else "warning"
          add(
            severity,
            paste0(loc_q, "$", field_name),
            paste0("has an invalid value: ", deparse(val))
          )
        }
      }
    }

    unknown_keys <- setdiff(names(q), names(LS_QUOTA_OPTIONS))
    for (k in unknown_keys) {
      add(
        "warning",
        loc_q,
        paste0("contains unknown field '", k, "'. It will be ignored.")
      )
    }

    members <- q$members
    if (is.null(members) || !is.list(members)) {
      next
    }

    for (qst_code in names(members)) {
      loc_m <- paste0(loc_q, "$members$", qst_code)
      grp_name <- find_group_of_qst(qst_code, structure)

      if (is.null(grp_name)) {
        add(
          "warning",
          loc_m,
          sprintf(
            "references question '%s', which does not exist in structure.",
            qst_code
          )
        )
        next
      }

      qst_data <- structure[[grp_name]][[qst_code]]
      ls_type <- resolve_question_type(qst_code, qst_data)
      type_spec <- LS_TYPES[[ls_type]]

      if (!isTRUE(type_spec$quota)) {
        add(
          "warning",
          loc_m,
          sprintf(
            "Question '%s' (type '%s') does not officially support quotas.",
            qst_code,
            ls_type
          )
        )
      }

      ans_codes <- as.character(unlist(members[[qst_code]]))
      valid_codes <- valid_codes_for[[qst_code]]

      for (ans in ans_codes) {
        if (ls_type %in% c("A", "B")) {
          if (!grepl("-", ans, fixed = TRUE)) {
            add(
              "error",
              loc_m,
              sprintf(
                "Array quota member '%s' must use 'SubQuestion-Answer' format (e.g. 'SQ1-A1').",
                ans
              )
            )
          }
        } else if (!ans %in% valid_codes) {
          add(
            "error",
            loc_m,
            sprintf(
              "Answer code '%s' is not a valid choice for question '%s'.",
              ans,
              qst_code
            )
          )
        }
      }
    }
  }
}


# ══ Helpers ══════════════════════════════════════════════════════════════════

#' Find the group name containing a given question code
#' @keywords internal
find_group_of_qst <- function(qst_code, structure) {
  for (grp_name in names(structure)) {
    if (qst_code %in% names(structure[[grp_name]])) return(grp_name)
  }
  NULL
}


# ══ Reporting ════════════════════════════════════════════════════════════════

#' Format, print, and optionally save all validation issues
#'
#' Prints every issue via [message()] (no truncation), then signals conditions.
#' `warning()` and `stop()` carry only a short count summary so their own
#' truncation limit never matters.
#'
#' @param issues       Raw issues list from [collect_issues()].
#' @param file         Optional file path for a plain-text report.
#' @param stop_on_error Logical; `FALSE` demotes errors to warnings.
#' @keywords internal
report_issues <- function(issues, file = NULL, stop_on_error = TRUE) {
  errors <- Filter(function(x) x$severity == "error", issues)
  warnings <- Filter(function(x) x$severity == "warning", issues)

  # ── Build report lines ────────────────────────────────────────────────────
  rule <- strrep("\u2500", 60)
  ts <- format(Sys.time(), "%Y-%m-%d %H:%M:%S")
  lines <- c(
    paste0("── LimeSeed Validation ", ts, " ", rule)
  )

  if (length(issues) == 0) {
    lines <- c(lines, "\u2713  Validation passed — no issues found.")
  } else {
    n_e <- length(errors)
    n_w <- length(warnings)
    summary_parts <- c(
      if (n_e > 0) paste0(n_e, " error(s)") else NULL,
      if (n_w > 0) paste0(n_w, " warning(s)") else NULL
    )
    lines <- c(lines, paste("  ", paste(summary_parts, collapse = "  |  ")))

    if (n_e > 0) {
      lines <- c(
        lines,
        "",
        paste0("Errors (", n_e, "):"),
        vapply(
          errors,
          function(x) {
            paste0("  [error]   ", x$location, ": ", x$message)
          },
          character(1)
        )
      )
    }
    if (n_w > 0) {
      lines <- c(
        lines,
        "",
        paste0("Warnings (", n_w, "):"),
        vapply(
          warnings,
          function(x) {
            paste0("  [warning] ", x$location, ": ", x$message)
          },
          character(1)
        )
      )
    }
  }

  lines <- c(lines, rule)

  # ── Print via message() — each line separately so nothing is truncated ────
  for (line in lines) {
    message(line)
  }

  # ── Optionally write to file ──────────────────────────────────────────────
  if (!is.null(file)) {
    tryCatch(
      {
        writeLines(lines, con = file)
        message("Validation report written to: ", file)
      },
      error = function(e) {
        warning(
          "Could not write report to '",
          file,
          "': ",
          e$message,
          call. = FALSE
        )
      }
    )
  }

  # ── Signal conditions with short summaries only ───────────────────────────
  # Detail is already on screen — these carry only a terse count so that
  # R's truncation of warning()/stop() strings is never a problem.
  if (length(warnings) > 0 && (length(errors) == 0 || !stop_on_error)) {
    warning(
      length(warnings),
      " validation warning(s) — see console output for details.",
      call. = FALSE
    )
  }

  if (length(errors) > 0) {
    if (stop_on_error) {
      stop(
        length(errors),
        " validation error(s) — build aborted. ",
        "See console output for details.",
        call. = FALSE
      )
    } else {
      warning(
        length(errors),
        " validation error(s) — see console output for details.",
        call. = FALSE
      )
    }
  }

  invisible(TRUE)
}
