# survey_validate.R — Pre-build validation of a LimeSeed sprout
# ─────────────────────────────────────────────────────────────────────────────
# Collect ALL issues in one pass, then report at once.
# "error"   → build aborted
# "warning" → build continues with message
#
# resolve_question_type() lives in survey_defs.R (shared with survey_build.R).

#' Validate a LimeSeed sprout before building
#'
#' @param sprout Named list with `settings` and `structure` elements.
#' @param settings Resolved settings list (output of [resolve_settings()]).
#' @return Invisibly TRUE if valid.
#' @keywords internal
validate_sprout <- function(sprout, settings) {
  issues <- list()

  add <- function(severity, location, msg) {
    issues[[length(issues) + 1L]] <<-
      list(severity = severity, location = location, message = msg)
  }

  validate_settings(sprout$settings, add)
  validate_structure(sprout$structure, settings, add)

  if (!is.null(sprout$quota)) {
    validate_quota(sprout$quota, sprout$structure, add)
  }

  report_issues(issues)
  return(invisible(issues))
}


# ══ Settings ══════════════════════════════════════════════════════════════════

validate_settings <- function(raw, add) {
  # Required fields
  if (is.null(raw$language) || nchar(trimws(raw$language)) == 0) {
    add("error", "settings$language", "is required.")
  }

  if (is.null(raw$titles)) {
    add("error", "settings$titles", "is required.")
  }

  # Y/N fields — derived from SETTINGS_DEFAULTS
  yn_fields <- names(Filter(
    function(v) identical(v, "Y") || identical(v, "N"),
    SETTINGS_DEFAULTS
  ))
  for (f in yn_fields) {
    v <- raw[[f]]
    if (!is.null(v) && !as.character(v) %in% c("Y", "N")) {
      add("warning", paste0("settings$", f),
          paste0("should be \"Y\" or \"N\", got: \"", v, "\"."))
    }
  }

  # Numeric fields
  num_fields <- names(Filter(is.numeric, SETTINGS_DEFAULTS))
  for (f in num_fields) {
    v <- raw[[f]]
    if (!is.null(v) && is.na(suppressWarnings(as.numeric(v)))) {
      add("warning", paste0("settings$", f),
          paste0("should be numeric, got: \"", v, "\"."))
    }
  }

  # Free-string fields
  str_fields <- names(Filter(function(v) identical(v, ""), SETTINGS_DEFAULTS))
  for (f in str_fields) {
    v <- raw[[f]]
    if (!is.null(v) && !is.character(v)) {
      add("warning", paste0("settings$", f),
          paste0("should be character, got class: \"", class(v), "\"."))
    }
  }

  # Enum fields
  for (f in names(SETTINGS_VALID)) {
    v <- raw[[f]]
    if (!is.null(v) && !as.character(v) %in% as.character(SETTINGS_VALID[[f]])) {
      add("warning", paste0("settings$", f),
          paste0("should be one of: ", paste(SETTINGS_VALID[[f]], collapse = ", "),
                 ". Got: \"", v, "\"."))
    }
  }
}


# ══ Structure ════════════════════════════════════════════════════════════════

validate_structure <- function(structure, settings, add) {
  primary_lang <- settings$language
  seen_grps    <- character(0)
  seen_codes   <- character(0)

  if (length(structure) == 0) {
    add("warning", "structure", "Structure contains no groups.")
    return(invisible(NULL))
  }

  for (grp_code in names(structure)) {
    grp_data  <- structure[[grp_code]]
    qst_codes <- setdiff(names(grp_data), "groupOptions")
    loc_g     <- paste0("structure$", grp_code)

    if (grp_code %in% seen_grps) {
      add("error", loc_g, paste0("Group code '", grp_code, "' is used more than once."))
    }
    seen_grps <- c(seen_grps, grp_code)

    if (length(qst_codes) == 0) {
      add("warning", loc_g, paste0("Group '", grp_code, "' has no questions."))
      next
    }

    for (qst_code in qst_codes) {
      qst_data <- grp_data[[qst_code]]
      loc_q    <- paste0(loc_g, "$", qst_code)

      # Unique question code
      if (qst_code %in% seen_codes) {
        add("error", loc_q,
            paste0("Question code '", qst_code, "' is used more than once."))
      }
      seen_codes <- c(seen_codes, qst_code)

      # Code format
      if (!grepl("^[A-Za-z][A-Za-z0-9]*$", qst_code)) {
        add("warning", loc_q,
            paste0("Code '", qst_code,
                   "' should start with a letter and contain only letters and digits."))
      }

      # Type resolution — resolve_question_type() is defined in survey_defs.R
      ls_type <- resolve_question_type(qst_code, qst_data)
      if (is.null(ls_type)) {
        add("error", loc_q,
            paste0("Could not resolve type '",
                   qst_data$type %||% qst_data$lsType %||% "",
                   "'. Use a valid type label or a direct LS type code."))
        next
      }

      type_spec <- LS_TYPES[[ls_type]]

      # Question text
      if (nchar(get_text(qst_data$questionTexts, primary_lang)) == 0) {
        add("warning", loc_q,
            paste0("No question text in primary language ('", primary_lang, "')."))
      }

      # Structural requirements
      for (req in type_spec$requires) {
        if (length(qst_data[[req]] %||% list()) == 0) {
          add("error", loc_q,
              paste0("Type '", ls_type, "' ('", paste(type_spec$labels, collapse = "/"),
                     "') requires '", req, "'."))
        }
      }

      # Structural keys present but not required by this type
      for (struct_key in c("answerOptions", "subquestions")) {
        if (!struct_key %in% type_spec$requires &&
            length(qst_data[[struct_key]] %||% list()) > 0) {
          add("warning", paste0(loc_q, "$", struct_key),
              paste0("Type '", ls_type, "' ('", paste(type_spec$labels, collapse = "/"),
                     "') does not use '", struct_key, "'. It will be ignored."))
        }
      }

      # Validate extra user-supplied options via LS_TYPES[[ls_type]]$options
      user_extras <- qst_data[!names(qst_data) %in% Q_CORE_FIELDS]

      for (opt_name in names(user_extras)) {
        opt_spec <- type_spec$options[[opt_name]]

        if (is.null(opt_spec)) {
          if (opt_name %in% names(LS_Q_OPTIONS)) {
            add("warning", paste0(loc_q, "$", opt_name),
                paste0("'", opt_name, "' is not valid for type '", ls_type,
                       "'. It will be ignored."))
          } else {
            add("warning", paste0(loc_q, "$", opt_name),
                paste0("'", opt_name,
                       "' is not a known LimeSurvey attribute. ",
                       "It will be written to the TSV but may be ignored."))
          }
          next
        }

        # Value check — valid can be a vector of allowed values or a function
        valid_spec <- opt_spec$valid
        if (!is.null(valid_spec)) {
          user_val      <- user_extras[[opt_name]]
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
              add("warning", paste0(loc_q, "$", opt_name),
                  paste0("Value \"", v, "\" is not valid for '", opt_name, "'."))
            }
          }
        }
      }

      # Numerical bounds cross-check
      mn <- qst_data$min_num_value_n
      mx <- qst_data$max_num_value_n
      if (!is.null(mn) && !is.null(mx) &&
          suppressWarnings(as.numeric(mn)) > suppressWarnings(as.numeric(mx))) {
        add("error", loc_q,
            paste0("min_num_value_n (", mn, ") > max_num_value_n (", mx, ")."))
      }

      # Duplicate answer / subquestion codes
      check_dupes <- function(items, label) {
        dupes <- names(items)[duplicated(names(items))]
        if (length(dupes)) {
          add("error", paste0(loc_q, "$", label),
              paste0("Duplicate codes: ", paste(unique(dupes), collapse = ", "), "."))
        }
      }
      check_dupes(qst_data$answerOptions %||% list(), "answerOptions")
      check_dupes(qst_data$subquestions  %||% list(), "subquestions")
    }
  }
}


# ══ Quota ════════════════════════════════════════════════════════════════════

#' Validate Quota definitions in the LimeSeed sprout
#'
#' @param quotas The `$quota` list from the parsed YAML.
#' @param structure The `$structure` list from the parsed YAML.
#' @param add The internal `add(severity, location, msg)` closure.
#' @keywords internal
validate_quota <- function(quotas, structure, add) {
  if (is.null(quotas)) return(invisible(NULL))

  if (!is.list(quotas) || is.null(names(quotas))) {
    add("error", "quota", "must be a named list.")
    return(invisible(NULL))
  }

  # Pre-compute valid answer/subquestion codes per question
  valid_codes_for <- list()
  for (grp in structure) {
    for (q_code in setdiff(names(grp), "groupOptions")) {
      qdata <- grp[[q_code]]
      valid_codes_for[[q_code]] <- c(
        names(qdata$answerOptions %||% list()),
        names(qdata$subquestions  %||% list())
      )
    }
  }

  for (q_name in names(quotas)) {
    q     <- quotas[[q_name]]
    loc_q <- paste0("quota$", q_name)

    # 1. Check required fields and validate values via LS_QUOTA_OPTIONS
    for (field_name in names(LS_QUOTA_OPTIONS)) {
      spec <- LS_QUOTA_OPTIONS[[field_name]]
      val  <- q[[field_name]]

      if (isTRUE(spec$required) && is.null(val)) {
        add("error", loc_q, paste0("missing required field: '", field_name, "'."))
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
          add(severity, paste0(loc_q, "$", field_name),
              paste0("has an invalid value: ", deparse(val)))
        }
      }
    }

    # 2. Unknown keys
    unknown_keys <- setdiff(names(q), names(LS_QUOTA_OPTIONS))
    for (k in unknown_keys) {
      add("warning", loc_q,
          paste0("contains unknown field '", k, "'. It will be ignored."))
    }

    # 3. Deep-check members
    members <- q$members
    if (is.null(members) || !is.list(members)) next   # already caught above

    for (qst_code in names(members)) {
      loc_m <- paste0(loc_q, "$members$", qst_code)

      # 3a. Does the question exist in the structure?
      grp_name <- find_group_of_qst(qst_code, structure)
      if (is.null(grp_name)) {
        add("warning", loc_m,
            sprintf("references question '%s', which does not exist in structure.",
                    qst_code))
        next
      }

      # 3b. Does the question type support quotas?
      qst_data  <- structure[[grp_name]][[qst_code]]
      ls_type   <- resolve_question_type(qst_code, qst_data)
      type_spec <- LS_TYPES[[ls_type]]

      if (!isTRUE(type_spec$quota)) {
        add("warning", loc_m,
            sprintf("Question '%s' (type '%s') does not officially support quotas.",
                    qst_code, ls_type))
      }

      # 3c. Validate each answer code referenced in members
      ans_codes   <- members[[qst_code]]
      valid_codes <- valid_codes_for[[qst_code]]

      # Normalise to a flat character vector (handles single value or vector)
      ans_codes <- as.character(unlist(ans_codes))

      for (ans in ans_codes) {
        # Array types (A, B) use "SubQuestion-Answer" format
        if (ls_type %in% c("A", "B")) {
          if (!grepl("-", ans, fixed = TRUE)) {
            add("error", loc_m,
                sprintf("Array quota member '%s' must use 'SubQuestion-Answer' format (e.g. 'SQ1-A1').",
                        ans))
          }
          # Optionally: check each side against valid_codes
        } else if (!ans %in% valid_codes) {
          add("error", loc_m,
              sprintf("Answer code '%s' is not a valid choice for question '%s'.",
                      ans, qst_code))
        }
      }
    }
  }
}


# ══ Helpers ══════════════════════════════════════════════════════════════════

#' Find the group name that contains a given question code
#' @keywords internal
find_group_of_qst <- function(qst_code, structure) {
  for (grp_name in names(structure)) {
    if (qst_code %in% names(structure[[grp_name]])) return(grp_name)
  }
  NULL
}


# ══ Reporting ════════════════════════════════════════════════════════════════

report_issues <- function(issues) {
  if (length(issues) == 0) {
    message("Validation passed.")
    return(invisible(TRUE))
  }

  errors   <- Filter(function(x) x$severity == "error",   issues)
  warnings <- Filter(function(x) x$severity == "warning", issues)

  fmt <- function(x) paste0("  [", x$severity, "] ", x$location, ": ", x$message)

  if (length(warnings) > 0) {
    warning("Validation warnings:\n",
            paste(sapply(warnings, fmt), collapse = "\n"),
            call. = FALSE)
  }

  if (length(errors) > 0) {
    stop("Validation errors — build aborted:\n",
         paste(sapply(errors, fmt), collapse = "\n"),
         call. = FALSE)
  }

  invisible(TRUE)
}
