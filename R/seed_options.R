# seed_options.R - Bulk seed-level question option manipulation

#' Bulk-change question options in a seed
#'
#' Applies a named list of question option values to every question whose
#' resolved LimeSurvey type supports that option. Unknown options, unresolved
#' question types, invalid values, and options that do not apply to any question
#' in the seed are reported with warnings. Invalid values are not applied.
#'
#' This modifies the seed before validation/building, not the generated LSDF.
#'
#' @param seed Survey definition in any format accepted by [load_seed()].
#' @param question_options Named list of question options, for example
#'   `list(hide_tip = 1, input_size = 4)`.
#'
#' @return A loaded seed list with applicable question options changed.
#'
#' @examples
#' \dontrun{
#' seed <- set_question_options(
#'   "path/to/survey.yaml",
#'   list(hide_tip = 1, input_size = 4)
#' )
#' }
#'
#' @export
set_question_options <- function(seed, question_options = list()) {
  seed <- load_seed(seed)
  .assert_named_list(question_options, "question_options")

  if (length(question_options) == 0L) {
    return(seed)
  }

  option_names <- names(question_options)
  unknown_options <- setdiff(option_names, names(LS_Q_OPTIONS))
  for (opt_name in unknown_options) {
    warning(
      "`",
      opt_name,
      "` is not a known LimeSurvey question option; skipped.",
      call. = FALSE
    )
  }

  applicable_counts <- stats::setNames(integer(length(option_names)), option_names)

  for (grp_code in names(seed$structure)) {
    qst_codes <- setdiff(names(seed$structure[[grp_code]]), "groupOptions")

    for (qst_code in qst_codes) {
      qst <- seed$structure[[grp_code]][[qst_code]]
      loc_q <- paste0("structure$", grp_code, "$", qst_code)
      ls_type <- resolve_question_type(qst_code, qst)

      if (is.null(ls_type)) {
        warning(
          loc_q,
          ": could not resolve question type; bulk question options skipped.",
          call. = FALSE
        )
        next
      }

      type_opts <- LS_TYPES[[ls_type]]$options %||% list()

      for (opt_name in setdiff(option_names, unknown_options)) {
        opt_spec <- type_opts[[opt_name]]
        if (is.null(opt_spec)) {
          next
        }

        applicable_counts[[opt_name]] <- applicable_counts[[opt_name]] + 1L
        opt_value <- question_options[[opt_name]]

        if (!.question_option_value_is_valid(opt_value, opt_spec)) {
          warning(
            loc_q,
            "$",
            opt_name,
            ": value ",
            .format_option_value(opt_value),
            " is not valid; skipped.",
            call. = FALSE
          )
          next
        }

        qst[[opt_name]] <- opt_value
      }

      seed$structure[[grp_code]][[qst_code]] <- qst
    }
  }

  for (opt_name in setdiff(option_names, unknown_options)) {
    if (identical(applicable_counts[[opt_name]], 0L)) {
      warning(
        "`",
        opt_name,
        "` does not apply to any question in this seed; skipped.",
        call. = FALSE
      )
    }
  }

  seed
}

.assert_named_list <- function(x, arg) {
  if (
    !is.list(x) ||
      (length(x) > 0L &&
        (is.null(names(x)) || any(!nzchar(names(x)))))
  ) {
    stop("`", arg, "` must be a named list.", call. = FALSE)
  }
}

.question_option_value_is_valid <- function(value, opt_spec) {
  valid_spec <- opt_spec$valid
  if (is.null(valid_spec)) {
    return(TRUE)
  }

  vals_to_check <- if (is.list(value)) unlist(value) else value
  if (length(vals_to_check) == 0L) {
    return(FALSE)
  }

  all(vapply(vals_to_check, function(v) {
    tryCatch(
      if (is.function(valid_spec)) {
        isTRUE(valid_spec(v))
      } else {
        as.character(v) %in% as.character(valid_spec)
      },
      error = function(e) FALSE
    )
  }, logical(1)))
}

.format_option_value <- function(value) {
  paste(deparse(value, width.cutoff = 60L), collapse = " ")
}
