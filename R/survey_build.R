# survey_build.R — Row-building functions: settings, language blocks, structure
# ─────────────────────────────────────────────────────────────────────────────
#
# resolve_question_type() is defined in survey_defs.R and shared with
# survey_validate.R — it is NOT redefined here.

# ══ Settings ══════════════════════════════════════════════════════════════════

#' Merge user settings over package defaults
#' @keywords internal
resolve_settings <- function(settings) {
  out <- utils::modifyList(SETTINGS_DEFAULTS, settings)

  if (is.null(settings$additional_languages) && is.list(out$titles)) {
    extra <- setdiff(names(out$titles), out$language)
    out$additional_languages <- paste(extra, collapse = " ")
  } else {
    out$additional_languages <- out$additional_languages %||% ""
  }

  out
}


#' Return all survey languages as a character vector (primary language first)
#' @keywords internal
all_languages <- function(s) {
  extra <- trimws(strsplit(s$additional_languages, "\\s+")[[1]])
  c(s$language, extra[nchar(extra) > 0])
}


#' Build S-rows (global survey settings)
#' Uses names(SETTINGS_DEFAULTS) directly — no separate S_ROW_FIELDS constant.
#' @keywords internal
build_settings_rows <- function(s) {
  lapply(names(SETTINGS_DEFAULTS), function(field) {
    ls_row(class = "S", name = field, text = as.character(s[[field]] %||% ""))
  })
}


#' Build SL-rows (language-specific survey settings) for one language
#' @keywords internal
build_lang_rows <- function(s, lang) {
  e <- EMAIL_DEFAULTS

  sl_fields <- list(
    surveyls_survey_id            = as.character(s$sid),
    surveyls_language             = lang,
    surveyls_title                = get_text(s$titles, lang),
    surveyls_description          = get_text(s$descriptions, lang),
    surveyls_welcometext          = get_text(s$welcomeTexts, lang),
    surveyls_policy_notice        = get_text(s$policy_notice, lang),
    surveyls_policy_notice_label  = get_text(s$policy_notice_label, lang),
    surveyls_endtext              = get_text(s$endTexts, lang),
    surveyls_url                  = get_text(s$endURLs, lang),
    surveyls_urldescription       = get_text(s$endURLdescriptions, lang),
    surveyls_email_invite_subj    = e$surveyls_email_invite_subj,
    surveyls_email_invite         = e$surveyls_email_invite,
    surveyls_email_remind_subj    = e$surveyls_email_remind_subj,
    surveyls_email_remind         = e$surveyls_email_remind,
    surveyls_email_register_subj  = e$surveyls_email_register_subj,
    surveyls_email_register       = e$surveyls_email_register,
    surveyls_email_confirm_subj   = e$surveyls_email_confirm_subj,
    surveyls_email_confirm        = e$surveyls_email_confirm,
    surveyls_dateformat           = as.character(s$dateformats %||% 6),
    email_admin_notification_subj = e$email_admin_notification_subj,
    email_admin_notification      = e$email_admin_notification,
    email_admin_responses_subj    = e$email_admin_responses_subj,
    email_admin_responses         = e$email_admin_responses,
    surveyls_numberformat         = as.character(s$numberformats %||% 0)
  )

  lapply(names(sl_fields), function(field) {
    ls_row(class = "SL", name = field, text = sl_fields[[field]], language = lang)
  })
}


# ══ ID map ════════════════════════════════════════════════════════════════════

#' Pre-compute sequential integer IDs for all structural elements
#'
#' IDs are assigned depth-first: group → questions → subquestions.
#' Answer options reuse their parent question's ID and consume no counter slot.
#'
#' @param structure Named list from `sprout$structure`.
#' @return Named integer vector. Keys: `"G:code"`, `"Q:code"`, `"SQ:qcode:sqcode"`.
#' @keywords internal
build_id_map <- function(structure) {
  ids <- integer(0)
  nxt <- 0L

  new_id <- function(key) {
    nxt <<- nxt + 1L
    ids[key] <<- nxt
  }

  for (grp_code in names(structure)) {
    new_id(paste0("G:", grp_code))
    grp_data  <- structure[[grp_code]]
    qst_codes <- setdiff(names(grp_data), "groupOptions")

    for (qst_code in qst_codes) {
      new_id(paste0("Q:", qst_code))
      qst_data <- grp_data[[qst_code]]
      sqs      <- qst_data$subquestions %||% list()
      for (sq_code in names(sqs)) {
        new_id(paste0("SQ:", qst_code, ":", sq_code))
      }
    }
  }

  ids
}


# ══ Structure rows ════════════════════════════════════════════════════════════

#' Build all G/Q/SQ/A/QTAM rows for one language block
#' @keywords internal
build_structure_rows <- function(
  structure,
  lang,
  id_map,
  primary_lang,
  quotas       = NULL,
  quota_id_map = NULL
) {
  rows <- list()

  for (grp_code in names(structure)) {
    grp_data <- structure[[grp_code]]
    grp_id   <- id_map[[paste0("G:", grp_code)]]
    grp_opts <- grp_data$groupOptions %||% list()

    rows <- c(rows, list(ls_row(
      id         = grp_id,
      class      = "G",
      type.scale = "0",
      name       = get_text_fb(grp_opts$titles %||% grp_code, lang, primary_lang),
      relevance  = as.character(grp_opts$relevance %||% 1),
      text       = get_text_fb(grp_opts$descriptions, lang, primary_lang),
      language   = lang,
      random_group = as.character(grp_opts$random_group %||% "")
    )))

    for (qst_code in setdiff(names(grp_data), "groupOptions")) {
      qst_data <- grp_data[[qst_code]]
      qst_id   <- id_map[[paste0("Q:", qst_code)]]
      rows <- c(
        rows,
        build_question_rows(
          qst_id, qst_code, qst_data,
          lang, primary_lang, id_map,
          quotas, quota_id_map
        )
      )
    }
  }

  rows
}


#' Build Q + SQ/A/QTAM rows for a single question
#'
#' Core Q-row fields (relevance, mandatory, other, validation, same_default,
#' prefix, suffix, default) are written with inline defaults — no Q_ROW_ALWAYS
#' constant is needed.
#'
#' Extra attribute columns are resolved via LS_TYPES[[ls_type]]$options:
#'   1. User provided a value in YAML  → use it
#'   2. Option has a non-NULL default  → write that default
#'   3. NULL default and no user value → skip (column stays blank)
#'
#' QTAM rows are emitted only for the primary language (language-independent
#' per LimeSurvey spec) and placed immediately after all A/SQ rows for this
#' question.  The `id` field of QTAM rows is filled in post-hoc by
#' `build_lsdf()` after all rows are assembled.
#'
#' @keywords internal
build_question_rows <- function(
  qst_id,
  qst_code,
  qst_data,
  lang,
  primary_lang,
  id_map,
  quotas       = NULL,
  quota_id_map = NULL
) {
  rows    <- list()
  ls_type <- resolve_question_type(qst_code, qst_data)

  # ── Q row: core fixed columns ─────────────────────────────────────────────
  q_row <- ls_row(
    id           = qst_id,
    class        = "Q",
    type.scale   = ls_type,
    name         = qst_code,
    relevance    = as.character(qst_data$relevance  %||% "1"),
    text         = get_text_fb(qst_data$questionTexts, lang, primary_lang),
    help         = get_text_fb(qst_data$helpTexts,     lang, primary_lang),
    language     = lang,
    validation   = as.character(qst_data$validation   %||% ""),
    mandatory    = as.character(qst_data$mandatory    %||% "N"),
    other        = as.character(qst_data$other        %||% "N"),
    default      = get_text_fb(qst_data$default,       lang, primary_lang),
    same_default = as.character(qst_data$same_default %||% "0"),
    prefix       = get_text_fb(qst_data$prefix,        lang, primary_lang),
    suffix       = get_text_fb(qst_data$suffix,        lang, primary_lang)
  )

  # ── Q row: extra attribute columns from LS_TYPES[[ls_type]]$options ───────
  type_opts   <- LS_TYPES[[ls_type]]$options %||% list()
  user_extras <- qst_data[!names(qst_data) %in% Q_CORE_FIELDS]

  for (opt_name in names(type_opts)) {
    if (opt_name %in% Q_CORE_FIELDS) next   # already written above

    opt_spec  <- type_opts[[opt_name]]
    user_val  <- user_extras[[opt_name]]
    is_lang   <- isTRUE(opt_spec$language)

    val_to_write <- if (!is.null(user_val)) {
      if (is_lang) get_text_fb(user_val, lang, primary_lang) else as.character(user_val)
    } else if (!is.null(opt_spec$default)) {
      as.character(opt_spec$default)
    } else {
      NULL
    }

    if (!is.null(val_to_write) && opt_name %in% LS_COLUMNS) {
      q_row[[opt_name]] <- val_to_write
    }
  }

  rows <- c(rows, list(q_row))

  # ── SQ rows ───────────────────────────────────────────────────────────────
  sqs <- qst_data$subquestions %||% list()

  for (sq_code in names(sqs)) {
    sq_data <- sqs[[sq_code]]
    sq_id   <- id_map[[paste0("SQ:", qst_code, ":", sq_code)]]

    rows <- c(rows, list(ls_row(
      id           = sq_id,
      class        = "SQ",
      type.scale   = as.character(sq_attr(sq_data, "type.scale", 0)),
      name         = as.character(sq_code),
      relevance    = as.character(sq_attr(sq_data, "relevance", 1)),
      text         = sq_text_fb(sq_data, lang, primary_lang),
      help         = sq_field_fb(sq_data, "helpTexts", lang, primary_lang),
      language     = lang,
      mandatory    = as.character(sq_attr(sq_data, "mandatory", "")),
      default      = sq_field_fb(sq_data, "default", lang, primary_lang),
      same_default = as.character(sq_attr(sq_data, "same_default", ""))
    )))
  }

  # ── A rows ────────────────────────────────────────────────────────────────
  # M-type: answer options are stored as subquestions in LimeSurvey's TSV.
  if (ls_type != "M" && !is.null(qst_data$answerOptions)) {
    for (ans_code in names(qst_data$answerOptions)) {
      ans_data <- qst_data$answerOptions[[ans_code]]
      rows <- c(rows, list(ls_row(
        id         = qst_id,   # A rows reuse the parent question's id
        class      = "A",
        type.scale = as.character(ans_attr(ans_data, "type.scale", 0)),
        name       = as.character(ans_code),
        relevance  = as.character(ans_attr(ans_data, "relevance", "")),
        text       = ans_text_fb(ans_data, lang, primary_lang),
        language   = lang
      )))
    }
  }

  # ── QTAM rows (Quota members) ─────────────────────────────────────────────
  # Spec: language-independent; placed immediately after the question they
  # reference.  Emitted for primary language only to avoid duplication.
  # The `id` field is intentionally left blank here; build_lsdf() fills in
  # sequential QTAM IDs across the whole file after all rows are assembled.

  if (lang == primary_lang && !is.null(quotas)) {
    for (q_name in names(quotas)) {
      q_def <- quotas[[q_name]]

      if (!is.null(q_def$members[[qst_code]])) {
        ans_codes <- as.character(unlist(q_def$members[[qst_code]]))

        for (ans in ans_codes) {
          rows <- c(rows, list(ls_row(
            # id filled in post-hoc by build_lsdf()
            related_id = quota_id_map[[q_name]],
            class      = "QTAM",
            name       = ans
          )))
        }
      }
    }
  }

  rows
}


#' Build QTA and QTALS rows (appended at the end of the TSV)
#' @keywords internal
build_quota_rows <- function(quotas, langs, primary_lang, quota_id_map) {
  rows            <- list()
  qtals_id_counter <- 1L

  for (q_name in names(quotas)) {
    q_id <- quota_id_map[[q_name]]
    q    <- quotas[[q_name]]

    # ── QTA row ──────────────────────────────────────────────────────────────
    # Column mapping (per LimeSurvey TSV spec):
    #   mandatory    → limit
    #   other        → action
    #   default      → active
    #   same_default → autoloadURL
    rows <- c(rows, list(ls_row(
      id           = q_id,
      class        = "QTA",
      name         = q_name,
      mandatory    = as.character(q$limit),
      other        = as.character(q$action       %||% LS_QUOTA_OPTIONS$action$default),
      default      = as.character(q$active       %||% LS_QUOTA_OPTIONS$active$default),
      same_default = as.character(q$autoloadURL  %||% LS_QUOTA_OPTIONS$autoloadURL$default)
    )))

    # ── QTALS rows (one per survey language) ─────────────────────────────────
    # Column mapping (per LimeSurvey TSV spec):
    #   relevance → message text
    #   text      → redirect URL
    #   help      → URL description
    for (lang in langs) {
      rows <- c(rows, list(ls_row(
        id         = qtals_id_counter,
        related_id = q_id,
        class      = "QTALS",
        relevance  = get_text_fb(q$messageTexts,    lang, primary_lang),
        text       = get_text_fb(q$urls,            lang, primary_lang),
        help       = get_text_fb(q$urlDescriptions, lang, primary_lang),
        language   = lang
      )))
      qtals_id_counter <- qtals_id_counter + 1L
    }
  }

  rows
}


# ══ Row constructor ════════════════════════════════════════════════════════════

#' Create a named list for one TSV row (all values coerced to character)
#' @keywords internal
ls_row <- function(...) {
  lapply(list(...), function(x) if (is.null(x)) "" else as.character(x))
}


# ══ Text helpers ══════════════════════════════════════════════════════════════

#' Extract localised text with primary-language fallback
#'
#' Input shapes handled:
#'   NULL              → ""
#'   plain string (unnamed) → returned as-is for every language
#'   named list/vector → lang, then primary_lang, then first non-empty value
#'
#' @keywords internal
get_text_fb <- function(input, lang, primary_lang) {
  if (is.null(input)) return("")

  if (is.null(names(input))) {
    return(trimws(paste(as.character(input), collapse = "")))
  }

  try_get <- function(l) trimws(as.character(input[[l]] %||% ""))

  val <- try_get(lang)
  if (nchar(val) > 0) return(val)

  val <- try_get(primary_lang)
  if (nchar(val) > 0) return(val)

  for (v in input) {
    v <- trimws(as.character(v))
    if (nchar(v) > 0) return(v)
  }

  ""
}


#' Strict variant: return value for `lang` only, no fallback
#'
#' Used for SL-rows where each language block must contain only its own texts.
#' @keywords internal
get_text <- function(input, lang) {
  if (is.null(input))         return("")
  if (is.null(names(input)))  return(trimws(paste(as.character(input), collapse = "")))
  trimws(as.character(input[[lang]] %||% ""))
}


# ── Subquestion helpers ────────────────────────────────────────────────────

#' @keywords internal
sq_text_fb <- function(sq_data, lang, primary_lang) {
  if (is.list(sq_data) && !is.null(sq_data$subquestionTexts)) {
    get_text_fb(sq_data$subquestionTexts, lang, primary_lang)
  } else {
    get_text_fb(sq_data, lang, primary_lang)
  }
}

#' @keywords internal
sq_field_fb <- function(sq_data, field, lang, primary_lang) {
  if (is.list(sq_data)) get_text_fb(sq_data[[field]], lang, primary_lang) else ""
}

#' @keywords internal
sq_attr <- function(sq_data, attr, default) {
  if (is.list(sq_data) && !is.null(sq_data[[attr]])) sq_data[[attr]] else default
}


# ── Answer option helpers ──────────────────────────────────────────────────

#' @keywords internal
ans_text_fb <- function(ans_data, lang, primary_lang) {
  if (is.list(ans_data) && !is.null(ans_data$optionTexts)) {
    get_text_fb(ans_data$optionTexts, lang, primary_lang)
  } else {
    get_text_fb(ans_data, lang, primary_lang)
  }
}

#' @keywords internal
ans_attr <- function(ans_data, attr, default) {
  if (is.list(ans_data) && !is.null(ans_data[[attr]])) ans_data[[attr]] else default
}
