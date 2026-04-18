# survey_template.R -- Template generators for survey authoring scaffolds

.lst_template_result <- function(x, heading, component, add_valid) {
  structure(
    x,
    class = c("lst_template_result", "list"),
    lst_heading = heading,
    lst_component = component,
    lst_add_valid = isTRUE(add_valid)
  )
}

#' @export
print.lst_template_result <- function(x, ...) {
  heading <- attr(x, "lst_heading") %||% "Template"
  component <- attr(x, "lst_component") %||% "settings"
  add_valid <- isTRUE(attr(x, "lst_add_valid"))
  .lst_print_yaml(unclass(x), heading, component, add_valid = add_valid)
  invisible(x)
}

.lst_match_arg <- function(x, choices, arg) {
  if (!is.character(x) || length(x) != 1L || is.na(x)) {
    stop("`", arg, "` must be one of: ", paste(choices, collapse = ", "), ".")
  }
  match.arg(x, choices)
}

.lst_langs <- function(lang) {
  langs <- unique(trimws(as.character(lang)))
  langs <- langs[nzchar(langs)]
  if (length(langs) == 0L || !all(langs %in% c("en", "de"))) {
    stop("`lang` must contain 'en', 'de', or both.")
  }
  langs
}

.lst_text <- function(lang, en, de = en) {
  langs <- .lst_langs(lang)
  values <- list(en = en, de = de)
  if (length(langs) == 1L) {
    return(values[[langs]])
  }
  stats::setNames(lapply(langs, function(code) values[[code]]), langs)
}

.lst_assert_file <- function(file) {
  if (!is.character(file) || length(file) != 1L || !nzchar(file)) {
    stop("`file` must be a non-empty path when YAML output is requested.")
  }
  invisible(file)
}

.lst_is_named_list <- function(x) {
  is.list(x) && length(x) > 0L && !is.null(names(x)) && all(nzchar(names(x)))
}

.lst_yaml_escape <- function(x) {
  x <- enc2utf8(as.character(x))
  x <- gsub("\\\\", "\\\\\\\\", x)
  x <- gsub("\"", "\\\\\"", x)
  x <- gsub("\r\n|\r|\n", "\\\\n", x)
  paste0("\"", x, "\"")
}

.lst_yaml_scalar <- function(x) {
  if (is.null(x)) {
    return("null")
  }
  if (is.logical(x)) {
    return(if (isTRUE(x)) "true" else "false")
  }
  if (is.integer(x)) {
    return(as.character(x))
  }
  if (is.numeric(x)) {
    return(format(x, trim = TRUE, scientific = FALSE))
  }
  if (is.character(x)) {
    return(.lst_yaml_escape(x))
  }
  .lst_yaml_escape(as.character(x))
}

.lst_text_comment <- function(value, multilingual = FALSE) {
  if (.lst_is_named_list(value)) {
    return("multilingual")
  }
  if (isTRUE(multilingual)) {
    return("text (multilingual)")
  }
  "text"
}

.lst_valid_comment <- function(spec, value = NULL, fallback = NULL) {
  if (is.null(spec)) {
    return(fallback %||% .lst_text_comment(value))
  }

  if (isTRUE(spec$language)) {
    return(.lst_text_comment(value, multilingual = TRUE))
  }

  valid <- spec$valid
  if (is.null(valid)) {
    return(fallback %||% .lst_text_comment(value))
  }

  if (is.function(valid)) {
    if (is.integer(spec$default) || is.integer(value)) {
      return("integer")
    }
    if (is.numeric(spec$default) || is.numeric(value)) {
      return("numeric")
    }
    return(fallback %||% .lst_text_comment(value))
  }

  paste0("valid: ", paste(as.character(valid), collapse = " | "))
}

.lst_settings_comment <- function(name, value) {
  if (name %in% names(LS_SETTINGS)) {
    return(.lst_valid_comment(LS_SETTINGS[[name]], value))
  }
  if (name %in% names(LS_SL_SETTINGS)) {
    return(.lst_valid_comment(LS_SL_SETTINGS[[name]], value))
  }
  .lst_text_comment(value)
}

.lst_question_spec <- function(question, name) {
  code <- resolve_question_type("", question)
  if (is.null(code)) {
    return(NULL)
  }
  LS_TYPES[[code]]$options[[name]] %||% NULL
}

.lst_question_comment <- function(question, name, value) {
  if (name %in% c("type", "lsType")) {
    return("text")
  }
  if (name %in% c("answerOptions", "subquestions")) {
    return(NULL)
  }
  if (name %in% c("questionTexts", "helpTexts")) {
    return(.lst_text_comment(value, multilingual = TRUE))
  }
  .lst_valid_comment(.lst_question_spec(question, name), value)
}

.lst_subquestion_comment <- function(name, value, question = NULL) {
  if (identical(name, "subquestionTexts")) {
    return(.lst_text_comment(value, multilingual = TRUE))
  }
  .lst_valid_comment(.lst_question_spec(question, name), value)
}

.lst_group_comment <- function(name, value) {
  if (name %in% c("titles", "descriptions")) {
    return(.lst_text_comment(value, multilingual = TRUE))
  }
  .lst_text_comment(value)
}

.lst_quota_comment <- function(name, value) {
  if (grepl("^quota_", name) || identical(name, "members")) {
    return(NULL)
  }
  spec <- LS_QUOTA_OPTIONS[[name]] %||% NULL
  if (!is.null(spec)) {
    return(.lst_valid_comment(spec, value))
  }
  if (identical(name, "members")) {
    return(NULL)
  }
  .lst_text_comment(value)
}

.lst_comment_for <- function(value, state, name) {
  scope <- state$scope %||% "settings"

  if (scope == "settings") {
    return(.lst_settings_comment(name, value))
  }

  if (scope == "quota") {
    return(.lst_quota_comment(name, value))
  }

  if (scope == "quotaItem") {
    return(.lst_quota_comment(name, value))
  }

  if (scope == "groupOptions") {
    return(.lst_group_comment(name, value))
  }

  if (scope == "question") {
    return(.lst_question_comment(state$question, name, value))
  }

  if (scope == "subquestion") {
    return(.lst_subquestion_comment(name, value, state$question))
  }

  if (scope == "subquestionValue") {
    return(.lst_text_comment(value, multilingual = TRUE))
  }

  if (scope == "answerOptions" || scope == "answerOption") {
    return(.lst_text_comment(value, multilingual = TRUE))
  }

  if (scope == "quotaMembers") {
    return(.lst_text_comment(value))
  }

  NULL
}

.lst_child_state <- function(component, state, name, value) {
  scope <- state$scope %||% component

  if (component == "survey") {
    if (name %in% c("settings", "structure", "quota")) {
      return(list(scope = name, component = name))
    }
    return(list(scope = component, component = component))
  }

  if (component == "structure") {
    if (identical(scope, "structure")) {
      return(list(scope = "group", component = "structure"))
    }
    if (identical(scope, "group")) {
      if (identical(name, "groupOptions")) {
        return(list(scope = "groupOptions", component = "structure"))
      }
      return(list(
        scope = "question",
        component = "structure",
        question = value
      ))
    }
  }

  if (component == "questions") {
    if (identical(scope, "questions")) {
      return(list(
        scope = "question",
        component = "questions",
        question = value
      ))
    }
  }

  if (scope == "question") {
    if (identical(name, "answerOptions")) {
      return(list(
        scope = "answerOptions",
        component = component,
        question = state$question
      ))
    }
    if (identical(name, "subquestions")) {
      return(list(
        scope = "subquestions",
        component = component,
        question = state$question
      ))
    }
  }

  if (scope == "subquestions") {
    return(list(
      scope = if (.lst_is_named_list(value)) {
        "subquestion"
      } else {
        "subquestionValue"
      },
      component = component,
      question = state$question
    ))
  }

  if (scope == "answerOptions") {
    return(list(
      scope = "answerOption",
      component = component,
      question = state$question
    ))
  }

  if (scope == "quota") {
    if (grepl("^quota_", name)) {
      return(list(scope = "quotaItem", component = "quota"))
    }
  }

  if (scope == "quotaItem" && identical(name, "members")) {
    return(list(scope = "quotaMembers", component = "quota"))
  }

  if (scope == "settings") {
    return(list(scope = "settings", component = "settings"))
  }

  state
}

.lst_render_yaml <- function(
  obj,
  component,
  indent = 0L,
  state = NULL,
  add_valid = FALSE
) {
  state <- state %||% list(scope = component, component = component)
  lines <- character(0)
  pad <- paste(rep(" ", indent), collapse = "")

  for (name in names(obj)) {
    value <- obj[[name]]
    comment <- if (isTRUE(add_valid)) {
      .lst_comment_for(value, state, name)
    } else {
      NULL
    }
    comment_suffix <- if (!is.null(comment) && nzchar(comment)) {
      paste0("  # ", comment)
    } else {
      ""
    }

    if (.lst_is_named_list(value)) {
      lines <- c(lines, paste0(pad, name, ":", comment_suffix))
      child_state <- .lst_child_state(component, state, name, value)
      lines <- c(
        lines,
        .lst_render_yaml(
          value,
          component = child_state$component %||% component,
          indent = indent + 2L,
          state = child_state,
          add_valid = add_valid
        )
      )
      next
    }

    lines <- c(
      lines,
      paste0(pad, name, ": ", .lst_yaml_scalar(value), comment_suffix)
    )
  }

  lines
}

.lst_as_yaml <- function(obj, component, add_valid = FALSE) {
  if (!isTRUE(add_valid)) {
    return(yaml::as.yaml(obj))
  }
  paste0(
    paste(.lst_render_yaml(obj, component, add_valid = TRUE), collapse = "\n"),
    "\n"
  )
}

.lst_write_yaml <- function(obj, file, component, add_valid = FALSE) {
  .lst_assert_file(file)

  if (dir.exists(file)) {
    if (identical(component, "survey")) {
      writeLines(
        .lst_as_yaml(obj$settings, "settings", add_valid = add_valid),
        file.path(file, "settings.yml"),
        useBytes = TRUE
      )
      writeLines(
        .lst_as_yaml(obj$structure, "structure", add_valid = add_valid),
        file.path(file, "structure.yml"),
        useBytes = TRUE
      )
      if (!is.null(obj$quota)) {
        writeLines(
          .lst_as_yaml(obj$quota, "quota", add_valid = add_valid),
          file.path(file, "quota.yml"),
          useBytes = TRUE
        )
      }
      return(invisible(obj))
    }

    target <- switch(
      component,
      settings = file.path(file, "settings.yml"),
      structure = file.path(file, "structure.yml"),
      questions = file.path(file, "questions.yml"),
      quota = file.path(file, "quota.yml"),
      file.path(file, paste0(component, ".yml"))
    )
    writeLines(
      .lst_as_yaml(obj, component, add_valid = add_valid),
      target,
      useBytes = TRUE
    )
    return(invisible(obj))
  }

  parent <- dirname(normalizePath(file, mustWork = FALSE))
  if (!dir.exists(parent)) {
    stop("Directory does not exist: ", dirname(file))
  }

  writeLines(
    .lst_as_yaml(obj, component, add_valid = add_valid),
    file,
    useBytes = TRUE
  )
  invisible(obj)
}

.lst_print_yaml <- function(obj, heading, component, add_valid = FALSE) {
  cat(
    "\n",
    heading,
    "\n",
    .lsh_rule(max(30L, nchar(heading))),
    "\n\n",
    sep = ""
  )
  cat(.lst_as_yaml(obj, component, add_valid = add_valid), "\n", sep = "")
  invisible(obj)
}

.lst_emit <- function(obj, path, component, heading, add_valid = FALSE) {
  if (is.null(path)) {
    return(.lst_template_result(obj, heading, component, add_valid))
  }

  .lst_write_yaml(obj, path, component, add_valid = add_valid)
  invisible(obj)
}

.lst_settings_template <- function(example, lang) {
  example <- .lst_match_arg(
    example,
    c("minimal", "simple", "full"),
    "example"
  )
  langs <- .lst_langs(lang)

  fields <- switch(
    example,
    minimal = c("language", "titles"),
    simple = c(
      "language",
      "format",
      "anonymized",
      "admin",
      "adminemail",
      "titles",
      "welcomeTexts",
      "endTexts"
    ),
    full = c(
      names(LS_SETTINGS),
      names(LS_SL_SETTINGS)
    )
  )

  settings <- list(language = langs[[1L]])

  if (length(langs) > 1L) {
    settings$additional_languages <- paste(langs[-1L], collapse = " ")
  }

  for (field in intersect(names(LS_SETTINGS), fields)) {
    if (field %in% c("language", "additional_languages")) {
      next
    }
    settings[[field]] <- LS_SETTINGS[[field]]$default %||% ""
  }

  extra_fields <- setdiff(fields, names(LS_SETTINGS))
  for (field in extra_fields) {
    spec <- LS_SL_SETTINGS[[field]] %||% list()
    if (!is.null(spec$default)) {
      settings[[field]] <- spec$default
      next
    }
    settings[[field]] <- switch(
      field,
      titles = .lst_text(langs, "Survey title", "Umfragetitel"),
      descriptions = .lst_text(
        langs,
        "Short survey description",
        "Kurze Umfragebeschreibung"
      ),
      welcomeTexts = .lst_text(
        langs,
        "Welcome and thank you for taking part.",
        "Willkommen und vielen Dank für Ihre Teilnahme."
      ),
      endTexts = .lst_text(
        langs,
        "Thank you for completing the survey.",
        "Vielen Dank für das Ausfüllen der Umfrage."
      ),
      endURLs = .lst_text(
        langs,
        "https://example.org/thanks",
        "https://example.org/danke"
      ),
      endURLdescriptions = .lst_text(
        langs,
        "More information",
        "Mehr Informationen"
      ),
      policy_notice = .lst_text(
        langs,
        "Please read the privacy notice before continuing.",
        "Bitte lesen Sie vor dem Fortfahren den Datenschutzhinweis."
      ),
      policy_notice_label = .lst_text(
        langs,
        "Privacy notice",
        "Datenschutzhinweis"
      ),
      ""
    )
  }

  settings
}

.lst_example_answer_options <- function(lang) {
  list(
    A1 = .lst_text(lang, "Option 1", "Option 1"),
    A2 = .lst_text(lang, "Option 2", "Option 2")
  )
}

.lst_example_subquestions <- function(lang, example = "minimal") {
  example <- .lst_match_arg(
    example,
    c("minimal", "simple", "full"),
    "example"
  )

  if (identical(example, "minimal")) {
    return(list(
      SQ1 = .lst_text(lang, "Item 1", "Item 1"),
      SQ2 = .lst_text(lang, "Item 2", "Item 2")
    ))
  }

  if (identical(example, "simple")) {
    return(list(
      SQ1 = list(
        subquestionTexts = .lst_text(lang, "Item 1", "Item 1")
      ),
      SQ2 = list(
        subquestionTexts = .lst_text(lang, "Item 2", "Item 2")
      )
    ))
  }

  list(
    SQ1 = list(
      subquestionTexts = .lst_text(lang, "Item 1", "Item 1")
    ),
    SQ2 = list(
      subquestionTexts = .lst_text(lang, "Item 2", "Item 2"),
      relevance = "1"
    )
  )
}

.lst_full_question_options <- function(code, lang) {
  opts <- LS_TYPES[[code]]$options
  opt_names <- setdiff(names(opts), Q_CORE_FIELDS)

  out <- list()
  for (nm in opt_names) {
    spec <- opts[[nm]]
    if (isTRUE(spec$language)) {
      if (!is.null(spec$default)) {
        out[[nm]] <- .lst_text(
          lang,
          as.character(spec$default),
          as.character(spec$default)
        )
      } else {
        out[[nm]] <- .lst_text(lang, "", "")
      }
    } else if (!is.null(spec$default)) {
      out[[nm]] <- spec$default
    } else {
      out[[nm]] <- ""
    }
  }

  out
}

.lst_question_template <- function(code, example = "minimal", lang = "en") {
  example <- .lst_match_arg(example, c("minimal", "simple", "full"), "example")
  label <- LS_QUESTION_TYPES[[code]]$labels[[1L]]
  enriched <- !identical(example, "minimal")
  full <- identical(example, "full")

  q <- list(
    type = label,
    questionTexts = .lst_text(
      lang,
      sprintf("Example question for [%s] %s", code, label),
      sprintf("Beispielfrage für [%s] %s", code, label)
    )
  )

  if (enriched) {
    q$helpTexts <- .lst_text(
      lang,
      "Explain the purpose of this question here.",
      "Erlaeutern Sie hier den Zweck dieser Frage."
    )
    q$mandatory <- "N"
    q$relevance <- "1"
  }

  requires <- LS_TYPES[[code]]$requires

  if ("answerOptions" %in% requires) {
    q$answerOptions <- .lst_example_answer_options(lang)
  }

  if ("subquestions" %in% requires) {
    q$subquestions <- .lst_example_subquestions(lang, example)
  }

  if (full) {
    q <- c(q, .lst_full_question_options(code, lang))
  }

  q
}

.lst_question_block <- function(
  types = "all",
  example = "minimal",
  lang = "en"
) {
  example <- .lst_match_arg(example, c("minimal", "simple", "full"), "example")
  codes <- .lsh_resolve_types(types, quiet = TRUE)
  if (length(codes) == 0L) {
    stop("No question types matched `types`.")
  }

  questions <- list()
  for (i in seq_along(codes)) {
    questions[[paste0("q", i)]] <- .lst_question_template(
      codes[[i]],
      example,
      lang
    )
  }

  questions
}

.lst_grouped_structure <- function(
  questions,
  example = "minimal",
  lang = "en"
) {
  example <- .lst_match_arg(example, c("minimal", "simple", "full"), "example")
  group <- questions

  if (identical(example, "full")) {
    group <- c(
      list(
        groupOptions = list(
          titles = .lst_text(lang, "Example group", "Beispielgruppe"),
          descriptions = .lst_text(
            lang,
            "Group-level notes",
            "Hinweise auf Gruppenebene"
          )
        )
      ),
      group
    )
  }

  list(grp1 = group)
}

.lst_find_quota_member <- function(structure) {
  for (grp_code in names(structure)) {
    grp <- structure[[grp_code]]
    for (q_code in setdiff(names(grp), "groupOptions")) {
      q <- grp[[q_code]]
      code <- resolve_question_type(q_code, q)
      if (is.null(code) || !isTRUE(LS_TYPES[[code]]$quota)) {
        next
      }

      if (code %in% c("A", "B")) {
        sq_code <- names(q$subquestions %||% list())[[1L]]
        ans_code <- names(q$answerOptions %||% list())[[1L]]
        if (!is.null(sq_code) && !is.null(ans_code)) {
          return(list(
            question = q_code,
            member = paste(sq_code, ans_code, sep = "-")
          ))
        }
      }

      ans_code <- names(q$answerOptions %||% list())[[1L]]
      if (!is.null(ans_code)) {
        return(list(question = q_code, member = ans_code))
      }

      sq_code <- names(q$subquestions %||% list())[[1L]]
      if (!is.null(sq_code)) {
        return(list(question = q_code, member = sq_code))
      }
    }
  }

  NULL
}

.lst_quota_template <- function(lang = "en", structure = NULL) {
  member_ref <- if (is.null(structure)) {
    list(question = "q1", member = "A1")
  } else {
    .lst_find_quota_member(structure) %||% list(question = "q1", member = "A1")
  }

  list(
    quota_1 = list(
      limit = 100L,
      members = stats::setNames(list(member_ref$member), member_ref$question),
      action = LS_QUOTA_OPTIONS$action$default,
      active = LS_QUOTA_OPTIONS$active$default,
      autoloadURL = LS_QUOTA_OPTIONS$autoloadURL$default,
      messageTexts = .lst_text(
        lang,
        "Thank you. The quota for this group has already been reached.",
        "Vielen Dank. Die Quote für diese Gruppe wurde bereits erreicht."
      ),
      urls = .lst_text(
        lang,
        "https://example.org/quota-reached",
        "https://example.org/quote-erreicht"
      ),
      urlDescriptions = .lst_text(
        lang,
        "More information",
        "Mehr Informationen"
      )
    )
  )
}

#' List survey settings templates
#'
#' Creates starter templates for the `settings` part of a LimeSeed seed.
#'
#' @param example Character. One of `"minimal"`, `"simple"`, or `"full"`.
#' @param lang Character vector containing `"en"`, `"de"`, or both.
#' @param add_valid Logical. If `TRUE`, printed or written YAML adds inline
#'   comments with valid values or expected input types.
#' @param path Optional output path. May point to a file
#'   or to an existing directory.
#'
#' @return A settings list. When `path` is `NULL`, the object is returned
#'   visibly and prints as YAML when not assigned. When `path` is supplied, YAML
#'   is written and the object is returned invisibly.
#'
#' @examples
#' lst_settings()
#' lst_settings(add_valid = TRUE)
#' x <- lst_settings("simple", lang = c("en", "de"))
#' lst_settings("full", path = tempdir())
#' @export
lst_settings <- function(
  example = "minimal",
  lang = "en",
  add_valid = FALSE,
  path = NULL
) {
  settings <- .lst_settings_template(example, lang)
  .lst_emit(
    settings,
    path,
    "settings",
    "Survey Settings Template",
    add_valid = add_valid
  )
}

#' List question templates
#'
#' Creates starter templates for survey questions without wrapping them in
#' groups. Use [lst_seed()] to build a complete survey seed with group
#' structure. `example = "full"` adds all applicable question option fields for
#' the selected type(s).
#'
#' @param types Character vector of question types, search terms, or `"all"`.
#' @param example Character. One of `"minimal"`, `"simple"`, or `"full"`.
#' @param lang Character vector containing `"en"`, `"de"`, or both.
#' @param add_valid Logical. If `TRUE`, printed or written YAML adds inline
#'   comments with valid values or expected input types.
#' @param path Optional output path. May point to a file
#'   or to an existing directory.
#'
#' @return A named list of questions. When `path` is `NULL`, the object is
#'   returned visibly and prints as YAML when not assigned. When `path` is
#'   supplied, YAML is written and the object is returned invisibly.
#'
#' @examples
#' lst_questions()
#' lst_questions(c("radio", "array"), example = "minimal")
#' lst_questions("radio", example = "simple")
#' lst_questions("radio", example = "full", add_valid = TRUE)
#' qs <- lst_questions("long text", example = "full", lang = "de")
#' lst_questions("radio", path = tempdir())
#' @export
lst_questions <- function(
  types = "all",
  example = "minimal",
  lang = "en",
  add_valid = FALSE,
  path = NULL
) {
  questions <- .lst_question_block(types, example, lang)
  .lst_emit(
    questions,
    path,
    "questions",
    "Question Template",
    add_valid = add_valid
  )
}

#' List quota templates
#'
#' Creates a simple starter template for the optional `quota` section.
#'
#' @param lang Character vector containing `"en"`, `"de"`, or both.
#' @param add_valid Logical. If `TRUE`, printed or written YAML adds inline
#'   comments with valid values or expected input types.
#' @param path Optional output path. May point to a file
#'   or to an existing directory.
#'
#' @return A quota list. When `path` is `NULL`, the object is returned visibly
#'   and prints as YAML when not assigned. When `path` is supplied, YAML is
#'   written and the object is returned invisibly.
#'
#' @examples
#' lst_quota()
#' lst_quota(add_valid = TRUE)
#' q <- lst_quota(lang = c("en", "de"))
#' lst_quota(path = tempdir())
#' @export
lst_quota <- function(lang = "en", add_valid = FALSE, path = NULL) {
  quota <- .lst_quota_template(lang)
  .lst_emit(quota, path, "quota", "Quota Template", add_valid = add_valid)
}

#' List a full seed template
#'
#' Combines `lst_settings()`, `lst_questions()`, and `lst_quota()` into one
#' usable starter seed. `example = "full"` adds all applicable question option
#' fields for the selected type(s).
#'
#' @param example Character. One of `"minimal"`, `"simple"`, or `"full"`. Controls both the
#'   settings and question templates. `"full"` also adds `groupOptions` to the
#'   sample group.
#' @param question_types Character vector of question types, search terms, or
#'   `"all"`.
#' @param quota Logical. Whether to include a quota template.
#' @param lang Character vector containing `"en"`, `"de"`, or both.
#' @param add_valid Logical. If `TRUE`, printed or written YAML adds inline
#'   comments with valid values or expected input types.
#' @param path Optional output path. An existing directory
#'   writes split `settings.yml`, `structure.yml`, and `quota.yml`; otherwise a
#'   single combined YAML file is written.
#'
#' @return A full seed list with `settings`, `structure`, and optional `quota`,
#'   returned visibly when `path` is `NULL` and invisibly otherwise.
#'
#' @examples
#' lst_seed()
#' seed <- lst_seed(
#'   example = "full",
#'   question_types = c("radio", "array"),
#'   quota = TRUE
#' )
#' lst_seed(example = "full", add_valid = TRUE)
#' lst_seed(path = tempdir())
#' @export
lst_seed <- function(
  example = "minimal",
  question_types = c("radio", "long text"),
  quota = FALSE,
  lang = "en",
  add_valid = FALSE,
  path = NULL
) {
  example <- .lst_match_arg(example, c("minimal", "simple", "full"), "example")

  settings <- lst_settings(
    example = example,
    lang = lang,
    add_valid = FALSE,
    path = NULL
  )

  questions <- lst_questions(
    types = question_types,
    example = example,
    lang = lang,
    add_valid = FALSE,
    path = NULL
  )

  structure <- .lst_grouped_structure(questions, example, lang)

  seed <- list(
    settings = settings,
    structure = structure
  )

  if (isTRUE(quota)) {
    seed$quota <- .lst_quota_template(lang, structure)
  }

  .lst_emit(
    seed,
    path,
    "survey",
    "Survey Seed Template",
    add_valid = add_valid
  )
}
