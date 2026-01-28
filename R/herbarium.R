#' Generate Survey Codebook (Herbarium)
#'
#' Creates a formatted Word document codebook from a limonaid Survey object,
#' documenting groups, questions, answer options, and metadata.
#'
#' @param tree A `limonaid::Survey` R6 object.
#' @param file Character. Output file path. Default: "codebook.docx"
#' @param lang Character vector. Language codes to include. Use "all" for all
#'   available languages or a subset. Default: "all"
#' @param fields Character vector. Fields to include in output. Options:
#'   "welcome", "policy", "description", "questionTexts", "helpTexts",
#'   "subquestions", "answerOptions", "type", "relevance", "mandatory",
#'   "hidden", "default", "validation", "prefix", "suffix", "other".
#'   Default: c("welcome", "policy", "description", "type", "questionTexts",
#'   "subquestions", "answerOptions", "relevance", "mandatory", "hidden")
#' @param labels Named list. Custom labels for field headings.
#'   Example: list("questionTexts" = "Question", "default" = "Default Value")
#' @param rm_html Logical. Remove HTML tags from text. Default: TRUE
#' @param show_hidden Logical. Include hidden questions in output. Default: TRUE
#'
#' @return Invisibly returns the file path.
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' build_herbarium(tree, "codebook.docx")
#'
#' # Single language with custom fields
#' build_herbarium(tree, "codebook_en.docx", lang = "en",
#'           fields = c("questionTexts", "answerOptions", "mandatory"))
#'
#' # Custom labels
#' build_herbarium(tree, "codebook.docx",
#'           labels = list("default" = "Default Value",
#'                        "relevance" = "Display Logic"))
#' }
#'
#' @import officer
#' @import stringr
#' @export
build_herbarium <- function(
  tree,
  file = "codebook.docx",
  lang = "all",
  fields = c(
    "welcome",
    "policy",
    "description",
    "type",
    "questionTexts",
    "subquestions",
    "answerOptions",
    "relevance",
    "mandatory",
    "hidden"
  ),
  labels = list(),
  rm_html = TRUE,
  show_hidden = TRUE
) {
  # Validate inputs
  if (!inherits(tree, "Survey")) {
    stop("'tree' must be a limonaid::Survey R6 object")
  }

  # Default labels
  default_labels <- list(
    welcome = "Welcome Text",
    policy = "Policy Notice",
    description = "Description",
    questionTexts = "Question Text",
    helpTexts = "Help Text",
    subquestions = "Subquestions",
    answerOptions = "Answer Options",
    type = "Question type",
    relevance = "Filter",
    validation = "Validation",
    default = "Default Value",
    prefix = "Prefix",
    suffix = "Suffix",
    other = "Other Option"
  )

  # Merge custom labels
  labels <- modifyList(default_labels, labels)

  # Initialize document
  doc <- officer::read_docx()

  # Add welcome text if requested
  if ("welcome" %in% fields && !is.null(tree$welcomeTexts)) {
    doc <- .add_text_block(
      doc,
      labels$welcome,
      tree$welcomeTexts,
      rm_html = rm_html,
      lang = lang
    )
  }

  # Add policy notice if requested
  if ("policy" %in% fields && !is.null(tree$policy_notice)) {
    doc <- .add_text_block(
      doc,
      labels$policy,
      tree$policy_notice,
      rm_html = rm_html,
      lang = lang
    )
  }

  # Process groups
  groups <- tree$groups
  for (g in seq_along(groups)) {
    grp <- groups[[g]]

    # Group header
    doc <- officer::body_add_par(doc, grp$title, style = "heading 1")

    # Group description
    if (
      "description" %in%
        fields &&
        !is.null(grp$description) &&
        any(stringr::str_length(grp$description) > 0)
    ) {
      doc <- .add_text_block(
        doc,
        labels$description,
        grp$description,
        rm_html = rm_html,
        lang = lang
      )
    }

    # Group relevance
    if (
      "relevance" %in%
        fields &&
        !is.null(grp$relevance) &&
        grp$relevance != 1
    ) {
      doc <- doc |>
        officer::body_add_par(paste0(labels$relevance, ": ", grp$relevance))
    }

    # Process questions
    questions <- grp$questions
    if (length(questions) > 0) {
      for (q in seq_along(questions)) {
        qobj <- questions[[q]]

        if (
          !show_hidden &&
            !is.null(qobj$otherOptions$hidden) &&
            qobj$otherOptions$hidden == 1
        ) {
          message("Skipping hidden question: ", qobj$code)
          next
        }

        message("Processing question: ", qobj$code)

        doc <- doc |> officer::body_add_par("")
        doc <- doc |> officer::body_add_par(qobj$code, style = "heading 2")

        # Question type with mandatory/hidden in parentheses
        type_line <- .build_type_line(qobj, fields, labels)
        if (!is.null(type_line)) {
          doc <- officer::body_add_par(doc, type_line)
        }

        # Relevance (Filter) on separate line without title
        if (
          "relevance" %in%
            fields &&
            !is.null(qobj$relevance) &&
            qobj$relevance != 1
        ) {
          doc <- doc |>
            officer::body_add_par(paste0(
              labels$relevance,
              ": ",
              qobj$relevance
            ))
        }

        # Validation on separate line without title
        if (
          "validation" %in%
            fields &&
            !is.null(qobj$validation) &&
            nzchar(qobj$validation) &&
            qobj$validation != ""
        ) {
          doc <- doc |>
            officer::body_add_par(paste0(
              labels$validation,
              ": ",
              qobj$validation
            ))
        }

        # Question text
        if (
          "questionTexts" %in%
            fields &&
            !is.null(qobj$questionTexts) &&
            any(stringr::str_length(qobj$questionTexts) > 0)
        ) {
          doc <- doc |> officer::body_add_par("")
          doc <- .add_text_block(
            doc,
            labels$questionTexts,
            qobj$questionTexts,
            rm_html = rm_html,
            lang = lang
          )
        }

        # Help text
        if (
          "helpTexts" %in%
            fields &&
            !is.null(qobj$helpTexts) &&
            any(stringr::str_length(qobj$helpTexts) > 0)
        ) {
          doc <- .add_text_block(
            doc,
            labels$helpTexts,
            qobj$helpTexts,
            rm_html = rm_html,
            lang = lang
          )
        }

        # Prefix
        if (
          "prefix" %in%
            fields &&
            !is.null(qobj$prefix) &&
            any(stringr::str_length(qobj$prefix) > 0)
        ) {
          doc <- .add_text_block(
            doc,
            labels$prefix,
            qobj$prefix,
            rm_html = rm_html,
            lang = lang
          )
        }

        # Suffix
        if (
          "suffix" %in%
            fields &&
            !is.null(qobj$suffix) &&
            any(stringr::str_length(qobj$suffix) > 0)
        ) {
          doc <- .add_text_block(
            doc,
            labels$suffix,
            qobj$suffix,
            rm_html = rm_html,
            lang = lang
          )
        }

        # Default value
        if (
          "default" %in%
            fields &&
            !is.null(qobj$default) &&
            any(stringr::str_length(qobj$default) > 0)
        ) {
          doc <- .add_text_block(
            doc,
            labels$default,
            qobj$default,
            rm_html = rm_html,
            lang = lang
          )
        }

        # Subquestions
        if (
          "subquestions" %in%
            fields &&
            !is.null(qobj$subquestions) &&
            length(qobj$subquestions) > 0
        ) {
          doc <- doc |> officer::body_add_par("")
          doc <- doc |>
            officer::body_add_fpar(
              officer::fpar(officer::ftext(
                labels$subquestions,
                officer::fp_text(bold = TRUE)
              ))
            )

          for (sq in seq_along(qobj$subquestions)) {
            code <- qobj$subquestions[[sq]]$code
            elem <- qobj$subquestions[[sq]]$subquestionTexts
            doc <- .add_text_block(
              doc,
              code,
              elem,
              rm_html = rm_html,
              lang = lang
            )
          }
        }

        # Answer options
        if (
          "answerOptions" %in%
            fields &&
            !is.null(qobj$answerOptions) &&
            length(qobj$answerOptions) > 0
        ) {
          doc <- officer::body_add_par(doc, "")
          doc <- doc |>
            officer::body_add_fpar(
              officer::fpar(officer::ftext(
                labels$answerOptions,
                officer::fp_text(bold = TRUE)
              ))
            )

          for (ao in seq_along(qobj$answerOptions)) {
            code <- qobj$answerOptions[[ao]]$code
            elem <- qobj$answerOptions[[ao]]$optionTexts
            doc <- .add_text_block(
              doc,
              code,
              elem,
              rm_html = rm_html,
              lang = lang
            )
          }
        }

        # Other option
        if ("other" %in% fields && !is.null(qobj$other) && qobj$other == "Y") {
          other_text <- if (!is.null(qobj$otherReplaceTexts)) {
            qobj$otherReplaceTexts
          } else {
            "Yes"
          }
          doc <- .add_text_block(
            doc,
            labels$other,
            other_text,
            rm_html = rm_html,
            lang = lang
          )
        }
      }
    }

    doc <- officer::body_add_par(doc, "")
  }

  # Write document
  print(doc, target = file)
  message("Codebook written to: ", file)

  invisible(file)
}


# Helper: Build question type line with metadata in parentheses
.build_type_line <- function(qobj, fields, labels) {
  if (is.null(qobj$type)) {
    return(NULL)
  }

  # Start with question type
  type_line <- paste0(labels$type, ": ", qobj$type)

  # Build metadata parts for parentheses
  meta_parts <- character(0)

  if (
    "mandatory" %in% fields && !is.null(qobj$mandatory) && qobj$mandatory == "Y"
  ) {
    meta_parts <- c(meta_parts, "mandatory")
  }

  if ("hidden" %in% fields && !is.null(qobj$otherOptions$hidden)) {
    meta_parts <- c(meta_parts, "hidden")
  }

  # Add metadata in parentheses if any exists
  if (length(meta_parts) > 0) {
    type_line <- paste0(
      type_line,
      " (",
      paste(meta_parts, collapse = " & "),
      ")"
    )
  }

  return(type_line)
}


# Helper: Add a text block with language handling
.add_text_block <- function(doc, title, element, rm_html = TRUE, lang = "all") {
  if (is.null(element) || length(element) == 0) {
    return(doc)
  }

  lang_names <- names(element)
  if (is.null(lang_names)) {
    # Handle non-named elements
    if (nzchar(title)) {
      doc <- doc |>
        officer::body_add_fpar(
          officer::fpar(officer::ftext(
            title,
            officer::fp_text(bold = TRUE, font.size = 11)
          ))
        )
    }
    doc <- officer::body_add_par(doc, as.character(element))
    return(doc)
  }

  # Validate language selection
  if (!("all" %in% lang)) {
    lang_diff <- setdiff(lang, lang_names)
    if (length(lang_diff) > 0) {
      warning(
        "Language(s) not found: ",
        paste(lang_diff, collapse = ", "),
        " in available languages: ",
        paste(lang_names, collapse = ", ")
      )
    }
  }

  # Add title
  if (nzchar(title)) {
    doc <- doc |>
      officer::body_add_fpar(
        officer::fpar(officer::ftext(
          title,
          officer::fp_text(bold = TRUE, font.size = 11)
        ))
      )
  }

  # Determine which languages to process
  langs_to_process <- if ("all" %in% lang) {
    lang_names
  } else {
    intersect(lang, lang_names)
  }

  # Add text for each language
  for (lang_code in langs_to_process) {
    txt <- element[[lang_code]]

    if (!is.null(txt) && nzchar(txt)) {
      if (rm_html) {
        txt <- .clean_html(txt)
      }

      # Handle multi-language output
      if (length(langs_to_process) > 1) {
        for (i in seq_along(txt)) {
          line <- if (i == 1) {
            paste0("  [", lang_code, "]\t", txt[i])
          } else {
            txt[i]
          }
          doc <- officer::body_add_par(doc, line)
        }
      } else {
        # Single language - no language tag needed
        for (t in txt) {
          doc <- officer::body_add_par(doc, t)
        }
      }
    }
  }

  return(doc)
}


# Helper: Clean HTML from text
.clean_html <- function(txt) {
  txt |>
    stringr::str_split_1("</p>\\s*<p>|</h2>") |>
    stringr::str_remove_all("<script>.+?</script>") |>
    stringr::str_remove_all("<.+?>") |>
    stringr::str_squish()
}
