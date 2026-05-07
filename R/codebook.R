# codebook.R - Quarto-based codebook generator

#' Generate a LimeSurvey Codebook via Quarto
#'
#' @param df A data.frame from [build_lsdf()] or a path to a LimeSurvey TSV file.
#' @param output_file Character. Default: `"codebook.html"`.
#' @param lang Character vector. `"all"` or specific codes like `c("en", "de")`.
#' @param fields Character vector. Additional fields to include in question
#'   blocks beyond the fixed defaults (`text`, `help`, `type/scale`,
#'   `relevance`), e.g. `"mandatory"` or `"hidden"`. Empty or missing values
#'   are not printed. Named vectors are supported: values are field names and
#'   names are the labels printed in the codebook, e.g.
#'   `c("Filter" = "relevance")`.
#' @param rm_hidden Logical. Remove hidden question variables from the codebook.
#' @param rm_vars Character vector. Question variable names that should not be
#'   reported in the codebook.
#' @param html_mode Character. One of:
#'   - `"render"`: HTML is rendered after script tags are removed.
#'   - `"remove"`: All HTML tags and scripts are stripped.
#'   - `"raw"`: HTML is shown as literal code text.
#' @param keep_qmd Logical. Keep the generated `.qmd` file for manual editing.
#'
#' @return Invisibly, the codebook output path.
#' @export
ls_codebook <- function(
  df,
  output_file = "codebook.html",
  lang = "all",
  fields = NULL,
  rm_hidden = TRUE,
  rm_vars = character(),
  html_mode = c("render", "remove", "raw"),
  keep_qmd = FALSE
) {
  html_mode <- match.arg(html_mode)

  if (is.character(df) && file.exists(df)) {
    df <- utils::read.delim(
      df,
      sep = "\t",
      stringsAsFactors = FALSE,
      quote = "\"",
      na.strings = "",
      check.names = FALSE
    )
  }

  if (!is.data.frame(df)) {
    stop("`df` must be a data frame (output of build_lsdf()).")
  }
  ext <- tolower(tools::file_ext(output_file))
  if (!nzchar(ext)) {
    stop("`output_file` must include a file extension supported by Quarto.")
  }
  if (!requireNamespace("quarto", quietly = TRUE)) {
    stop("Package `quarto` is required to render codebooks.")
  }

  output_dir <- dirname(output_file)
  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (!dir.exists(output_dir)) {
    stop("Could not create output directory: ", output_dir)
  }
  output_dir <- normalizePath(output_dir, mustWork = TRUE)
  output_name <- basename(output_file)
  qmd_file <- paste0(tools::file_path_sans_ext(output_name), ".qmd")
  target_file <- file.path(output_dir, output_name)
  target_qmd <- file.path(output_dir, qmd_file)

  df <- .normalize_codebook_df(df)
  df <- .filter_codebook_df(df, rm_hidden = rm_hidden, rm_vars = rm_vars)
  fields <- .codebook_fields(fields)

  avail_langs <- .available_codebook_languages(df)

  target_langs <- if ("all" %in% lang) {
    avail_langs
  } else {
    intersect(lang, avail_langs)
  }

  if (length(target_langs) == 0L) {
    stop(
      "No matching languages found in the dataframe: ",
      paste(lang, collapse = ", ")
    )
  }

  titles <- df$text[
    df$name == "surveyls_title" & df$language %in% target_langs
  ] |>
    paste(collapse = " / ")

  yaml_title <- gsub('"', '\\"', titles, fixed = TRUE)
  yaml_admin <- gsub('"', '\\"', .setting_value(df, "admin"), fixed = TRUE)
  yaml_email <- gsub('"', '\\"', .setting_value(df, "adminemail"), fixed = TRUE)

  format_lines <- if (identical(ext, "html")) {
    c("format:", "  html:", "    embed-resources: true", "    toc: true")
  } else if (identical(ext, "docx")) {
    c(
      "format:",
      "  docx:",
      sprintf('    reference-doc: "%s"', .yaml_path(.codebook_reference_docx()))
    )
  } else if (identical(ext, "pdf")) {
    "format: typst"
  } else {
    sprintf("format: %s", ext)
  }

  qmd_lines <- c(
    "---",
    sprintf('title: "%s"', yaml_title),
    "author:",
    sprintf('  - name: "%s"', yaml_admin),
    sprintf('    email: "%s"', yaml_email),
    sprintf('date: "%s"', Sys.Date()),
    format_lines,
    "---",
    "",
    .generate_qmd_body(df, target_langs, fields, html_mode)
  )

  qmd_path <- qmd_file
  writeLines(qmd_lines, qmd_path, useBytes = TRUE)

  quarto::quarto_render(input = qmd_file, output_file = output_name)

  .move_codebook_file(output_name, target_file)

  if (keep_qmd) {
    .move_codebook_file(qmd_path, target_qmd)
  } else {
    unlink(qmd_path)
  }
  invisible(output_file)
}


#' Generate a LimeSurvey Codebook from a LimeSeed seed
#'
#' Convenience wrapper that runs [load_seed()], [validate_seed()],
#' [build_lsdf()], and [ls_codebook()] in sequence.
#'
#' @param seed Survey definition in any format accepted by [load_seed()].
#' @param output_file Character. Codebook output path.
#' @param validate Logical. Validate the seed before building.
#' @param ... Additional arguments passed to [ls_codebook()].
#'
#' @return Invisibly, the codebook output path.
#' @export
seed_to_codebook <- function(
  seed,
  output_file = "codebook.html",
  validate = TRUE,
  ...
) {
  seed <- load_seed(seed)
  if (isTRUE(validate)) {
    validate_seed(seed, file = NULL, stop_on_error = TRUE)
  }
  df <- build_lsdf(seed)
  ls_codebook(df, output_file = output_file, ...)
}


.normalize_codebook_df <- function(df) {
  names(df)[names(df) == "type/scale"] <- "type.scale"

  required <- c("class", "name", "text", "language")
  missing <- setdiff(required, names(df))
  if (length(missing) > 0L) {
    stop("`df` is missing required column(s): ", paste(missing, collapse = ", "))
  }

  df[is.na(df)] <- ""
  df[] <- lapply(df, function(col) {
    if (is.character(col)) .strip_outer_quotes(col) else col
  })
  df
}


.strip_outer_quotes <- function(x) {
  quoted <- grepl('^".*"$', x)
  x[quoted] <- substring(x[quoted], 2L, nchar(x[quoted]) - 1L)
  x[quoted] <- gsub('""', '"', x[quoted], fixed = TRUE)
  x
}


.normalize_codebook_fields <- function(fields) {
  if (is.null(fields)) {
    return(character(0L))
  }

  field_names <- names(fields)
  if (is.null(field_names)) {
    field_names <- rep("", length(fields))
  }
  fields <- as.character(fields)

  keep <- !is.na(fields) & nzchar(fields)
  fields <- fields[keep]
  field_names <- field_names[keep]

  fields[fields == "type/scale"] <- "type.scale"

  missing_labels <- is.na(field_names) | !nzchar(field_names)
  field_names[missing_labels] <- vapply(
    fields[missing_labels],
    .field_label,
    character(1L)
  )

  out <- stats::setNames(fields, field_names)
  out[!duplicated(unname(out))]
}


.codebook_default_fields <- function() {
  c(
    "Type" = "type.scale",
    "Filter" = "relevance",
    "Question text" = "text",
    "Help text" = "help"
  )
}


.codebook_fields <- function(fields = NULL) {
  out <- .codebook_default_fields()
  extra <- .normalize_codebook_fields(fields)

  for (i in seq_along(extra)) {
    field <- unname(extra[[i]])
    pos <- match(field, unname(out))
    if (is.na(pos)) {
      out <- c(out, extra[i])
    } else {
      names(out)[pos] <- names(extra)[i]
    }
  }

  out
}


.filter_codebook_df <- function(
  df,
  rm_hidden = TRUE,
  rm_vars = character()
) {
  rm_vars <- unique(as.character(rm_vars %||% character()))
  rm_vars <- rm_vars[!is.na(rm_vars) & nzchar(rm_vars)]

  hidden_vars <- character(0L)
  if (isTRUE(rm_hidden) && "hidden" %in% names(df)) {
    hidden_rows <- df$class == "Q" & .codebook_truthy(df$hidden)
    hidden_vars <- unique(df$name[hidden_rows])
  }

  rm_questions <- unique(c(rm_vars, hidden_vars))
  rm_questions <- rm_questions[!is.na(rm_questions) & nzchar(rm_questions)]
  if (length(rm_questions) == 0L) {
    return(df)
  }

  keep <- rep(TRUE, nrow(df))
  current_question <- ""
  for (i in seq_len(nrow(df))) {
    cls <- df$class[[i]]
    if (identical(cls, "G")) {
      current_question <- ""
    } else if (identical(cls, "Q")) {
      current_question <- df$name[[i]]
    }

    if (
      cls %in% c("Q", "SQ", "A") &&
        nzchar(current_question) &&
        current_question %in% rm_questions
    ) {
      keep[[i]] <- FALSE
    }
  }

  df[keep, , drop = FALSE]
}


.codebook_truthy <- function(x) {
  x <- tolower(trimws(as.character(x)))
  x %in% c("1", "y", "yes", "true", "t")
}


.move_codebook_file <- function(from, to) {
  if (identical(
    normalizePath(from, mustWork = FALSE),
    normalizePath(to, mustWork = FALSE)
  )) {
    return(invisible(TRUE))
  }
  if (file.exists(to)) {
    unlink(to)
  }
  if (!file.rename(from, to)) {
    if (!file.copy(from, to, overwrite = TRUE)) {
      stop("Could not move file to: ", to)
    }
    unlink(from)
  }
  invisible(TRUE)
}


.field_label <- function(field) {
  if (identical(field, "type.scale")) {
    return("Type")
  }
  if (identical(field, "text")) {
    return("Question text")
  }
  if (identical(field, "help")) {
    return("Help text")
  }
  if (identical(field, "relevance")) {
    return("Filter")
  }
  tools::toTitleCase(field)
}


.setting_value <- function(df, name) {
  vals <- df$text[df$class == "S" & df$name == name]
  vals <- vals[!is.na(vals) & nzchar(vals)]
  if (length(vals) == 0L) {
    return("")
  }
  vals[[1L]]
}


.codebook_reference_docx <- function() {
  ref <- system.file("extdata", "codebook-reference.docx", package = "LimeSeed")
  if (nzchar(ref) && file.exists(ref)) {
    return(ref)
  }

  ref <- file.path("inst", "extdata", "codebook-reference.docx")
  if (file.exists(ref)) {
    return(normalizePath(ref, winslash = "/", mustWork = TRUE))
  }

  stop("Could not find bundled codebook Word reference document.")
}


.yaml_path <- function(path) {
  gsub('"', '\\"', normalizePath(path, winslash = "/", mustWork = TRUE), fixed = TRUE)
}


.split_langs <- function(x) {
  x <- x[!is.na(x) & nzchar(x)]
  unique(unlist(strsplit(x, "\\s+"), use.names = FALSE))
}


.available_codebook_languages <- function(df) {
  base <- .setting_value(df, "language")
  extras <- .split_langs(df$text[df$class == "S" & df$name == "additional_languages"])
  row_langs <- unique(df$language[!is.na(df$language) & nzchar(df$language)])
  langs <- unique(c(base, extras, row_langs))
  langs[nzchar(langs)]
}


.process_text <- function(txt, mode) {
  if (is.na(txt) || !nzchar(txt)) {
    return("")
  }

  txt <- gsub("(?is)<script[^>]*>.*?</script>", "", txt, perl = TRUE)

  if (mode == "remove") {
    txt <- gsub("<[^>]+>", "", txt)
  } else if (mode == "raw") {
    txt <- paste0("`", gsub("`", "\\\\`", txt, fixed = TRUE), "`")
  }

  trimws(txt)
}


.generate_qmd_body <- function(
  df,
  target_langs,
  fields,
  mode
) {
  lines <- character(0L)
  df_sub <- df[df$class %in% c("G", "Q", "SQ", "A"), , drop = FALSE]

  if (nrow(df_sub) == 0L) {
    return(lines)
  }

  # build_lsdf() writes the full structure once per language. Group by stable
  # row identity instead of adjacency so multilingual rows render together.
  keys <- .block_keys(df_sub)
  blocks <- split(df_sub, factor(keys, levels = unique(keys)))
  current_section <- NULL

  for (block in blocks) {
    cls <- block$class[1]
    name <- block$name[1]

    if (cls == "G") {
      current_section <- NULL
      lines <- c(lines, "", sprintf("## Group: %s", name), "")
      title <- .extract_multilang(block, "name", target_langs, mode)
      if (nzchar(title) && !identical(title, name)) {
        lines <- c(lines, paste("**Title:**", title), "")
      }
      desc <- .extract_multilang(block, "text", target_langs, mode)
      if (nzchar(desc)) {
        lines <- c(lines, paste("**Description:**", desc), "")
      }
    } else if (cls == "Q") {
      current_section <- NULL
      lines <- c(lines, "", sprintf("### %s", name), "")
      field_labels <- names(fields)
      if (is.null(field_labels)) {
        field_labels <- rep("", length(fields))
      }
      for (i in seq_along(fields)) {
        f <- unname(fields[[i]])
        label <- field_labels[[i]]
        if (!nzchar(label)) {
          label <- .field_label(f)
        }
        lines <- c(
          lines,
          .field_block_lines(block, f, label, target_langs, mode)
        )
      }
    } else if (cls %in% c("SQ", "A")) {
      section <- if (cls == "SQ") "Subquestions" else "Answer options"
      if (!identical(current_section, section)) {
        lines <- c(lines, "", sprintf("**%s:**", section), "")
        current_section <- section
      }
      lines <- c(lines, .choice_block_lines(block, target_langs, mode))
    }
  }

  lines
}


.block_keys <- function(df) {
  id <- if ("id" %in% names(df)) df$id else rep("", nrow(df))
  type_scale <- if ("type.scale" %in% names(df)) {
    df$type.scale
  } else {
    rep("", nrow(df))
  }
  name_key <- ifelse(df$class == "A" | !nzchar(id), df$name, "")
  paste(df$class, id, name_key, type_scale, sep = "\r")
}


.extract_multilang <- function(block, field, target_langs, mode) {
  if (!field %in% colnames(block)) {
    return("")
  }

  rows <- block[
    block$language %in% target_langs &
      !is.na(block[[field]]) &
      block[[field]] != "",
    ,
    drop = FALSE
  ]
  if (nrow(rows) == 0L) {
    return("")
  }

  vals <- vapply(seq_len(nrow(rows)), function(i) {
    .process_text(rows[[field]][i], mode)
  }, character(1L))

  language_fields <- c("text", "help", "other_replace_text", "prefix", "suffix", "default")
  if (
    length(target_langs) > 1L &&
      !field %in% language_fields &&
      length(unique(vals)) == 1L
  ) {
    return(vals[[1L]])
  }

  res <- vapply(seq_len(nrow(rows)), function(i) {
    val <- vals[[i]]
    lang <- rows$language[i]
    if (length(target_langs) > 1L && nzchar(lang)) {
      sprintf("**[%s]** %s", lang, val)
    } else {
      val
    }
  }, character(1L))

  paste(res, collapse = " <br> ")
}


.field_block_lines <- function(
  block,
  field,
  label,
  target_langs,
  mode
) {
  values <- .field_values(block, field, target_langs, mode)
  if (length(values$value) == 0L) {
    return(character(0L))
  }

  if (length(values$value) == 1L && !nzchar(values$language[[1L]])) {
    return(c(sprintf("**%s:** %s", label, values$value[[1L]]), ""))
  }

  c(
    sprintf("**%s:**", label),
    "",
    .multilang_markdown_lines(values, indent = "  "),
    ""
  )
}


.choice_block_lines <- function(block, target_langs, mode) {
  values <- .field_values(block, "text", target_langs, mode)
  relevance <- .field_values(block, "relevance", target_langs, mode)
  rel_value <- ""
  if (length(relevance$value) > 0L) {
    rel_value <- paste(unique(relevance$value), collapse = " / ")
  }

  if (length(values$value) == 0L) {
    lines <- .markdown_hardbreak(sprintf("  **%s**", block$name[[1L]]))
    if (nzchar(rel_value)) {
      lines <- c(lines, sprintf("    **Filter:** %s", rel_value))
    }
    return(c(lines, ""))
  }

  lines <- if (length(values$value) == 1L && !nzchar(values$language[[1L]])) {
    .markdown_hardbreak(sprintf("  **%s:** %s", block$name[[1L]], values$value[[1L]]))
  } else {
    c(
      .markdown_hardbreak(sprintf("  **%s**", block$name[[1L]])),
      .multilang_markdown_lines(values, indent = "    ")
    )
  }

  if (nzchar(rel_value)) {
    lines <- c(lines, sprintf("    **Filter:** %s", rel_value))
  }
  c(lines, "")
}


.multilang_markdown_lines <- function(values, indent = "") {
  vapply(seq_along(values$value), function(i) {
    lang <- values$language[[i]]
    line <- if (nzchar(lang)) {
      sprintf("%s**[%s]** %s", indent, lang, values$value[[i]])
    } else {
      sprintf("%s%s", indent, values$value[[i]])
    }
    .markdown_hardbreak(line)
  }, character(1L))
}


.markdown_hardbreak <- function(x) {
  parts <- strsplit(x, "\n", fixed = TRUE)[[1L]]
  paste0(parts, "  ", collapse = "\n")
}


.field_values <- function(block, field, target_langs, mode) {
  empty <- list(language = character(0L), value = character(0L))
  if (!field %in% colnames(block)) {
    return(empty)
  }

  rows_all <- block[!is.na(block[[field]]) & block[[field]] != "", , drop = FALSE]
  rows <- rows_all[rows_all$language %in% target_langs, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(empty)
  }

  vals <- vapply(seq_len(nrow(rows)), function(i) {
    .format_field_value(field, rows[[field]][i], mode)
  }, character(1L))

  if (identical(field, "relevance") && all(vals == "1")) {
    return(empty)
  }

  if (
    !field %in% c("text", "help", "other_replace_text", "prefix", "suffix", "default") &&
      length(unique(vals)) == 1L
  ) {
    return(list(language = "", value = vals[[1L]]))
  }

  lang <- rows$language
  lang[is.na(lang)] <- ""
  if (length(target_langs) == 1L) {
    lang[] <- ""
  }
  list(language = lang, value = vals)
}


.format_field_value <- function(field, value, mode) {
  value <- .process_text(value, mode)
  if (identical(field, "type.scale")) {
    type_spec <- LS_QUESTION_TYPES[[value]]
    if (!is.null(type_spec) && length(type_spec$labels) > 0L) {
      return(type_spec$labels[[1L]])
    }
  }
  value
}
