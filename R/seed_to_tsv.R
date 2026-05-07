# seed_to_tsv.R — Two-stage pipeline: seed list → LimeSurvey data frame → TSV
# ─────────────────────────────────────────────────────────────────────────────

#' Load and normalise a LimeSeed survey definition, so called seed
#'
#' Accepts a survey definition in any of five formats and returns a
#' consistently structured named list ready for manipulation
#' or direct use by [build_lsdf()].
#'
#' @param seed_raw Survey definition in one of the following formats:
#'
#'   **1. Single YAML file path** (`character(1)`) \cr
#'   Path to a `.yml` / `.yaml` file whose top-level keys include at least
#'   `settings` and `structure`.  An optional `quota` key is also read if
#'   present.
#'
#'   **2. YAML folder path** (`character(1)`) \cr
#'   Path to a directory containing separate YAML files named
#'   `settings.yml`, `structure.yml`, and optionally `quota.yml`
#'   (`.yaml` extensions are also accepted, case-insensitively).
#'   Files with any other names in the same directory are silently ignored.
#'
#'   **3. Named list of file paths** (`list`) \cr
#'   A named list where every element is a length-1 character string
#'   pointing to a YAML file.  Required names: `"settings"`, `"structure"`.
#'   Optional name: `"quota"`.  Each file is read with [yaml::read_yaml()].
#'
#'   **4. Mixed named list** (`list`) \cr
#'   A named list whose elements are either a length-1 character path to a
#'   YAML file *or* an already-parsed R list.  Useful when one component
#'   (e.g. `structure`) is built programmatically while the other
#'   (e.g. `settings`) is still read from disk.  Required names and
#'   optional names are the same as in case 3.
#'
#'   **5. Pre-loaded seed list** (`list`) \cr
#'   A fully parsed named list that already contains `settings`,
#'   `structure`, and optionally `quota` as R lists.  Returned unchanged.
#'   Passing a seed through `load_seed()` is therefore idempotent and
#'   safe to use in pipelines where the upstream format is not known in
#'   advance.
#'
#' @return A named list — the seed — with the following elements:
#'   \describe{
#'     \item{`settings`}{Named list of survey-level settings.}
#'     \item{`structure`}{Named list of groups, questions, subquestions,
#'       and answer options.}
#'     \item{`quota`}{Named list of quota definitions, or `NULL` when no
#'       quota is defined.}
#'   }
#'
#' @section Errors:
#' `load_seed()` calls [stop()] in the following situations:
#' \itemize{
#'   \item The path in `seed_raw` does not exist.
#'   \item A single-file path does not have a `.yml` or `.yaml` extension.
#'   \item A single YAML file is missing the `settings` or `structure`
#'     top-level keys.
#'   \item A folder contains fewer than two recognised YAML files
#'     (`settings`, `structure`).
#'   \item A list `seed_raw` is unnamed or has any unnamed elements.
#'   \item A list `seed_raw` is missing the `"settings"` or `"structure"` keys.
#'   \item In case 4, a list element is neither a valid file path nor an
#'     R list.
#' }
#'
#' @seealso [validate_seed()] to validate a seed; [build_lsdf()] to convert the seed into a LimeSurvey data
#'   frame; [seed_to_tsv()] for the all-in-one shortcut.
#'
#' @examples
#' \dontrun{
#' # Case 1 — single combined YAML file
#' seed <- load_seed("path/to/survey.yaml")
#'
#' # Case 2 — folder with settings.yml, structure.yml, quota.yml
#' seed <- load_seed("path/to/survey_dir/")
#'
#' # Case 3 — named list of file paths
#' seed <- load_seed(list(
#'   settings  = "path/to/settings.yaml",
#'   structure = "path/to/structure.yaml",
#'   quota     = "path/to/quota.yaml"
#' ))
#'
#' # Case 4 — mixed: one file path, one pre-built R list
#' my_structure <- list(G1 = list(Q1 = list(type = "S", questionTexts = "Name?")))
#' seed <- load_seed(list(
#'   settings  = "path/to/settings.yaml",
#'   structure = my_structure
#' ))
#'
#' # Case 5 — already a seed; returned unchanged (idempotent)
#' seed <- load_seed(seed)
#'
#' # Typical manipulation workflow
#' seed <- load_seed("path/to/survey.yaml")
#' seed$settings$anonymized          <- "N"
#' seed$structure$G1$Q2$relevance    <- "Q1 == 'yes'"
#' df <- build_lsdf(seed)
#' }
#'
#' @importFrom yaml read_yaml
#' @importFrom tools file_path_sans_ext file_ext
#' @export
load_seed <- function(seed_raw) {
  # Internal templates
  seed <- list(settings = NULL, structure = NULL, quota = NULL)

  # --- CASE 1 & 2: Input is a Character Path ---
  if (is.character(seed_raw) && length(seed_raw) == 1) {
    if (dir.exists(seed_raw)) {
      # Case 2: Path to a folder (containing the separated YAML files)

      files <- list.files(
        seed_raw,
        pattern = "\\.yml|\\.yaml",
        ignore.case = TRUE,
        full.names = TRUE
      )

      elements <- basename(files) |> tools::file_path_sans_ext()

      files_valid <- files[elements %in% c("settings", "structure", "quota")]

      if (length(files_valid) < 2) {
        stop(
          "Input folder path does not contain 'settings' and 'structure' files."
        )
      }

      seed <- lapply(files_valid, yaml::read_yaml)

      names(seed) <- basename(files_valid) |> tools::file_path_sans_ext()
    } else if (file.exists(seed_raw)) {
      # Case 1: Path to a single YAML file (containing everything)

      if (tools::file_ext(seed_raw) %in% c("yml", "yaml")) {
        seed <- yaml::read_yaml(seed_raw)
      } else {
        stop("Input file path missing valid .yml or .yaml file extension.")
      }

      if (!all(c("settings", "structure") %in% names(seed))) {
        stop("Input file does not contain 'settings' and 'structure' keys.")
      }
    } else {
      stop("Path specified in 'seed_raw' does not exist.")
    }
  } else if (is.list(seed_raw)) {
    # check if it's a valid named list
    if (is.null(names(seed_raw))) {
      stop("If you provide a list as `seed_raw`, it must be a named.")
    }
    if (!all(nzchar(names(seed_raw)))) {
      stop("If you provide a list as `seed_raw`, all elements must be named.")
    }
    if (!all(c("settings", "structure") %in% names(seed_raw))) {
      stop("Input list does not contain named key 'settings' and 'structure'")
    }

    if (
      all(sapply(seed_raw, class) == "character") &&
        all(lengths(seed_raw) == 1)
    ) {
      # Case 3: It's a named list of paths (Case 3)
      seed <- lapply(seed_raw, yaml::read_yaml)
    } else if (
      any(sapply(seed_raw, class) == "character") &&
        any(sapply(seed_raw, class) == "list")
    ) {
      # Case 4: It's a named list of paths and R lists

      seed <- lapply(seed_raw, function(s) {
        if (is.character(s) && length(s) == 1 && file.exists(s)) {
          yaml::read_yaml(s)
        } else if (is.list(s)) {
          s
        } else {
          stop(
            "When providing a named list, each element must be either a YAML file path or a list."
          )
        }
      })
    } else {
      # Case 5: It's allready a proper list
      seed <- seed_raw
    }
  }

  return(seed)
}


# ══ Stage 1: build ════════════════════════════════════════════════════════════

#' Build the LimeSurvey data frame from a seed list
#'
#' @param seed Named list with `settings` and `structure` elements.
#' @return data.frame with columns matching `LS_COLUMNS` (all character).
#' @seealso [write_lsdf()], [seed_to_tsv()]
#' @export
build_lsdf <- function(seed) {
  if (!is.list(seed) || !all(c("settings", "structure") %in% names(seed))) {
    stop(
      "`seed` must be a named list with 'settings' and 'structure' elements."
    )
  }

  settings <- resolve_settings(seed$settings)
  primary_lang <- settings$language
  langs <- all_languages(settings)

  # validate_seed_internal(seed, settings)

  message(
    "Building survey: '",
    get_text(settings$titles, primary_lang),
    "' [",
    paste(langs, collapse = " + "),
    "]"
  )

  # Pre-compute structural IDs
  id_map <- build_id_map(seed$structure)

  # Pre-compute quota IDs: quota name → sequential integer (1-based)
  # These become the `id` of QTA rows and the `related_id` of QTAM/QTALS rows.
  quota_id_map <- if (!is.null(seed$quota)) {
    stats::setNames(seq_along(seed$quota), names(seed$quota))
  } else {
    NULL
  }

  # ── S rows (global settings) ──────────────────────────────────────────────
  rows <- build_settings_rows(settings)

  # ── SL rows (per-language survey settings) ────────────────────────────────
  for (lang in langs) {
    rows <- c(rows, build_lang_rows(settings, lang))
  }

  # ── G / Q / SQ / A / QTAM rows ───────────────────────────────────────────
  # Structure rows are built per language. QTAM rows are emitted only for the
  # primary language (inside build_question_rows) and placed immediately after
  # their parent question.
  for (lang in langs) {
    rows <- c(
      rows,
      build_structure_rows(
        seed$structure,
        lang,
        id_map,
        primary_lang,
        seed$quota,
        quota_id_map
      )
    )
  }

  # ── Number QTAM rows sequentially ────────────────────────────────────────
  # QTAM `id` must be a unique integer starting at 1.  We fill this in here
  # rather than inside build_question_rows() because the counter must be global
  # across all questions — it is not scoped per question or per group.
  qtam_n <- 0L
  for (i in seq_along(rows)) {
    if (identical(rows[[i]]$class, "QTAM")) {
      qtam_n <- qtam_n + 1L
      rows[[i]]$id <- as.character(qtam_n)
    }
  }

  # ── QTA / QTALS rows (appended at end of file) ───────────────────────────
  if (!is.null(seed$quota)) {
    rows <- c(
      rows,
      build_quota_rows(seed$quota, langs, primary_lang, quota_id_map)
    )
  }

  df <- rows_to_df(rows)

  message(
    "Done: ",
    nrow(df),
    " rows | ",
    sum(df$class == "G"),
    " group(s) | ",
    sum(df$class == "Q") / length(langs),
    " question(s) | ",
    length(langs),
    " language(s)"
  )

  df
}


# ══ Stage 2: write ════════════════════════════════════════════════════════════

#' Write a LimeSurvey data frame to a TSV import file
#'
#' Format-only: applies LimeSurvey quoting rules and writes UTF-8 TSV.
#' Has no knowledge of seed structure.
#'
#' @param df data.frame from [build_lsdf()].
#' @param file Destination `.tsv` file path.
#' @return Invisibly, the data frame as written.
#' @seealso [build_lsdf()], [seed_to_tsv()]
#' @export
write_lsdf <- function(df, file) {
  if (!is.data.frame(df)) {
    stop("`df` must be a data frame (output of build_lsdf()).")
  }
  if (!is.character(file) || nchar(file) == 0) {
    stop("`file` must be a non-empty file path.")
  }
  if (!dir.exists(dirname(normalizePath(file, mustWork = FALSE)))) {
    stop("Directory does not exist: ", dirname(file))
  }

  # Rename type.scale → type/scale for TSV header
  names(df)[names(df) == "type.scale"] <- "type/scale"

  # Double internal quotes in text column
  df$text <- gsub('"', '""', df$text)

  # Wrap fields in quotes when they contain whitespace, @ or embedded quotes
  needs_quoting <- function(x) grepl('[ \t\n@"]', x, perl = TRUE) & nchar(x) > 0
  df$text <- ifelse(needs_quoting(df$text), paste0('"', df$text, '"'), df$text)
  df$name <- ifelse(needs_quoting(df$name), paste0('"', df$name, '"'), df$name)

  # Quote non-trivial relevance expressions
  df$relevance <- ifelse(
    df$relevance %in% c("1", ""),
    df$relevance,
    paste0('"', df$relevance, '"')
  )

  df[is.na(df)] <- ""

  header <- paste(names(df), collapse = "\t")
  body <- apply(df, 1, paste, collapse = "\t")
  content <- enc2utf8(paste(c(header, body), collapse = "\n"))

  con <- file(file, open = "w", encoding = "native.enc")
  writeLines(content, con = con, useBytes = TRUE)
  close(con)

  message("Written: ", file, " (", nrow(df), " rows)")
  invisible(df)
}


# ══ Convenience wrapper ═══════════════════════════════════════════════════════

#' Export a LimeSeed survey definition to a LimeSurvey TSV import file
#'
#' Convenience wrapper that runs the full three-stage pipeline —
#' [load_seed()] → [validate_seed()] -> [build_lsdf()] → [write_lsdf()] — in a single call.
#' Use this with prepared seed file(s) or programmatic created or modified seed object.
#' Always stops on errors.
#'
#' @param seed Survey definition in any format accepted by [load_seed()]:
#'   a path to a single YAML file, a path to a folder of YAML files, a
#'   named list of file paths, a mixed named list of paths and R lists, or
#'   a pre-loaded seed list.  See [load_seed()] for full details.
#' @param file Path to the output `.tsv` file.  The parent directory must
#'   already exist; it will not be created automatically.
#' @param codebook Optional codebook output path. Use `NULL` or `FALSE` to skip
#'   codebook generation, `TRUE` to write a codebook next to `file`, or a
#'   character path such as `"codebook.html"`.
#' @param codebook_options Named list of additional options passed to
#'   [ls_codebook()] when `codebook` is enabled.
#' @param question_options Named list of question option overrides applied to
#'   all questions whose resolved type supports the option, before validation
#'   and building. Invalid or inapplicable values are skipped with warnings.
#' @param test Logical. When `TRUE`, export a test-friendly survey: backward
#'   navigation enabled, question codes and question-jump index shown, titles
#'   marked as draft, welcome text prepended with a draft notice when present,
#'   hidden questions shown with a red `HIDDEN` marker, and mandatory questions
#'   made optional with a red `MANDATORY` marker.
#'
#' @return The written data frame, returned invisibly.  The primary effect
#'   is the `.tsv` file created at `file`.
#'
#' @seealso
#' * [load_seed()] — stage 1: read and normalise various seed files into a seed.
#' * [validate_seed()] - stage 2: validate seed and report any issues.
#' * [build_lsdf()] — stage 3: compile a seed into a LimeSurvey data frame.
#' * [write_lsdf()] — stage 4: format and write the data frame to a TSV file.
#'
#' @examples
#' \dontrun{
#' # Any seed format works — load_seed() handles normalisation internally.
#'
#' # Single combined YAML file
#' seed_to_tsv("path/to/survey.yaml", "output/survey.tsv")
#'
#' # Folder with separate settings.yml / structure.yml / quota.yml
#' seed_to_tsv("path/to/survey_dir/", "output/survey.tsv")
#'
#' # Named list of file paths
#' seed_to_tsv(
#'   seed = list(
#'     settings  = "path/to/settings.yaml",
#'     structure = "path/to/structure.yaml"
#'   ),
#'   file = "output/survey.tsv"
#' )
#'
#' # When seed manipulation is required, load with [load_seed()], manipulate and
#' # feed back into seed_to_tsv() pipeline
#' seed <- load_seed("path/to/survey.yaml")
#' seed$settings$mandatory        <- "N"
#' seed$structure$G1$Q2$relevance <- "Q1 == 'yes'"
#' seed_to_tsv(seed, "output/survey.tsv")   # seed is a valid seed (case 5)
#' seed_to_tsv(seed, "output/survey.tsv", codebook = "output/codebook.html")
#' seed_to_tsv(
#'   seed,
#'   "output/survey.tsv",
#'   codebook = "output/codebook.html",
#'   codebook_options = list(fields = "hidden")
#' )
#' seed_to_tsv(
#'   seed,
#'   "output/survey.tsv",
#'   question_options = list(hide_tip = 1, input_size = 4)
#' )
#'
#' # Export a test version before fielding.
#' seed_to_tsv(seed, "output/survey-test.tsv", test = TRUE)
#' }
#'
#' @export
seed_to_tsv <- function(
  seed,
  file,
  codebook = NULL,
  codebook_options = list(),
  question_options = list(),
  test = FALSE
) {
  seed <- load_seed(seed)
  seed <- set_question_options(seed, question_options)
  if (!is.logical(test) || length(test) != 1L || is.na(test)) {
    stop("`test` must be `TRUE` or `FALSE`.")
  }
  if (isTRUE(test)) {
    seed <- apply_test_mode(seed)
  }
  validate_seed(seed, file = NULL, stop_on_error = TRUE)
  df <- build_lsdf(seed)
  write_lsdf(df, file)
  if (!is.null(codebook) && !identical(codebook, FALSE)) {
    if (
      !is.list(codebook_options) ||
        (length(codebook_options) > 0L &&
          (is.null(names(codebook_options)) ||
            any(!nzchar(names(codebook_options)))))
    ) {
      stop("`codebook_options` must be a named list.")
    }
    codebook_file <- if (isTRUE(codebook)) {
      paste0(tools::file_path_sans_ext(file), "_codebook.html")
    } else {
      codebook
    }
    do.call(
      ls_codebook,
      c(list(df = df, output_file = codebook_file), codebook_options)
    )
  }
  invisible(df)
}


# ══ Internal helper ═══════════════════════════════════════════════════════════

# Apply test-mode survey changes before validation/building.
apply_test_mode <- function(seed) {
  seed$settings$allowprev <- "Y"
  seed$settings$showqnumcode <- "C"
  seed$settings$questionindex <- 2L
  seed$settings$titles <- .test_mode_prefix_values(
    seed$settings$titles,
    "DRAFT: "
  )

  if (.test_mode_has_text(seed$settings$welcomeTexts)) {
    seed$settings$welcomeTexts <- .test_mode_prefix_values(
      seed$settings$welcomeTexts,
      "DRAFT: This is a test version of the survey.<br /><br />"
    )
  }

  for (grp_code in names(seed$structure)) {
    qst_codes <- setdiff(names(seed$structure[[grp_code]]), "groupOptions")
    for (qst_code in qst_codes) {
      qst <- seed$structure[[grp_code]][[qst_code]]
      markers <- character(0)

      if (.test_mode_truthy(qst$hidden)) {
        qst$hidden <- 0
        markers <- c(markers, "HIDDEN")
      }
      if (.test_mode_mandatory(qst$mandatory)) {
        qst$mandatory <- "N"
        markers <- c(markers, "MANDATORY")
      }
      if (length(markers) > 0L) {
        qst$questionTexts <- .test_mode_prefix_text(qst$questionTexts, markers)
      }

      seed$structure[[grp_code]][[qst_code]] <- qst
    }
  }

  seed
}

.test_mode_truthy <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    return(FALSE)
  }
  tolower(as.character(x)) %in% c("1", "true", "t", "yes", "y")
}

.test_mode_mandatory <- function(x) {
  if (is.null(x) || length(x) != 1L || is.na(x)) {
    return(FALSE)
  }
  toupper(as.character(x)) %in% c("Y", "S")
}

.test_mode_has_text <- function(x) {
  if (is.null(x)) {
    return(FALSE)
  }
  any(nchar(trimws(as.character(unlist(x)))) > 0L)
}

.test_mode_prefix_text <- function(question_texts, markers) {
  prefix <- paste0(
    sprintf(
      '<span style="color:red;font-weight:bold;">%s</span><br />',
      markers
    ),
    collapse = ""
  )

  if (is.null(question_texts)) {
    return(prefix)
  }

  .test_mode_prefix_values(question_texts, prefix)
}

.test_mode_prefix_values <- function(texts, prefix) {
  if (is.null(texts)) {
    return(prefix)
  }
  if (is.null(names(texts))) {
    text <- paste(as.character(texts), collapse = "")
    if (startsWith(text, prefix)) {
      return(text)
    }
    return(paste0(prefix, text))
  }

  lapply(texts, function(txt) {
    txt <- as.character(txt)
    if (startsWith(txt, prefix)) {
      txt
    } else {
      paste0(prefix, txt)
    }
  })
}

#' Combine row-lists into the LimeSurvey data frame
#' @keywords internal
rows_to_df <- function(rows) {
  n <- length(rows)
  df <- as.data.frame(
    matrix("", nrow = n, ncol = length(LS_COLUMNS)),
    stringsAsFactors = FALSE
  )
  names(df) <- LS_COLUMNS

  for (i in seq_len(n)) {
    cols <- intersect(names(rows[[i]]), LS_COLUMNS)
    df[i, cols] <- unlist(rows[[i]][cols])
  }

  df
}
