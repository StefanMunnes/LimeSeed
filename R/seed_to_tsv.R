# seed_to_tsv.R — Two-stage pipeline: sprout list → LimeSurvey data frame → TSV
# ─────────────────────────────────────────────────────────────────────────────

#' Load and normalise a LimeSeed survey definition, so called seed
#'
#' Accepts a survey definition in any of five formats and returns a
#' consistently structured named list (a *sprout*) ready for manipulation
#' or direct use by [build_lsdf()].
#'
#' @param seed Survey definition in one of the following formats:
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
#'   **5. Pre-loaded sprout list** (`list`) \cr
#'   A fully parsed named list that already contains `settings`,
#'   `structure`, and optionally `quota` as R lists.  Returned unchanged.
#'   Passing a sprout through `prime_seed()` is therefore idempotent and
#'   safe to use in pipelines where the upstream format is not known in
#'   advance.
#'
#' @return A named list — the *sprout* — with the following elements:
#'   \describe{
#'     \item{`settings`}{Named list of survey-level settings.}
#'     \item{`structure`}{Named list of groups, questions, subquestions,
#'       and answer options.}
#'     \item{`quota`}{Named list of quota definitions, or `NULL` when no
#'       quota is defined.}
#'   }
#'
#' @section Errors:
#' `prime_seed()` calls [stop()] in the following situations:
#' \itemize{
#'   \item The path in `seed` does not exist.
#'   \item A single-file path does not have a `.yml` or `.yaml` extension.
#'   \item A single YAML file is missing the `settings` or `structure`
#'     top-level keys.
#'   \item A folder contains fewer than two recognised YAML files
#'     (`settings`, `structure`).
#'   \item A list `seed` is unnamed or has any unnamed elements.
#'   \item A list `seed` is missing the `"settings"` or `"structure"` keys.
#'   \item In case 4, a list element is neither a valid file path nor an
#'     R list.
#' }
#'
#' @seealso [build_lsdf()] to convert the sprout into a LimeSurvey data
#'   frame; [seed_to_tsv()] for the all-in-one shortcut.
#'
#' @examples
#' \dontrun{
#' # Case 1 — single combined YAML file
#' sprout <- prime_seed("path/to/survey.yaml")
#'
#' # Case 2 — folder with settings.yml, structure.yml, quota.yml
#' sprout <- prime_seed("path/to/survey_dir/")
#'
#' # Case 3 — named list of file paths
#' sprout <- prime_seed(list(
#'   settings  = "path/to/settings.yaml",
#'   structure = "path/to/structure.yaml",
#'   quota     = "path/to/quota.yaml"
#' ))
#'
#' # Case 4 — mixed: one file path, one pre-built R list
#' my_structure <- list(G1 = list(Q1 = list(type = "S", questionTexts = "Name?")))
#' sprout <- prime_seed(list(
#'   settings  = "path/to/settings.yaml",
#'   structure = my_structure
#' ))
#'
#' # Case 5 — already a sprout; returned unchanged (idempotent)
#' sprout <- prime_seed(sprout)
#'
#' # Typical manipulation workflow
#' sprout <- prime_seed("path/to/survey.yaml")
#' sprout$settings$anonymized          <- "N"
#' sprout$structure$G1$Q2$relevance    <- "Q1 == 'yes'"
#' df <- build_lsdf(sprout)
#' }
#'
#' @importFrom yaml read_yaml
#' @importFrom tools file_path_sans_ext file_ext
#' @export
prime_seed <- function(seed) {
  # Internal templates
  sprout <- list(settings = NULL, structure = NULL, quota = NULL)

  # --- CASE 1 & 2: Input is a Character Path ---
  if (is.character(seed) && length(seed) == 1) {
    if (dir.exists(seed)) {
      # Case 2: Path to a folder (containing the separated YAML files)

      files <- list.files(
        seed,
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

      sprout <- lapply(files_valid, yaml::read_yaml)

      names(sprout) <- basename(files_valid) |> tools::file_path_sans_ext()
    } else if (file.exists(seed)) {
      # Case 1: Path to a single YAML file (containing everything)

      if (tools::file_ext(seed) %in% c("yml", "yaml")) {
        sprout <- yaml::read_yaml(seed)
      } else {
        stop("Input file path missing valid .yml or .yaml file extension.")
      }

      if (!all(c("settings", "structure") %in% names(sprout))) {
        stop("Input file does not contain 'settings' and 'structure' keys.")
      }
    } else {
      stop("Path specified in 'seed' does not exist.")
    }
  } else if (is.list(seed)) {
    # check if it's a valid named list
    if (is.null(names(seed))) {
      stop("If you provide a list as `seed`, it must be a named.")
    }
    if (!all(nzchar(names(seed)))) {
      stop("If you provide a list as `seed`, all elements must be named.")
    }
    if (!all(c("settings", "structure") %in% names(seed))) {
      stop("Input list does not contain named key 'settings' and 'structure'")
    }

    if (
      all(sapply(seed, class) == "character") &&
        all(lengths(seed) == 1)
    ) {
      # Case 3: It's a named list of paths (Case 3)
      sprout <- lapply(seed, yaml::read_yaml)
    } else if (
      any(sapply(seed, class) == "character") &&
        any(sapply(seed, class) == "list")
    ) {
      # Case 4: It's a named list of paths and R lists

      sprout <- lapply(seed, function(s) {
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
      sprout <- seed
    }
  }

  return(sprout)
}


# ══ Stage 1: build ════════════════════════════════════════════════════════════

#' Build the LimeSurvey data frame from a sprout list
#'
#' @param sprout Named list with `settings` and `structure` elements.
#' @return data.frame with columns matching [LS_COLUMNS] (all character).
#' @seealso [write_lsdf()], [seed_to_tsv()]
#' @export
build_lsdf <- function(sprout) {
  if (!is.list(sprout) || !all(c("settings", "structure") %in% names(sprout))) {
    stop(
      "`sprout` must be a named list with 'settings' and 'structure' elements."
    )
  }

  settings <- resolve_settings(sprout$settings)
  primary_lang <- settings$language
  langs <- all_languages(settings)

  validate_sprout(sprout, settings)

  message(
    "Building survey: '",
    get_text(settings$titles, primary_lang),
    "' [",
    paste(langs, collapse = " + "),
    "]"
  )

  # Pre-compute structural IDs
  id_map <- build_id_map(sprout$structure)

  # Pre-compute quota IDs: quota name → sequential integer (1-based)
  # These become the `id` of QTA rows and the `related_id` of QTAM/QTALS rows.
  quota_id_map <- if (!is.null(sprout$quota)) {
    setNames(seq_along(sprout$quota), names(sprout$quota))
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
        sprout$structure,
        lang,
        id_map,
        primary_lang,
        sprout$quota,
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
  if (!is.null(sprout$quota)) {
    rows <- c(
      rows,
      build_quota_rows(sprout$quota, langs, primary_lang, quota_id_map)
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
#' Has no knowledge of sprout structure.
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
#' [prime_seed()] → [build_lsdf()] → [write_lsdf()] — in a single call.
#' Use this with prepared seed file(s) or programmatic created or modified seed object.
#'
#' @param seed Survey definition in any format accepted by [prime_seed()]:
#'   a path to a single YAML file, a path to a folder of YAML files, a
#'   named list of file paths, a mixed named list of paths and R lists, or
#'   a pre-loaded seed list.  See [prime_seed()] for full details.
#' @param file Path to the output `.tsv` file.  The parent directory must
#'   already exist; it will not be created automatically.
#'
#' @return The written data frame, returned invisibly.  The primary effect
#'   is the `.tsv` file created at `file`.
#'
#' @seealso
#' * [prime_seed()] — stage 1: read and normalise a seed into a sprout.
#' * [build_lsdf()] — stage 2: compile a sprout into a LimeSurvey data frame.
#' * [write_lsdf()] — stage 3: format and write the data frame to a TSV file.
#'
#' @examples
#' \dontrun{
#' # Any seed format works — prime_seed() handles normalisation internally.
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
#' # When seed manipulation is required, load with [prime_seed()], manipulate and feed back into seed_to_tsv() pipeline
#' sprout <- prime_seed("path/to/survey.yaml")
#' sprout$settings$mandatory        <- "N"
#' sprout$structure$G1$Q2$relevance <- "Q1 == 'yes'"
#' seed_to_tsv(sprout, "output/survey.tsv")   # sprout is a valid seed (case 5)
#' }
#'
#' @export
seed_to_tsv <- function(seed, file) {
  sprout <- prime_seed(seed)
  df <- build_lsdf(sprout)
  write_lsdf(df, file)
  invisible(df)
}


# ══ Internal helper ═══════════════════════════════════════════════════════════

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
