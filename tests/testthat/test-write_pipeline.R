build_df <- function(seed) {
  suppressMessages(build_lsdf(seed))
}

test_that("write_lsdf writes a TSV with the LimeSurvey header and preserves row count", {
  file <- tempfile(fileext = ".tsv")
  on.exit(unlink(file), add = TRUE)

  df <- build_df(radio_seed())
  out <- suppressMessages(write_lsdf(df, file))

  expect_true(file.exists(file))
  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(df))
  expect_true("type/scale" %in% names(out))
  expect_false("type.scale" %in% names(out))

  header <- strsplit(
    readLines(file, n = 1L, warn = FALSE),
    "\t",
    fixed = TRUE
  )[[1]]
  expect_true("type/scale" %in% header)
  expect_false("type.scale" %in% header)

  roundtrip <- utils::read.delim(
    file,
    sep = "\t",
    header = TRUE,
    stringsAsFactors = FALSE,
    quote = ""
  )
  expect_equal(nrow(roundtrip), nrow(df))
  expect_true(all(c("S", "G", "Q", "A") %in% roundtrip$class))
})

test_that("write_lsdf applies LimeSurvey quoting rules to text, name, and relevance", {
  seed <- radio_seed()
  seed$structure$grp1$q1$questionTexts$de <- "He said \"Hi\" @ home"
  seed$structure$grp1$q1$relevance <- "Q0 == 'yes'"
  df <- build_df(seed)

  file <- tempfile(fileext = ".tsv")
  on.exit(unlink(file), add = TRUE)
  suppressMessages(write_lsdf(df, file))

  content <- paste(readLines(file, warn = FALSE), collapse = "\n")

  expect_match(content, "\"He said \"\"Hi\"\" @ home\"")
  expect_match(content, "\"Q0 == 'yes'\"")
})

test_that("write_lsdf rejects invalid inputs and missing output directories", {
  file <- tempfile(fileext = ".tsv")
  on.exit(unlink(file), add = TRUE)

  expect_error(write_lsdf(list(class = "Q"), file), "data frame")
  expect_error(write_lsdf(build_df(minimal_seed()), ""), "file path")
  expect_error(
    write_lsdf(
      build_df(minimal_seed()),
      file.path(tempdir(), "missing-dir", "survey.tsv")
    ),
    "Directory does not exist"
  )
})

test_that("seed_to_tsv runs the full pipeline for pre-loaded seeds and folder inputs", {
  out_file <- tempfile(fileext = ".tsv")
  on.exit(unlink(out_file), add = TRUE)

  seed <- quota_seed()
  df <- suppressMessages(seed_to_tsv(seed, out_file))

  expect_true(file.exists(out_file))
  expect_s3_class(df, "data.frame")
  expect_true(all(c("QTA", "QTALS", "QTAM") %in% df$class))

  dir <- tempfile("seed-folder-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  write_seed_files(seed, dir)

  folder_file <- tempfile(fileext = ".tsv")
  on.exit(unlink(folder_file), add = TRUE)

  folder_df <- suppressMessages(seed_to_tsv(dir, folder_file))
  expect_equal(nrow(folder_df), nrow(df))
})

test_that("seed_to_tsv test mode makes surveys easier to inspect", {
  out_file <- tempfile(fileext = ".tsv")
  on.exit(unlink(out_file), add = TRUE)

  seed <- radio_seed()
  seed$settings$format <- "G"
  seed$settings$allowprev <- "N"
  seed$settings$showqnumcode <- "N"
  seed$settings$questionindex <- 0L
  seed$settings$welcomeTexts <- list(de = "Welcome")
  seed$structure$grp1$q1$hidden <- 1
  seed$structure$grp1$q1$mandatory <- "Y"

  df <- suppressMessages(seed_to_tsv(seed, out_file, test = TRUE))
  settings <- df[df$class == "S", c("name", "text")]
  lang_settings <- df[df$class == "SL", c("name", "text")]
  q_row <- df[df$class == "Q" & df$name == "q1", ]

  expect_equal(settings$text[settings$name == "format"], "G")
  expect_equal(settings$text[settings$name == "allowprev"], "Y")
  expect_equal(settings$text[settings$name == "showqnumcode"], "C")
  expect_equal(settings$text[settings$name == "questionindex"], "2")
  expect_equal(
    lang_settings$text[lang_settings$name == "surveyls_title"],
    "DRAFT: Radio survey"
  )
  expect_match(
    lang_settings$text[lang_settings$name == "surveyls_welcometext"],
    "^DRAFT: This is a test version of the survey.<br /><br />Welcome"
  )
  expect_equal(q_row$hidden, "0")
  expect_equal(q_row$mandatory, "N")
  expect_match(q_row$text, "HIDDEN")
  expect_match(q_row$text, "MANDATORY")
  expect_match(q_row$text, "Choose one")
  expect_match(q_row$text, "color:red", fixed = TRUE)
})

test_that("seed_to_tsv stops on validation errors before writing output", {
  file <- tempfile(fileext = ".tsv")
  on.exit(unlink(file), add = TRUE)

  bad_seed <- radio_seed()
  bad_seed$structure$grp1$q1$answerOptions <- NULL

  expect_error(seed_to_tsv(bad_seed, file), "validation error")
  expect_false(file.exists(file))
})
