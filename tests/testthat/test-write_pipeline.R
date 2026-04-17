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

  header <- strsplit(readLines(file, n = 1L, warn = FALSE), "\t", fixed = TRUE)[[1]]
  expect_true("type/scale" %in% header)
  expect_false("type.scale" %in% header)

  roundtrip <- utils::read.delim(file, sep = "\t", header = TRUE, stringsAsFactors = FALSE, quote = "")
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
    write_lsdf(build_df(minimal_seed()), file.path(tempdir(), "missing-dir", "survey.tsv")),
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

test_that("seed_to_tsv stops on validation errors before writing output", {
  file <- tempfile(fileext = ".tsv")
  on.exit(unlink(file), add = TRUE)

  bad_seed <- radio_seed()
  bad_seed$structure$grp1$q1$answerOptions <- NULL

  expect_error(seed_to_tsv(bad_seed, file), "validation error")
  expect_false(file.exists(file))
})
