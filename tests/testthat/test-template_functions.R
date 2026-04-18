test_that("lst_settings returns a usable multilingual settings template", {
  settings <- lst_settings(
    example = "simple",
    lang = c("en", "de")
  )

  expect_type(settings, "list")
  expect_s3_class(settings, "lst_template_result")
  expect_equal(settings$language, "en")
  expect_equal(settings$additional_languages, "de")
  expect_true(all(c("en", "de") %in% names(settings$titles)))
})

test_that("lst templates normalize whole-number numerics to integers", {
  settings <- lst_settings(
    example = "full",
    lang = "en"
  )
  quota <- lst_quota(lang = "en")

  expect_type(settings$questionindex, "integer")
  expect_type(settings$autonumber_start, "integer")
  expect_type(quota$quota_1$action, "integer")
  expect_type(quota$quota_1$active, "integer")
  expect_type(quota$quota_1$autoloadURL, "integer")
})

test_that("lst_questions builds question templates without groups", {
  questions <- lst_questions(
    types = c("radio", "array"),
    example = "minimal",
    lang = "en"
  )

  expect_type(questions, "list")
  expect_true(all(c("q1", "q2") %in% names(questions)))
  expect_true("answerOptions" %in% names(questions$q1))
  expect_true(all(c("answerOptions", "subquestions") %in% names(questions$q2)))
  expect_false("grp1" %in% names(questions))
})

test_that("lst_questions full examples keep answer options flat and subquestions use relevance", {
  questions <- lst_questions(
    types = "radio",
    example = "full",
    lang = "en"
  )

  expect_equal(questions$q1$answerOptions$A1, "Option 1")
  expect_equal(questions$q1$answerOptions$A2, "Option 2")
  expect_true("hide_tip" %in% names(questions$q1))
  expect_true("question_theme_name" %in% names(questions$q1))
  expect_true("other_replace_text" %in% names(questions$q1))
  expect_type(questions$q1$other_replace_text, "character")
})

test_that("lst_questions simple examples sit between minimal and full", {
  questions <- lst_questions(
    types = "array",
    example = "simple",
    lang = "en"
  )

  expect_true("helpTexts" %in% names(questions$q1))
  expect_true(is.list(questions$q1$subquestions$SQ1))
  expect_false("relevance" %in% names(questions$q1$subquestions$SQ2))
  expect_false("hide_tip" %in% names(questions$q1))
})

test_that("lst_seed wraps question templates into a valid grouped seed", {
  seed <- lst_seed(
    example = "full",
    question_types = c("radio", "array"),
    quota = TRUE,
    lang = c("en", "de"),
    path = NULL
  )

  expect_true(all(c("settings", "structure", "quota") %in% names(seed)))
  expect_true("grp1" %in% names(seed$structure))
  expect_true("q1" %in% names(seed$structure$grp1))

  issues <- collect_issues(seed, resolve_settings(seed$settings))
  expect_false(any(vapply(issues, `[[`, character(1), "severity") == "error"))
})

test_that("lst_seed writes split and combined YAML files", {
  dir_out <- tempfile("lst-survey-dir-")
  dir.create(dir_out)

  invisible(lst_seed(
    example = "minimal",
    question_types = "radio",
    quota = TRUE,
    lang = "en",
    path = dir_out
  ))

  expect_true(file.exists(file.path(dir_out, "settings.yml")))
  expect_true(file.exists(file.path(dir_out, "structure.yml")))
  expect_true(file.exists(file.path(dir_out, "quota.yml")))

  loaded_dir <- load_seed(dir_out)
  expect_true(all(c("settings", "structure", "quota") %in% names(loaded_dir)))

  file_out <- file.path(tempdir(), "lst-survey.yml")
  invisible(lst_seed(
    example = "minimal",
    question_types = "radio",
    quota = TRUE,
    lang = "en",
    path = file_out
  ))

  expect_true(file.exists(file_out))

  loaded_file <- load_seed(file_out)
  expect_true(all(c("settings", "structure", "quota") %in% names(loaded_file)))
})

test_that("add_valid annotates printed templates without changing returned objects", {
  settings <- lst_settings(example = "simple", lang = c("en", "de"), add_valid = TRUE)
  printed <- capture.output(print(settings))

  expect_s3_class(settings, "lst_template_result")
  expect_equal(settings$language, "en")
  expect_true(any(grepl("format: \"G\"  # valid: G \\| Q \\| A", printed)))
  expect_true(any(grepl("titles:  # multilingual", printed)))
  expect_true(any(grepl("admin: \"\"  # text", printed)))
})

test_that("add_valid annotates question and quota YAML files", {
  q_file <- file.path(tempdir(), "lst-questions-valid.yml")
  invisible(lst_questions(
    types = "radio",
    example = "full",
    add_valid = TRUE,
    path = q_file
  ))
  q_lines <- readLines(q_file, warn = FALSE, encoding = "UTF-8")

  expect_true(any(grepl("questionTexts: \"Example question.*# text \\(multilingual\\)", q_lines)))
  expect_true(any(grepl("mandatory: \"N\"  # valid: Y \\| N \\| S", q_lines)))
  expect_true(any(grepl("A1: \"Option 1\"  # text \\(multilingual\\)", q_lines)))

  quota_file <- file.path(tempdir(), "lst-quota-valid.yml")
  invisible(lst_quota(add_valid = TRUE, path = quota_file))
  quota_lines <- readLines(quota_file, warn = FALSE, encoding = "UTF-8")

  expect_true(any(grepl("limit: 100  # integer", quota_lines)))
  expect_true(any(grepl("messageTexts: \"Thank you.*# text \\(multilingual\\)", quota_lines)))
  expect_true(any(grepl("action: 1  # valid: 1 \\| 2", quota_lines)))
})
