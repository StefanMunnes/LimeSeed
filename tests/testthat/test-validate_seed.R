valid_settings <- function(lang = "de") {
  resolve_settings(list(
    language = lang,
    titles = stats::setNames(list("Survey"), lang)
  ))
}

test_that("validate_settings enforces required fields and warns on invalid values", {
  issues <- validator_issues(validate_settings, list(
    language = "",
    allowprev = "maybe",
    tokenlength = "abc",
    format = "Z"
  ))

  expect_true(has_issue(issues, "error", "settings\\$language"))
  expect_true(has_issue(issues, "error", "settings\\$titles"))
  expect_true(has_issue(issues, "warning", "allowprev"))
  expect_true(has_issue(issues, "warning", "tokenlength"))
  expect_true(has_issue(issues, "warning", "format"))
})

test_that("validate_settings accepts known SL settings without unknown-setting warnings", {
  issues <- validator_issues(validate_settings, list(
    language = "en",
    titles = list(en = "Survey"),
    welcomeTexts = list(en = "Welcome"),
    endTexts = list(en = "Done"),
    dateformats = 6L,
    numberformats = 0L
  ))

  expect_false(any(grepl("Unknown survey setting", vapply(issues, `[[`, character(1), "message"))))
})

test_that("validate_structure flags unsupported types, missing required parts, and invalid values", {
  structure <- list(
    grp1 = list(
      `1bad` = list(type = "text display"),
      q1 = list(type = "radio", questionTexts = list(de = "Q")),
      q2 = list(
        type = "text display",
        questionTexts = list(de = "Q"),
        answerOptions = list("1" = list(de = "unused"))
      ),
      q3 = list(
        type = "numerical input",
        questionTexts = list(de = "Number"),
        min_num_value_n = 10,
        max_num_value_n = 1
      ),
      q4 = list(
        type = "radio",
        questionTexts = list(de = "Q"),
        answerOptions = list("1" = list(de = "A")),
        hidden = 5
      ),
      q5 = list(type = "not-a-real-type", questionTexts = list(de = "Q"))
    )
  )

  issues <- validator_issues(validate_structure, structure, valid_settings())

  expect_true(has_issue(issues, "warning", "1bad"))
  expect_true(has_issue(issues, "error", "requires 'answerOptions'"))
  expect_true(has_issue(issues, "warning", "does not use 'answerOptions'"))
  expect_true(has_issue(issues, "error", "min_num_value_n"))
  expect_true(has_issue(issues, "warning", "hidden"))
  expect_true(has_issue(issues, "error", "Could not resolve type"))
})

test_that("validate_structure warns on empty structures and missing primary-language text", {
  empty_issues <- validator_issues(validate_structure, list(), valid_settings())
  text_issues <- validator_issues(
    validate_structure,
    list(grp1 = list(q1 = list(type = "text display", questionTexts = list(en = "Only English")))),
    valid_settings("de")
  )

  expect_true(has_issue(empty_issues, "warning", "^structure$"))
  expect_true(has_issue(text_issues, "warning", "primary language"))
})

test_that("validate_quota checks required fields, members, and supported codes", {
  structure <- list(
    grp1 = list(
      q1 = list(
        lsType = "A",
        questionTexts = list(de = "Matrix"),
        subquestions = list(sq1 = list(de = "Row")),
        answerOptions = list("1" = list(de = "Yes"))
      ),
      q2 = radio_seed()$structure$grp1$q1
    )
  )

  issues <- validator_issues(validate_quota, list(
    missing_limit = list(members = list(q2 = "1")),
    bad_member = list(limit = 1L, members = list(q2 = "99")),
    bad_array = list(limit = 1L, members = list(q1 = "sq1")),
    missing_question = list(limit = 1L, members = list(unknown = "1")),
    unknown_field = list(limit = 1L, members = list(q2 = "1"), extra = TRUE)
  ), structure)

  expect_true(has_issue(issues, "error", "missing required field: 'limit'"))
  expect_true(has_issue(issues, "error", "Answer code '99'"))
  expect_true(has_issue(issues, "error", "must use 'SubQuestion-Answer' format"))
  expect_true(has_issue(issues, "warning", "does not exist in structure"))
  expect_true(has_issue(issues, "warning", "unknown field 'extra'"))
})

test_that("collect_issues and issues_to_df return normalized issue records", {
  seed <- list(
    settings = list(language = "de"),
    structure = list()
  )
  issues <- collect_issues(seed, resolve_settings(seed$settings))
  df <- issues_to_df(issues)

  expect_type(issues, "list")
  expect_s3_class(df, "data.frame")
  expect_equal(names(df), c("severity", "location", "message"))
  expect_gt(nrow(df), 0L)
})

test_that("issues_to_df returns a zero-row data frame for zero issues", {
  df <- issues_to_df(list())

  expect_equal(nrow(df), 0L)
  expect_equal(names(df), c("severity", "location", "message"))
})

test_that("report_issues writes files and signals warning-only vs error conditions", {
  report_file <- tempfile(fileext = ".txt")
  on.exit(unlink(report_file), add = TRUE)

  warnings_only <- list(list(
    severity = "warning",
    location = "settings$format",
    message = "Bad format"
  ))

  expect_warning(
    expect_true(report_issues(warnings_only, file = report_file)),
    "validation warning"
  )
  expect_true(file.exists(report_file))
  expect_match(paste(readLines(report_file, warn = FALSE), collapse = "\n"), "settings\\$format")

  errors_only <- list(list(
    severity = "error",
    location = "structure$grp1$q1",
    message = "Broken"
  ))

  expect_error(report_issues(errors_only), "validation error")
  expect_warning(report_issues(errors_only, stop_on_error = FALSE), "validation error")
})

test_that("validate_seed returns issues, writes reports, and respects stop_on_error", {
  warn_seed <- minimal_seed()
  warn_seed$settings$format <- "Z"

  report_file <- tempfile(fileext = ".txt")
  on.exit(unlink(report_file), add = TRUE)

  expect_warning(
    warn_df <- validate_seed(warn_seed, file = report_file),
    "validation warning"
  )
  expect_s3_class(warn_df, "data.frame")
  expect_equal(warn_df$severity, "warning")
  expect_true(file.exists(report_file))

  error_seed <- radio_seed()
  error_seed$structure$grp1$q1$answerOptions <- NULL

  expect_error(validate_seed(error_seed), "validation error")
  expect_warning(
    error_df <- validate_seed(error_seed, stop_on_error = FALSE),
    "validation error"
  )
  expect_true(any(error_df$severity == "error"))
})
