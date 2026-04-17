test_that("lsh_types resolves codes and returns an lsh_types_result object", {
  output <- capture.output(result <- lsh_types(c("radio", "F"), details = FALSE, lang = "en"))

  expect_s3_class(result, "lsh_types_result")
  expect_equal(unclass(result), c("L", "F"))
  expect_gt(length(output), 0L)
})

test_that("print.lsh_types_result emits a compact summary", {
  result <- structure(c("L", "M"), class = c("lsh_types_result", "character"))
  output <- capture.output(print(result))

  expect_true(any(grepl("\\[L\\] radio", output)))
  expect_true(any(grepl("\\[M\\] multiple choice", output)))
})

test_that("lsh_options filters option metadata and accepts lsh_types output", {
  type_result <- suppressMessages(lsh_types("radio", details = FALSE))
  output <- capture.output(options_df <- lsh_options(type_result, search = "other", lang = "en"))

  expect_s3_class(options_df, "data.frame")
  expect_true(all(c("option", "default", "valid", "description", "applies_to", "multilingual") %in% names(options_df)))
  expect_true(all(grepl("other", options_df$option, ignore.case = TRUE)))
  expect_gt(length(output), 0L)
})

test_that("lsh_settings returns matching setting metadata in the requested language", {
  output <- capture.output(settings_df <- lsh_settings(c("format", "email"), lang = "de"))

  expect_s3_class(settings_df, "data.frame")
  expect_true(all(c("setting", "default", "valid", "description") %in% names(settings_df)))
  expect_true("format" %in% settings_df$setting)
  expect_true(any(grepl("Umfrageformat", settings_df$description, fixed = TRUE)))
  expect_gt(length(output), 0L)
})
