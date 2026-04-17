test_that("build_lsdf returns a character-only data frame using LS_COLUMNS", {
  df <- suppressMessages(build_lsdf(minimal_seed()))

  expect_s3_class(df, "data.frame")
  expect_equal(names(df), LS_COLUMNS)
  expect_true(all(vapply(df, is.character, logical(1))))
  expect_gt(nrow(df), 0L)
})

test_that("build_lsdf creates the expected core row classes and row content", {
  df <- suppressMessages(build_lsdf(radio_seed()))

  expect_equal(sum(df[["class"]] == "S"), length(LS_SETTINGS))
  expect_equal(sum(df[["class"]] == "SL"), 24L)
  expect_equal(sum(df[["class"]] == "G"), 1L)
  expect_equal(sum(df[["class"]] == "Q"), 1L)
  expect_equal(sum(df[["class"]] == "A"), 2L)

  q_row <- df[df[["class"]] == "Q", ]
  expect_equal(q_row$name, "q1")
  expect_equal(q_row$type.scale, "L")
  expect_equal(q_row$mandatory, "N")
  expect_equal(q_row$relevance, "1")
  expect_equal(q_row$text, "Choose one")

  a_rows <- df[df[["class"]] == "A", c("id", "name", "text")]
  expect_equal(a_rows$id, c("2", "2"))
  expect_equal(a_rows$name, c("1", "2"))
  expect_equal(a_rows$text, c("Yes", "No"))
})

test_that("build_lsdf expands multilingual text with fallback and language-aware options", {
  df <- suppressMessages(build_lsdf(bilingual_seed()))

  expect_equal(sort(unique(df[df[["class"]] == "Q", "language"])), c("de", "en"))
  expect_equal(sum(df[["class"]] == "G"), 2L)
  expect_equal(sum(df[["class"]] == "Q"), 2L)
  expect_equal(sum(df[["class"]] == "A"), 4L)

  q_rows <- df[df[["class"]] == "Q", c("language", "text", "help", "other", "other_replace_text")]
  expect_equal(q_rows$text[q_rows$language == "de"], "Frage")
  expect_equal(q_rows$text[q_rows$language == "en"], "Question")
  expect_equal(q_rows$other_replace_text[q_rows$language == "de"], "Sonst")
  expect_equal(q_rows$other_replace_text[q_rows$language == "en"], "Other")

  a_en <- df[df[["class"]] == "A" & df[["language"]] == "en", c("name", "text")]
  expect_equal(a_en$text[a_en$name == "1"], "Yes")
  expect_equal(a_en$text[a_en$name == "2"], "Nein")
})

test_that("build_lsdf creates SQ rows from array questions and no A rows for multiple choice", {
  array_df <- suppressMessages(build_lsdf(array_seed()))
  multi_df <- suppressMessages(build_lsdf(multiple_choice_seed()))

  expect_equal(sum(array_df[["class"]] == "SQ"), 2L)
  expect_equal(sum(array_df[["class"]] == "A"), 2L)

  sq_rows <- array_df[array_df[["class"]] == "SQ", c("name", "text", "help", "mandatory", "default")]
  expect_equal(sq_rows$text, c("Item 1", "Item 2"))
  expect_equal(sq_rows$help[sq_rows$name == "sq2"], "Explain item 2")
  expect_equal(sq_rows$mandatory[sq_rows$name == "sq2"], "Y")
  expect_equal(sq_rows$default[sq_rows$name == "sq2"], "1")

  expect_equal(sum(multi_df[["class"]] == "SQ"), 2L)
  expect_equal(sum(multi_df[["class"]] == "A"), 0L)
})

test_that("build_lsdf emits quota rows with sequential ids and mapped fields", {
  df <- suppressMessages(build_lsdf(quota_seed()))

  qtam <- df[df[["class"]] == "QTAM", c("id", "name", "related_id")]
  qta <- df[df[["class"]] == "QTA", c("id", "name", "mandatory", "other", "default", "same_default")]
  qtals <- df[df[["class"]] == "QTALS", c("id", "related_id", "language", "relevance", "text", "help")]

  expect_equal(nrow(qtam), 1L)
  expect_equal(nrow(qta), 1L)
  expect_equal(nrow(qtals), 1L)

  expect_equal(qtam$id, "1")
  expect_equal(qtam$name, "1")
  expect_equal(qtam$related_id, qta$id)

  expect_equal(qta$name, "quota_a")
  expect_equal(qta$mandatory, "100")
  expect_equal(qta$other, as.character(LS_QUOTA_OPTIONS$action$default))
  expect_equal(qta$default, as.character(LS_QUOTA_OPTIONS$active$default))
  expect_equal(qta$same_default, as.character(LS_QUOTA_OPTIONS$autoloadURL$default))

  expect_equal(qtals$related_id, qta$id)
  expect_equal(qtals$relevance, "Quota reached")
  expect_equal(qtals$text, "https://example.invalid/quota")
  expect_equal(qtals$help, "More info")
})

test_that("build_lsdf errors when required top-level seed components are missing", {
  expect_error(build_lsdf(list(structure = list())), "settings")
  expect_error(build_lsdf(list(settings = list(language = "de"))), "structure")
})
