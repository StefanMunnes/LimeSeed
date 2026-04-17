test_that("resolve_settings merges defaults and derives additional languages from titles", {
  settings <- resolve_settings(list(
    language = "de",
    titles = list(de = "Deutsch", en = "English", fr = "Francais"),
    format = "Q"
  ))

  expect_equal(settings$format, "Q")
  expect_equal(settings$anonymized, LS_SETTINGS$anonymized$default)
  expect_setequal(all_languages(settings), c("de", "en", "fr"))
})

test_that("resolve_settings respects explicit additional_languages", {
  settings <- resolve_settings(list(
    language = "de",
    titles = list(de = "Deutsch"),
    additional_languages = "en fr"
  ))

  expect_equal(all_languages(settings), c("de", "en", "fr"))
})

test_that("build_settings_rows and build_lang_rows produce the expected named rows", {
  settings <- resolve_settings(bilingual_seed()$settings)

  s_rows <- build_settings_rows(settings)
  sl_rows <- build_lang_rows(settings, "en")

  expect_length(s_rows, length(LS_SETTINGS))
  expect_true(all(vapply(s_rows, `[[`, character(1), "class") == "S"))
  expect_true("format" %in% vapply(s_rows, `[[`, character(1), "name"))

  expect_true(all(vapply(sl_rows, `[[`, character(1), "class") == "SL"))
  expect_true(all(vapply(sl_rows, `[[`, character(1), "language") == "en"))
  expect_true("surveyls_title" %in% vapply(sl_rows, `[[`, character(1), "name"))
})

test_that("build_id_map assigns depth-first sequential ids and skips groupOptions", {
  ids <- build_id_map(array_seed()$structure)

  expect_equal(ids[["G:grp1"]], 1L)
  expect_equal(ids[["Q:q1"]], 2L)
  expect_equal(ids[["SQ:q1:sq1"]], 3L)
  expect_equal(ids[["SQ:q1:sq2"]], 4L)
  expect_false("Q:groupOptions" %in% names(ids))
  expect_equal(length(ids), length(unique(ids)))
})

test_that("text helpers provide strict and fallback language resolution", {
  input <- list(de = "Deutsch", en = "", fr = "Francais")

  expect_equal(get_text_fb(NULL, "de", "de"), "")
  expect_equal(get_text_fb("plain", "en", "de"), "plain")
  expect_equal(get_text_fb(input, "en", "de"), "Deutsch")
  expect_equal(get_text_fb(list(de = "", en = "", fr = "Francais"), "en", "de"), "Francais")

  expect_equal(get_text(NULL, "de"), "")
  expect_equal(get_text("plain", "en"), "plain")
  expect_equal(get_text(input, "de"), "Deutsch")
  expect_equal(get_text(input, "en"), "")
})

test_that("row helpers coerce values and preserve blanks for missing columns", {
  row <- ls_row(id = 7L, class = "Q", name = "q1", mandatory = NULL)
  df <- rows_to_df(list(row))

  expect_identical(row$id, "7")
  expect_identical(row$mandatory, "")
  expect_equal(names(df), LS_COLUMNS)
  expect_equal(df$id, "7")
  expect_equal(df$class, "Q")
  expect_equal(df$mandatory, "")
  expect_equal(df$text, "")
})

test_that("subquestion and answer helpers support compact and explicit forms", {
  sq <- list(
    subquestionTexts = list(de = "Row"),
    helpTexts = list(de = "Help"),
    default = list(de = "1"),
    mandatory = "Y"
  )
  ans <- list(
    optionTexts = list(de = "Answer"),
    type.scale = 2
  )

  expect_equal(sq_text_fb(sq, "de", "de"), "Row")
  expect_equal(sq_text_fb(list(de = "Row"), "de", "de"), "Row")
  expect_equal(sq_field_fb(sq, "helpTexts", "de", "de"), "Help")
  expect_equal(sq_attr(sq, "mandatory", ""), "Y")
  expect_equal(sq_attr(list(), "mandatory", "N"), "N")

  expect_equal(ans_text_fb(ans, "de", "de"), "Answer")
  expect_equal(ans_text_fb(list(de = "Answer"), "de", "de"), "Answer")
  expect_equal(ans_attr(ans, "type.scale", 0), 2)
  expect_equal(ans_attr(list(), "type.scale", 0), 0)
})

test_that("find_group_of_qst locates a question code or returns NULL", {
  structure <- quota_seed()$structure

  expect_equal(find_group_of_qst("q1", structure), "grp1")
  expect_null(find_group_of_qst("missing", structure))
})
