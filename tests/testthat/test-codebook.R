test_that("codebook language discovery splits additional languages", {
  seed <- bilingual_seed()
  seed$settings$additional_languages <- "en fr"
  seed$settings$titles$fr <- "Enquete francaise"

  df <- suppressMessages(build_lsdf(seed))

  expect_equal(
    LimeSeed:::.available_codebook_languages(df),
    c("de", "en", "fr")
  )
})


test_that("codebook body groups multilingual structural rows together", {
  df <- suppressMessages(build_lsdf(bilingual_seed()))
  fields <- LimeSeed:::.codebook_fields(c("mandatory", "hidden"))
  df$hidden[df$class == "Q" & df$name == "q1"] <- "1"

  body <- LimeSeed:::.generate_qmd_body(
    df,
    target_langs = c("de", "en"),
    fields = fields,
    mode = "render"
  )

  body_trim <- trimws(body)

  expect_equal(sum(grepl("^## Group: Gruppe$", body)), 1L)
  expect_equal(sum(grepl("^### q1$", body)), 1L)
  expect_true(any(grepl("\\*\\*\\[en\\]\\*\\* Group", body)))
  expect_true(any(body_trim == "**Question text:**"))
  expect_true(any(body_trim == "[de] Frage"))
  expect_true(any(body_trim == "[en] Question"))
  expect_true(any(body_trim == "**Type:** radio"))
  expect_true(any(body_trim == "**Hidden:** 1"))
  expect_true(any(body_trim == "**Answer options:**"))
  expect_false(any(body_trim == "**Filter:** 1"))
  expect_false(any(grepl("^- \\*\\*", body)))
})


test_that("codebook labels non-default relevance as Filter", {
  df <- suppressMessages(build_lsdf(radio_seed("de")))
  df$relevance[df$class == "Q" & df$name == "q1"] <- "age > 18"

  body <- LimeSeed:::.generate_qmd_body(
    df,
    target_langs = "de",
    fields = LimeSeed:::.codebook_fields(NULL),
    mode = "render"
  )

  expect_true(any(trimws(body) == "**Filter:** age > 18"))
})


test_that("codebook body formats subquestions as indented content blocks", {
  df <- suppressMessages(build_lsdf(array_seed("de")))
  body <- LimeSeed:::.generate_qmd_body(
    df,
    target_langs = "de",
    fields = LimeSeed:::.codebook_fields(NULL),
    mode = "render"
  )

  body_trim <- trimws(body)

  expect_equal(sum(grepl("^### q1$", body)), 1L)
  expect_true(any(body_trim == "**Type:** array"))
  expect_true(any(body_trim == "**Subquestions:**"))
  expect_true(any(body_trim == "sq1"))
  expect_true(any(body_trim == "[de] Item 1"))
})


test_that("fields appends requested non-empty fields to fixed defaults", {
  fields <- LimeSeed:::.codebook_fields(
    c("mandatory", "hidden", "")
  )

  expect_equal(
    fields,
    c("type.scale", "relevance", "text", "help", "mandatory", "hidden")
  )
})


test_that("mandatory is optional, not a fixed default field", {
  expect_equal(
    LimeSeed:::.codebook_fields(NULL),
    c("type.scale", "relevance", "text", "help")
  )
})


test_that("codebook text processing preserves HTML but removes scripts", {
  html <- "<strong>Hello</strong><script>alert('x')</script><em>World</em>"

  expect_equal(
    LimeSeed:::.process_text(html, "render"),
    "<strong>Hello</strong><em>World</em>"
  )
  expect_equal(
    LimeSeed:::.process_text(html, "remove"),
    "HelloWorld"
  )
})


test_that("codebook rejects output files without extension", {
  df <- suppressMessages(build_lsdf(minimal_seed()))

  expect_error(
    LimeSeed::ls_codebook(df, output_file = "survey"),
    "must include a file extension"
  )
})
