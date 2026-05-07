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
  expect_true(any(body == "  **[de]** Frage  "))
  expect_true(any(body == "  **[en]** Question  "))
  expect_true(any(body_trim == "**Type:** radio"))
  expect_true(any(body_trim == "**Hidden:** 1"))
  expect_true(any(body_trim == "**Answer options:**"))
  expect_true(any(body == "  **1**  "))
  expect_true(any(body == "    **[de]** Ja  "))
  expect_true(any(body == "    **[en]** Yes  "))
  expect_false(any(body_trim == "**Filter:** 1"))
  expect_false(any(grepl("^- \\*\\*", body)))
  expect_false(any(grepl("<table", body, fixed = TRUE)))
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
  expect_true(any(body == "  **sq1:** Item 1  "))
})


test_that("fields appends requested non-empty fields to fixed defaults", {
  fields <- LimeSeed:::.codebook_fields(
    c("mandatory", "hidden", "")
  )

  expect_equal(
    unname(fields),
    c("type.scale", "relevance", "text", "help", "mandatory", "hidden")
  )
  expect_equal(
    names(fields),
    c("Type", "Filter", "Question text", "Help text", "Mandatory", "Hidden")
  )
})


test_that("mandatory is optional, not a fixed default field", {
  expect_equal(
    unname(LimeSeed:::.codebook_fields(NULL)),
    c("type.scale", "relevance", "text", "help")
  )
})


test_that("named fields control printed codebook labels", {
  df <- suppressMessages(build_lsdf(radio_seed("de")))
  df$relevance[df$class == "Q" & df$name == "q1"] <- "age > 18"

  body <- LimeSeed:::.generate_qmd_body(
    df,
    target_langs = "de",
    fields = LimeSeed:::.codebook_fields(c("Eligibility" = "relevance")),
    mode = "render"
  )

  expect_true(any(trimws(body) == "**Eligibility:** age > 18"))
  expect_false(any(trimws(body) == "**Filter:** age > 18"))
})


test_that("codebook filter removes hidden questions with descendants", {
  df <- suppressMessages(build_lsdf(array_seed("de")))
  df$hidden[df$class == "Q" & df$name == "q1"] <- "1"

  filtered <- LimeSeed:::.filter_codebook_df(df, rm_hidden = TRUE)

  expect_false(any(filtered$class %in% c("Q", "SQ", "A")))
  expect_true(any(filtered$class == "G"))
})


test_that("codebook filter removes requested variables with descendants", {
  df <- suppressMessages(build_lsdf(radio_seed("de")))

  filtered <- LimeSeed:::.filter_codebook_df(df, rm_hidden = FALSE, rm_vars = "q1")

  expect_false(any(filtered$class %in% c("Q", "A")))
  expect_true(any(filtered$class == "G"))
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


test_that("codebook preserves primary-language fallback text from build_lsdf", {
  df <- suppressMessages(build_lsdf(bilingual_seed()))

  body <- LimeSeed:::.generate_qmd_body(
    df,
    target_langs = "en",
    fields = LimeSeed:::.codebook_fields(NULL),
    mode = "render"
  )

  body_text <- paste(body, collapse = "\n")
  expect_match(body_text, "Question")
  expect_match(body_text, "Yes")
  expect_match(body_text, "Nein")
})


test_that("choice block includes relevance when present", {
  df <- suppressMessages(build_lsdf(array_seed("de")))
  df$relevance[df$class == "SQ" & df$name == "sq1"] <- "q0 == 1"

  body <- LimeSeed:::.generate_qmd_body(
    df,
    target_langs = "de",
    fields = LimeSeed:::.codebook_fields(NULL),
    mode = "render"
  )

  body_text <- paste(body, collapse = "\n")
  expect_match(body_text, "q0 == 1", fixed = TRUE)
})


test_that("markdown hard breaks are added to each physical line", {
  expect_equal(
    LimeSeed:::.markdown_hardbreak("a\nb"),
    "a  \nb  "
  )
})
