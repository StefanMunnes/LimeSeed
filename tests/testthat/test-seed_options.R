test_that("set_question_options applies valid options to supporting questions", {
  seed <- radio_seed()
  seed$structure$grp1$q2 <- list(
    type = "short text",
    questionTexts = list(de = "Text")
  )
  seed$structure$grp1$q3 <- list(
    type = "text display",
    questionTexts = list(de = "Info")
  )

  changed <- set_question_options(seed, list(hide_tip = 1, input_size = 4))

  expect_equal(changed$structure$grp1$q1$hide_tip, 1)
  expect_false("input_size" %in% names(changed$structure$grp1$q1))
  expect_equal(changed$structure$grp1$q2$hide_tip, 1)
  expect_equal(changed$structure$grp1$q2$input_size, 4)
  expect_false("hide_tip" %in% names(changed$structure$grp1$q3))
  expect_false("input_size" %in% names(changed$structure$grp1$q3))
})

test_that("set_question_options warns and skips unknown or invalid options", {
  seed <- radio_seed()

  changed <- expect_warning(
    expect_warning(
      set_question_options(
        seed,
        list(hide_tip = 99, unknown_option = 1)
      ),
      "not a known"
    ),
    "not valid"
  )

  expect_false("hide_tip" %in% names(changed$structure$grp1$q1))
  expect_false("unknown_option" %in% names(changed$structure$grp1$q1))
})

test_that("seed_to_tsv applies question_options before validation and build", {
  out_file <- tempfile(fileext = ".tsv")
  on.exit(unlink(out_file), add = TRUE)

  df <- suppressMessages(seed_to_tsv(
    radio_seed(),
    out_file,
    question_options = list(hide_tip = 1)
  ))
  q_row <- df[df$class == "Q" & df$name == "q1", ]

  expect_true(file.exists(out_file))
  expect_equal(q_row$hide_tip, "1")
})
