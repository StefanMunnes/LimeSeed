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
  expect_equal(changed$structure$grp1$q3$hide_tip, 1)
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

test_that("lsh_options wraps values within console-width columns", {
  old_width <- getOption("width")
  on.exit(options(width = old_width), add = TRUE)
  options(width = 80L)

  output <- capture.output(lsh_options(search = "order"))
  header <- output[grepl("^Option", output)]
  valid_start <- regexpr("Valid", header)
  description_start <- regexpr("Description", header)
  types_start <- regexpr("Types", header)

  table_lines <- output[seq.int(
    match(header, output),
    match("", output[match(header, output):length(output)]) + match(header, output) - 2L
  )]
  types_line <- output[grepl("U X Y ! : ; |", output, fixed = TRUE)]
  valid_text <- paste0(
    trimws(substr(table_lines, valid_start, description_start - 2L)),
    collapse = ""
  )

  expect_true(all(nchar(table_lines) <= 80L))
  expect_match(valid_text, "random_alphabetical", fixed = TRUE)
  expect_false(any(grepl("…", table_lines, fixed = TRUE)))
  expect_true(all(substr(types_line, types_start, types_start + 12L) ==
    "U X Y ! : ; |"
  ))
})

test_that("lsh_options formats consecutive numeric validation values as ranges", {
  expect_equal(.lsh_fmt_valid(1:100), "1:100")
  expect_equal(.lsh_fmt_valid(0:5), "0:5")
  expect_equal(.lsh_fmt_valid(c(0, 1, 3)), "0 | 1 | 3")
})
