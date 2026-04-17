test_that("load_seed returns a pre-loaded seed unchanged", {
  seed <- quota_seed()

  expect_identical(load_seed(seed), seed)
})

test_that("load_seed reads a combined YAML file", {
  file <- tempfile(fileext = ".yaml")
  on.exit(unlink(file), add = TRUE)
  yaml::write_yaml(quota_seed(), file)

  seed <- load_seed(file)

  expect_equal(seed$settings$language, "de")
  expect_equal(seed$structure$grp1$q1$type, "radio")
  expect_equal(seed$quota$quota_a$limit, 100L)
})

test_that("load_seed reads a folder of YAML files and ignores unrelated files", {
  dir <- tempfile("seed-dir-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  write_seed_files(quota_seed(), dir)
  yaml::write_yaml(list(ignore = TRUE), file.path(dir, "other.yml"))

  seed <- load_seed(dir)

  expect_setequal(names(seed), c("settings", "structure", "quota"))
  expect_equal(seed$quota$quota_a$members$q1, "1")
  expect_false("other" %in% names(seed))
})

test_that("load_seed reads a named list of file paths", {
  dir <- tempfile("seed-paths-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  seed <- quota_seed()
  write_seed_files(seed, dir)

  loaded <- load_seed(list(
    settings = file.path(dir, "settings.yml"),
    structure = file.path(dir, "structure.yaml"),
    quota = file.path(dir, "quota.YML")
  ))

  expect_equal(loaded$settings$titles$de, seed$settings$titles$de)
  expect_equal(names(loaded$structure$grp1$q1$answerOptions), c("1", "2"))
  expect_equal(loaded$quota$quota_a$messageTexts$de, "Quota reached")
})

test_that("load_seed reads a mixed named list of paths and parsed lists", {
  dir <- tempfile("seed-mixed-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)

  seed <- radio_seed()
  yaml::write_yaml(seed$settings, file.path(dir, "settings.yml"))

  loaded <- load_seed(list(
    settings = file.path(dir, "settings.yml"),
    structure = seed$structure,
    quota = list(custom = list(limit = 5L, members = list(q1 = "1")))
  ))

  expect_equal(loaded$settings$language, "de")
  expect_identical(loaded$structure, seed$structure)
  expect_equal(loaded$quota$custom$limit, 5L)
})

test_that("load_seed errors on missing paths, unsupported extensions, and malformed lists", {
  expect_error(load_seed("missing-file.yml"), "does not exist")

  file <- tempfile(fileext = ".txt")
  on.exit(unlink(file), add = TRUE)
  writeLines("not yaml", file)
  expect_error(load_seed(file), "valid .yml or .yaml")

  expect_error(load_seed(list(1, 2)), "must be a named")
  expect_error(
    load_seed(list(settings = list(language = "de"))),
    "'settings' and 'structure'"
  )

  yml <- tempfile(fileext = ".yml")
  on.exit(unlink(yml), add = TRUE)
  yaml::write_yaml(list(language = "de", titles = list(de = "T")), yml)
  expect_error(
    load_seed(list(settings = yml, structure = list(), quota = 1)),
    "either a YAML file path or a list"
  )
})

test_that("load_seed errors when required YAML keys or files are missing", {
  file <- tempfile(fileext = ".yml")
  on.exit(unlink(file), add = TRUE)
  yaml::write_yaml(list(settings = list(language = "de")), file)

  expect_error(load_seed(file), "'settings' and 'structure'")

  dir <- tempfile("seed-missing-")
  dir.create(dir)
  on.exit(unlink(dir, recursive = TRUE, force = TRUE), add = TRUE)
  yaml::write_yaml(list(language = "de"), file.path(dir, "settings.yml"))

  expect_error(load_seed(dir), "'settings' and 'structure'")
})
