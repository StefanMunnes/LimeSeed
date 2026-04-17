# Shared fixtures and test helpers for LimeSeed.

validator_issues <- function(fn, ...) {
  issues <- list()
  add <- function(severity, location, message) {
    issues[[length(issues) + 1L]] <<- list(
      severity = severity,
      location = location,
      message = message
    )
  }
  do.call(fn, c(list(...), list(add)))
  issues
}

has_issue <- function(issues, severity, pattern) {
  any(vapply(issues, function(issue) {
    identical(issue$severity, severity) &&
      (
        grepl(pattern, issue$location, fixed = FALSE) ||
          grepl(pattern, issue$message, fixed = FALSE)
      )
  }, logical(1)))
}

minimal_seed <- function(lang = "de") {
  list(
    settings = list(
      language = lang,
      titles = stats::setNames(list("Test survey"), lang)
    ),
    structure = list(
      grp1 = list(
        intro = list(
          type = "text display",
          questionTexts = stats::setNames(list("Read this first."), lang)
        )
      )
    )
  )
}

radio_seed <- function(lang = "de") {
  list(
    settings = list(
      language = lang,
      titles = stats::setNames(list("Radio survey"), lang)
    ),
    structure = list(
      grp1 = list(
        q1 = list(
          type = "radio",
          questionTexts = stats::setNames(list("Choose one"), lang),
          answerOptions = list(
            "1" = stats::setNames(list("Yes"), lang),
            "2" = stats::setNames(list("No"), lang)
          )
        )
      )
    )
  )
}

array_seed <- function(lang = "de") {
  list(
    settings = list(
      language = lang,
      titles = stats::setNames(list("Array survey"), lang)
    ),
    structure = list(
      grp1 = list(
        q1 = list(
          type = "array",
          questionTexts = stats::setNames(list("Rate each item"), lang),
          subquestions = list(
            sq1 = stats::setNames(list("Item 1"), lang),
            sq2 = list(
              subquestionTexts = stats::setNames(list("Item 2"), lang),
              helpTexts = stats::setNames(list("Explain item 2"), lang),
              default = stats::setNames(list("1"), lang),
              mandatory = "Y"
            )
          ),
          answerOptions = list(
            "1" = stats::setNames(list("Low"), lang),
            "2" = list(optionTexts = stats::setNames(list("High"), lang))
          )
        )
      )
    )
  )
}

multiple_choice_seed <- function(lang = "de") {
  list(
    settings = list(
      language = lang,
      titles = stats::setNames(list("Multiple choice survey"), lang)
    ),
    structure = list(
      grp1 = list(
        q1 = list(
          type = "multiple choice",
          questionTexts = stats::setNames(list("Select all that apply"), lang),
          subquestions = list(
            sq1 = stats::setNames(list("Option A"), lang),
            sq2 = stats::setNames(list("Option B"), lang)
          )
        )
      )
    )
  )
}

bilingual_seed <- function() {
  list(
    settings = list(
      language = "de",
      titles = list(de = "Deutsche Umfrage", en = "English survey"),
      descriptions = list(de = "Beschreibung", en = "Description")
    ),
    structure = list(
      grp1 = list(
        groupOptions = list(
          titles = list(de = "Gruppe", en = "Group"),
          descriptions = list(de = "Block", en = "Block")
        ),
        q1 = list(
          type = "radio",
          questionTexts = list(de = "Frage", en = "Question"),
          helpTexts = list(de = "Hilfe", en = "Help"),
          other = "Y",
          other_replace_text = list(de = "Sonst", en = "Other"),
          answerOptions = list(
            "1" = list(de = "Ja", en = "Yes"),
            "2" = list(de = "Nein")
          )
        )
      )
    )
  )
}

quota_seed <- function(lang = "de") {
  seed <- radio_seed(lang)
  seed$quota <- list(
    quota_a = list(
      limit = 100L,
      members = list(q1 = "1"),
      messageTexts = stats::setNames(list("Quota reached"), lang),
      urls = stats::setNames(list("https://example.invalid/quota"), lang),
      urlDescriptions = stats::setNames(list("More info"), lang)
    )
  )
  seed
}

write_seed_files <- function(seed, dir, with_quota = !is.null(seed$quota)) {
  yaml::write_yaml(seed$settings, file.path(dir, "settings.yml"))
  yaml::write_yaml(seed$structure, file.path(dir, "structure.yaml"))
  if (with_quota) {
    yaml::write_yaml(seed$quota, file.path(dir, "quota.YML"))
  }
  invisible(dir)
}
