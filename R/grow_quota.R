#' Load and Prepare Survey Seed
#'
#' @param tree
#' survey object
#' @param sprout
#' list from imported YAML file created by `lime_sprout` or manually created
#' @export
grow_quota <- function(tree, sprout = sprout) {
  if (!is.null(sprout$quota)) {
    stop("Quota is not available in provided YAML file(s).")
  }

  quota <- sprout$quota

  # process quota
  for (quota_code in names(quota)) {
    message("Processing quota: ", quota_code)

    quota_data <- quota[[quota_code]]

    if (is.null(quota_data$limit)) {
      stop("Quota has no limit.")
    }
    if (is.null(quota_data$members)) {
      stop("Quota has no members.")
    }
    if (!inherits(quota_data$members, "list")) {
      stop("Quota members is not a list.")
    }
    if (is.null(names(quota_data$members))) {
      stop("Quota members need names.")
    }

    tree$add_quota(
      name = quota_code,
      limit = quota_data$limit,
      members = quota_data$members,
      action = quota_data$action %||% 1,
      active = quota_data$active %||% 1,
      autoloadURL = quota_data$autoloadURL %||% 0,
      messageTexts = process_text(quota_data$messageTexts),
      urls = process_text(quota_data$urls),
      url_descriptions = process_text(quota_data$url_descriptions)
    )
  }
}
