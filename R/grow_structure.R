#' Build survey structure (groups, questions, options)
#'
#' Populates a `limonaid::Survey` R6 object with groups, questions, answer
#' options, and subquestions based on the `structure` element of a LimeSeed
#' sprout produced by `lime_sprout()`.
#'
#' @param tree A `limonaid::Survey` object, typically created by
#'   `grow_settings()`.
#' @param sprout Named list as returned by `lime_sprout()` containing a
#'   `structure` element that defines groups and questions.
#'
#' @return The modified `limonaid::Survey` object, returned invisibly.
#'
#' @details
#' For each group defined in `sprout$structure`, a group is added to `tree`
#' using titles, descriptions, relevance, and randomization options. For each
#' question within a group, a `limonaid::Question` is created with its texts,
#' validation, relevance, display and behavior settings. If present, answer
#' options and subquestions are added supporting:
#' - simple unnamed vectors (value used as both code and label),
#' - named lists (names as codes), and
#' - nested lists providing multilingual texts and additional attributes.
#'
#' Messages are emitted to trace progress while building the structure. The
#' function modifies `tree` by reference and returns it invisibly.
#'
#' @seealso `lime_sprout()`, `grow_settings()`, `grow_quota()`
#' @export
grow_structure <- function(tree, sprout = sprout) {
  structure <- sprout$structure

  message("Processing structure. Add groups and questions...\n")

  # 1. Process Groups
  for (grp_code in names(structure)) {
    message(grp_code)

    # 2. Get group attributes
    grp_data <- structure[[grp_code]]
    grp_name <- process_text(grp_data$groupOptions$titles, grp_code)

    # 3. Add group
    tree$add_group(
      titles = grp_name,
      descriptions = process_text(grp_data$groupOptions$descriptions),
      relevance = grp_data$groupOptions$relevance %||% 1,
      random_group = grp_data$groupOptions$random_group %||% ""
    )

    # 4. Get last added group object
    grp_idx <- length(tree$groups)
    current_group <- tree$groups[[grp_idx]]
    # (for some reason: current_group is a list (no methods to add question))

    # 5. Process Questions (for current group)
    for (qst_code in names(grp_data)) {
      if (qst_code == "groupOptions") {
        next
      }

      message("\t", qst_code)
      qst_data <- grp_data[[qst_code]]

      # 6. Instantiate Question Manually
      # First, define the explicit parameters
      explicit_params <- list(
        code = qst_code,
        type = qst_data$type,
        lsType = qst_data$lsType,
        questionTexts = process_text(qst_data$questionTexts),
        helpTexts = process_text(qst_data$helpTexts),
        relevance = qst_data$relevance %||% 1,
        validation = qst_data$validation %||% "",
        language = qst_data$language %||% tree$language,
        additional_languages = qst_data$additional_languages %||% "",
        mandatory = qst_data$mandatory %||% "N",
        other = qst_data$other %||% "N",
        otherReplaceTexts = process_text(qst_data$otherReplaceTexts),
        default = process_text(qst_data$default),
        same_default = qst_data$same_default %||% 0,
        array_filter = qst_data$array_filter %||% "",
        question_order = qst_data$question_order %||% 0,
        cssclass = qst_data$cssclass %||% "",
        hide_tip = qst_data$hide_tip %||% "",
        hidden = qst_data$hidden,
        random_order = qst_data$random_order,
        text_input_width = qst_data$text_input_width,
        input_size = qst_data$input_size,
        random_group = qst_data$random_group %||% "",
        maximum_chars = qst_data$maximum_chars,
        min_num_value = qst_data$min_num_value,
        max_num_value = qst_data$max_num_value,
        min_num_value_n = qst_data$min_num_value_n,
        max_num_value_n = qst_data$max_num_value_n,
        num_value_int_only = qst_data$num_value_int_only,
        prefix = process_text(qst_data$prefix),
        suffix = process_text(qst_data$suffix)
      )

      # Get the names of explicit parameters
      explicit_param_names <- names(explicit_params)

      # Extract any additional options from qst_data that aren't in explicit_params
      additional_options <- qst_data[
        !names(qst_data) %in%
          c(
            explicit_param_names,
            "answerOptions",
            "subquestions",
            "otherOptions"
          )
      ]

      # Create the question using do.call to pass both explicit and additional params
      qst_obj <- do.call(
        limonaid::Question$new,
        c(explicit_params, additional_options)
      )

      # 7. Add Answer Options (to the R6 Question Object)
      # Supports: 1. Unnamed List, 2. Named List, 3. Nested List with Args
      if (!is.null(qst_data$answerOptions)) {
        opts <- qst_data$answerOptions

        if (!is.null(names(opts))) {
          # --- Case 2 & 3: Named List (Key is the Code) ---
          for (code_val in names(opts)) {
            content <- opts[[code_val]]

            # Check if it is Case 3 (Nested list with explicit 'optionTexts' key)
            if (is.list(content) && !is.null(content$optionTexts)) {
              qst_obj$add_answer_option(
                code = code_val,
                optionTexts = unlist(content$optionTexts, use.names = TRUE),
                type.scale = content$type.scale %||% 0,
                relevance = content$relevance %||% "",
                assessment.value = content$assessment.value %||% 0,
                sort.order = content$sort.order # NULL is fine here
              )
            } else {
              # Case 2: Simple Key-Value (Code: Label)
              qst_obj$add_answer_option(
                code = code_val,
                optionTexts = unlist(content, use.names = TRUE)
              )
            }
          }
        } else {
          # --- Case 1: Unnamed List (Value is Code AND Label) ---
          for (i in seq_along(opts)) {
            val <- as.character(opts[[i]])
            qst_obj$add_answer_option(code = val, optionTexts = val)
          }
        }
      }

      # 8. Add Subquestions (to the R6 Object)
      # Supports: 1. Code: Label, 2. Code: {Lang: Label}, 3. Code: {subquestionTexts:..., args...}
      if (!is.null(qst_data$subquestions)) {
        subs <- qst_data$subquestions

        if (!is.null(names(subs))) {
          # Case 2 & 3: Named List (Key is the Code) ---
          for (code_val in names(subs)) {
            content <- subs[[code_val]]

            # Check if it is Case 3: Nested list with explicit 'subquestionTexts' key
            if (is.list(content) && !is.null(content$subquestionTexts)) {
              qst_obj$add_subquestion(
                code = code_val,
                subquestionTexts = process_text(content$subquestionTexts),
                relevance = content$relevance %||% "",
                helpTexts = process_text(content$helpTexts),
                type.scale = content$type.scale %||% 0,
                validation = content$validation %||% "",
                mandatory = content$mandatory %||% "",
                default = content$default %||% "",
                same_default = content$same_default %||% "",
                subquestion.order = content$subquestion.order
                # TODO: check if we could add suffix here (for multiple text/numerical inputs)
              )
            } else {
              # Case 2: Simple Key-Value or Multilingual List (Content is the Label)
              qst_obj$add_subquestion(
                code = code_val,
                subquestionTexts = process_text(content)
              )
            }
          }
        } else {
          # Case 1: Unnamed List (Value is Code AND Label) ---
          for (i in seq_along(subs)) {
            val <- as.character(subs[[i]])
            qst_obj$add_subquestion(code = val, subquestionTexts = val)
          }
        }
      }

      # 9. Inject Question into Survey
      tree$groups[[grp_idx]]$questions[[qst_code]] <- qst_obj
    }
  }

  return(invisible(tree))
}
