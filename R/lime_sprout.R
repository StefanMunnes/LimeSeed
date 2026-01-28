#' Load and Prepare Survey Seed
#'
#' @param seed
#' 1. path to single file containing all (necessary) elements (settings, structure)
#' 2. path to folder containing all the (necessary) files (named accordingly)
#' 3. named list containing paths to files for (necessary) elements
#' 4. named list containing paths to files and (pre-loaded or created) elements as list
#' 5. (pre-loaded or created) list with all (necessary) elements
#' @import yaml
#' @export
lime_sprout <- function(seed) {
  # Internal templates
  sprout <- list(settings = NULL, structure = NULL, quota = NULL)

  # --- CASE 1 & 2: Input is a Character Path ---
  if (is.character(seed) && length(seed) == 1) {
    if (dir.exists(seed)) {
      # Case 2: Path to a folder (containing the separated YAML files)

      files <- list.files(
        seed,
        pattern = "\\.yml|\\.yaml",
        ignore.case = TRUE,
        full.names = TRUE
      )

      elements <- basename(files) |> tools::file_path_sans_ext()

      files_valid <- files[elements %in% c("settings", "structure", "quota")]

      if (length(files_valid) < 2) {
        stop(
          "Input folder path does not contain 'settings' and 'structure' files."
        )
      }

      sprout <- lapply(files_valid, yaml::read_yaml)

      names(sprout) <- basename(files_valid) |> tools::file_path_sans_ext()
    } else if (file.exists(seed)) {
      # Case 1: Path to a single YAML file (containing everything)

      if (tools::file_ext(seed) %in% c("yml", "yaml")) {
        sprout <- yaml::read_yaml(seed)
      } else {
        stop("Input file path missing valid .yml or .yaml file extension.")
      }

      if (!all(c("settings", "structure") %in% names(sprout))) {
        stop("Input file does not contain 'settings' and 'structure' keys.")
      }
    } else {
      stop("Path specified in 'seed' does not exist.")
    }
  } else if (is.list(seed)) {
    # check if it's a valid named list
    if (is.null(names(seed))) {
      stop("If you provide a list as `seed`, it must be a named.")
    }
    if (!all(nzchar(names(seed)))) {
      stop("If you provide a list as `seed`, all elements must be named.")
    }
    if (!all(c("settings", "structure") %in% names(seed))) {
      stop("Input list does not contain named key 'settings' and 'structure'")
    }

    if (
      all(sapply(seed, class) == "character") &&
        all(lengths(seed) == 1)
    ) {
      # Case 3: It's a named list of paths (Case 3)
      sprout <- lapply(seed, read_yaml)
    } else if (
      any(sapply(seed, class) == "character") &&
        any(sapply(seed, class) == "list")
    ) {
      # Case 4: It's a named list of paths and R lists

      sprout <- lapply(seed, function(s) {
        if (is.character(s) && length(s) == 1 && file.exists(s)) {
          read_yaml(s)
        } else if (is.list(s)) {
          s
        } else {
          stop(
            "When providing a named list, each element must be either a YAML file path or a list."
          )
        }
      })
    } else {
      # Case 5: It's allready a proper list
      sprout <- seed
    }
  }

  return(sprout)
}
