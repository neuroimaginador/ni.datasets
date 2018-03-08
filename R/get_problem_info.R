#' @title FUNCTION_TITLE
#'
#' @description FUNCTION_DESCRIPTION
#'
#' @param problem_path    (character) PARAM_DESCRIPTION, Default: 'foo'
#' @param input_path      (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param output_path     (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param num_subjects    (NULL) PARAM_DESCRIPTION, Default: NULL
#' @param interactive     (logical) PARAM_DESCRIPTION, Default: TRUE
#' @param as_autoencoder  (logical) PARAM_DESCRIPTION, Default: FALSE
#'
#' @return OUTPUT_DESCRIPTION
#'
#' @details DETAILS
#' @seealso
#'  \code{\link[neurobase]{readnii}}
#'  \code{\link[dplyr]{near}}
#'  \code{\link[utils]{select.list}}
#' @export
#' @importFrom neurobase readnii
#' @importFrom dplyr near
#' @importFrom utils select.list
get_problem_info <- function(problem_path = NULL,
                             input_path = NULL,
                             output_path = NULL,
                             num_subjects = NULL,
                             interactive = TRUE,
                             as_autoencoder = FALSE) {

  if (is.null(problem_path) & is.null(input_path)) {

    stop("At least one of 'problem_path' or 'input_path' must be provided.")

  }

  info <- new.env()
  interactive <- interactive & interactive()

  if (file.exists(problem_path) ||
      (!is.null(input_path) && file.exists(input_path))) {

    if (is.null(input_path) & file.exists(problem_path))
      input_path <- file.path(problem_path, "inputs")

    if (!file.exists(input_path)) {

      # Maybe it's a BIDS dataset
      # Check that, at least, we have subject folders
      subject_folders <- list.files(path = problem_path, pattern = "sub-")

      if (length(subject_folders) > 0) {

        # BIDS!!
        c(inputs, outputs) := select_input_output_bids(path = problem_path, interactive = interactive)

      } else {

        # None of our readable structures
        stop("Folder structure not compatible.")

      }

    } else {

      # We are at a "inputs"/"outputs" structure.

      if (is.null(output_path) & file.exists(problem_path))
        output_path <- file.path(problem_path, "outputs")

      if (is.null(output_path) || !file.exists(output_path))
        output_path <- input_path

      # Special case for when we are defining an autoencoder
      if (as_autoencoder) {

        output_path <- input_path

      }

      c(inputs, outputs) := select_input_output_generic(input_path = input_path, output_path = output_path, interactive = interactive)

    }

    num_all_subjects <- length(outputs)
    num_inputs <- length(inputs)

    if (!is.null(num_subjects)) {

      if (num_subjects < num_all_subjects) {

        for (dir in seq(num_inputs)) {

          inputs[[dir]] <- inputs[[dir]][seq(num_subjects)]

        }

        outputs <- outputs[seq(num_subjects)]

      } else {

        num_subjects <- num_all_subjects

      }

    } else {

      num_subjects <- num_all_subjects

    }

    info <- info %>% analyze_input(input = inputs) %>% analyze_output(output = outputs)

    info$input_path <- input_path
    info$output_path <- output_path
    info$num_inputs <- num_inputs
    info$num_subjects <- num_subjects
    info$inputs <- inputs
    info$outputs <- outputs

  } else {

    stop("Not a valid path.")

  }

  # info <- as.environment(info)
  class(info) <- c("DLproblem", class(info))

  return(info)

}
