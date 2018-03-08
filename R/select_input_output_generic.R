select_input_output_generic <- function(input_path, output_path, interactive) {


  # Manage several inputs
  input_dirs <- list.dirs(path = input_path, full.names = TRUE)[-1]

  num_inputs <- 1

  if (length(input_dirs) > 1 & interactive) {

    # nocov start

    choices <- sapply(input_dirs, basename)
    title <- "Select inputs to use:"
    chosen_inputs <- select.list(choices = c(choices, "all"), multiple = TRUE, title = title)
    if ("all" %in% chosen_inputs) chosen_inputs <- choices
    chosen_inputs <- match(chosen_inputs, choices)
    num_inputs <- length(chosen_inputs)

    # nocov end

  } else {

    num_inputs <- length(input_dirs)
    chosen_inputs <- seq(num_inputs)

  }

  inputs <- list()
  for (dir in seq(num_inputs)) {

    name <- basename(input_dirs[chosen_inputs[dir]])
    inputs[[name]] <- list.files(path = input_dirs[chosen_inputs[dir]], full.names = TRUE)
    # num_all_subjects <- length(inputs[[dir]])

  }

  # Manage outputs
  # Check if there are more than one possible output
  output_dirs <- list.dirs(path = output_path, full.names = TRUE)[-1]
  if (length(output_dirs) > 0) {

    # Select one of them, if we are in an interactive session
    if (interactive) {

      # nocov start

      choices <- sapply(output_dirs, basename)
      title <- "Choose a possible output:"
      chosen_output <- utils::select.list(choices = choices, title = title)
      chosen_output <- match(chosen_output, choices)
      output_path <- output_dirs[chosen_output]

      # nocov end

    } else {

      # If in a non-interactive session, take the first one
      output_path <- output_dirs[1]

    }

  }

  outputs <- list.files(path = output_path, full.names = TRUE)

  return(list(input = inputs, outputs = outputs))

}