select_input_output_bids <- function(path, interactive = FALSE) {

  c(features, derivatives) := import_bids_dataset(path)

  available_features <- setdiff(colnames(features), c("subject", "session"))
  available_derivatives <- setdiff(colnames(derivatives), c("subject", "session"))

  data <- dplyr::full_join(features, derivatives)

  all_features <- c(available_features, available_derivatives)

  # Manage several inputs
  if (length(all_features) > 1 & interactive) {

    choices <- all_features

    title <- "Select inputs to use:"
    chosen_inputs <- select.list(choices = c(choices, "all"), multiple = TRUE, title = title)
    if ("all" %in% chosen_inputs) chosen_inputs <- choices

  } else {

    chosen_inputs <- available_features

  }

  num_inputs <- length(chosen_inputs)

  inputs <- data %>% dplyr::select(chosen_inputs) %>% as.list()

  # Manage outputs
  # Check if there are more than one possible output
  if (length(all_features) > 0) {

    # Select one of them, if we are in an interactive session
    if (interactive) {

      # nocov start

      choices <- all_features

      title <- "Choose a possible output:"
      chosen_output <- utils::select.list(choices = choices, title = title)

      # nocov end

    } else {

      # If in a non-interactive session, take the first one of the derivatives, if it exists

      if (length(available_derivatives) > 0) {

        chosen_output <- available_derivatives[1]

      } else {


        # Just in case we want an autoencoder
        chosen_output <- available_features[1]

      }

    }

  }

  outputs <- data %>% dplyr::select(chosen_output) %>% unlist() %>% unname()

  return(list(input = inputs, outputs = outputs))

}