import_bids_dataset <- function(path) {

  # There should be sub-* subfolders for all subjects in the dataset
  # Bear in mind that there can be multiple sessions per subject (ses-*)
  # Inside each subject:
  # - folder: anat/ and sub-*..._<modality_label>.nii
  # - folder: func/ and sub-*..._task-<task_label>..._bold.nii
  # - folder: dwi/  and sub-*..._dwi.nii, .bval and .bvec
  # Then, maybe a derivatives/ folder with:
  # One or many folders such as <derivative_label>/ and one subfolder for each subject, e.g.:
  # derivatives/brainmask/sub-01/[ses-*/]sub-01_brain_mask.nii

  # List of folders
  subfolders <- list.dirs(path = path, full.names = FALSE, recursive = FALSE)

  # Which are folders with subject data and which with derived data?
  subject_folders <- subfolders[grep(pattern = "sub-", x = subfolders)]
  derivative_folders <- subfolders[grep(pattern = "derivatives", x = subfolders)]

  # List of subjects
  available_subjects <- gsub(subject_folders, pattern = "sub-", replacement = "")

  # Data frame to store the results of subject data.
  my_subjects <- data.frame(stringsAsFactors = FALSE)

  # Loop over all subjects
  for (s_id in seq_along(subject_folders)) {

    # For each subject, read its corresponding folder
    s <- subject_folders[s_id]
    sub_id <- available_subjects[s_id]

    tmp <- import_subject_bids(subject_folder = file.path(path, s))

    df <- data.frame(subject = rep(sub_id, times = nrow(tmp)), stringsAsFactors = FALSE)

    # Append its results to the data frame.
    this_subject <- cbind(df, tmp)

    my_subjects <- dplyr::bind_rows(my_subjects,
                                    this_subject)

  }

  # Remove empty columns
  for (col in colnames(my_subjects)) {

    if (all(is.na(my_subjects[[col]])))
      my_subjects[[col]] <- NULL

  }

  # If there are derived data, read the corresponding folders
  if (length(derivative_folders) > 0) {

    my_derivatives <- import_derivatives_bids(deriv_folder = file.path(path, derivative_folders))

  }

  # Return a list with subject data and derived data.
  return(list(features = my_subjects, derivatives = my_derivatives))

}

#' Must return one row per session
import_subject_bids <- function(subject_folder) {

  # Check if there are sessions
  subfolders <- list.dirs(path = subject_folder, full.names = FALSE, recursive = FALSE)

  session_folders <- subfolders[grep(pattern = "ses-", x = subfolders)]
  session_id <- gsub(session_folders, pattern = "ses-", replacement = "")

  # Data frame with results
  my_sessions <- data.frame(stringsAsFactors = FALSE)

  # Loop over all sessions, importing each folder and appending to the data frame.
  if (length(session_folders) >= 1) {

    for (ses in session_folders) {

      my_sessions <- dplyr::bind_rows(my_sessions,
                                      import_session_bids(session_folder = file.path(subject_folder, ses)))

    }

    df <- data.frame(session = session_id, stringsAsFactors = FALSE)

    my_sessions <- cbind(df, my_sessions)

  } else {

    df <- data.frame(session = "nosess", stringsAsFactors = FALSE)

    my_sessions <- cbind(df, import_session_bids(session_folder = subject_folder))

  }

  return(my_sessions)

}


import_session_bids <- function(session_folder) {

  subfolders <- list.dirs(path = session_folder, full.names = FALSE, recursive = FALSE)

  # Inside each possible session: look for anat/, func/ and dwi/ folders, and import them.
  anat_folder <- subfolders[grep(pattern = "anat", x = subfolders)]
  func_folder <- subfolders[grep(pattern = "func", x = subfolders)]
  dwi_folder  <- subfolders[grep(pattern = "dwi", x = subfolders)]

  # If we have anatomical images, import the "anat/" folder, otherwise leave it blank (NA)
  if (length(anat_folder) > 0) {

    anat_res <- import_anatomical_bids(anat_folder = file.path(session_folder, anat_folder))

  } else {

    anat_res <- data.frame("T1w" = NA)

  }

  # If we have functional images, import the "func/" folder, otherwise leave it blank (NA)
  if (length(func_folder) > 0) {

    func_res <- import_functional_bids(func_folder = file.path(session_folder, func_folder))

  } else {

    func_res <- data.frame("nofunc" = NA)

  }

  # If we have diffusion images, import the "dwi/" folder, otherwise leave it blank (NA)
  if (length(dwi_folder) > 0) {

    dwi_res <- import_diffusion_bids(dwi_folder = file.path(session_folder, dwi_folder))

  } else {

    dwi_res <- data.frame("dwi" = NA)

  }

  # Return the binding of all modality-specific data frames
  return(cbind(anat_res, func_res, dwi_res))

}

import_anatomical_bids <- function(anat_folder) {

  # List of files under the anat/ folder.
  anat_files <- list.files(path = anat_folder, pattern = ".nii")

  # List of defined modalities in BIDS
  # TODO: parse "_ce-<ce_label>" for acquisitions with contrast agents
  available_modalities <- c("T1w", "T1wGad", "T2w", "T1rho", "T1map", "T2map", "T2star", "FLAIR", "FLASH", "PD", "PDmap", "PDT2", "inplaneT1",
                            "inplaneT2", "angio", "defacemask", "SWImagandphase")

  modalities_expressions <- c("T1w", "ce-[[:print:]]*_T1w", "T2w", "T1rho", "T1map", "T2map", "T2star", "FLAIR", "FLASH", "PD", "PDmap",
                              "PDT2", "inplaneT1", "inplaneT2", "angio", "defacemask", "SWImagandphase")

  # Find all available modalities for the specified subject
  my_modalities <- lapply(modalities_expressions,
                          function(mod) {

                            tmp <- anat_files[grep(anat_files, pattern = paste0("_", mod))]

                            if (length(tmp) > 0) {

                              return(file.path(anat_folder, tmp[1]))

                            } else {

                              return(NA)

                            }

                          })

  # And assign meaningful names
  names(my_modalities) <- paste0("ANAT: ", available_modalities)

  # Return as data frame (no factors, only character strings are allowed)
  my_modalities <- as.data.frame(my_modalities, stringsAsFactors = FALSE)

  return(my_modalities)

}

import_functional_bids <- function(func_folder) {

  # List of files in the func/ folder
  func_files <- list.files(path = func_folder, pattern = "_bold.nii")

  # Use a regular expression to match the name of the tasks, according to BIDS specs
  tasks <- regmatches(func_files, regexpr(pattern = "task-[[:print:]]*\\_bold", text = func_files))

  tasks <- gsub(tasks, pattern = "task-", replacement = "")
  tasks <- gsub(tasks, pattern = "\\_bold", replacement = "")

  # Just use this task names to build a data frame with the corresponding files, and meaningful column names.
  my_tasks <- lapply(tasks, function(t) {

    task_file <- list.files(path = func_folder, pattern = paste0("task-", t, "\\_bold.nii"), full.names = TRUE)

  })
  names(my_tasks) <- paste0("FUNC: ", tasks)

  my_tasks <- as.data.frame(my_tasks, stringsAsFactors = FALSE)

  return(my_tasks)

}

import_diffusion_bids <- function(dwi_folder) {

  # List of NIfTI files in the dwi/ folder
  dwi_files <- as.list(list.files(path = dwi_folder, pattern = "_dwi.nii", full.names = TRUE))

  # Append these files to a data frame with meaningful column names.
  my_dwis <- lapply(dwi_files,
                    function(f) {

                      f <- basename(f)
                      tmp <- paste0("DWI", regmatches(f, regexpr(pattern = "\\_acq-\\w*\\_dwi", text = f)))

                      return(gsub(tmp, pattern = "\\_dwi", replacement = ""))

                    })

  names(dwi_files) <- my_dwis

  return(as.data.frame(dwi_files, stringsAsFactors = FALSE))

}

import_derivatives_bids <- function(deriv_folder) {

  # List of subfolders of derivative/
  # These subfolders must correspond to derived data (segmentations, masks, parcellations or so)
  derivatives <- list.dirs(path = deriv_folder, full.names = FALSE, recursive = FALSE)

  # Initialize data frame for results
  my_derivatives <- data.frame(stringsAsFactors = FALSE)

  # Loop over all subfolders, analyze each one. It should be similar to a generic subjects structure:
  # - sub-<sub_label>)
  #   - [ses-<ses_label>]
  #     - <filename>.nii[.gz]
  for (der in derivatives) {

    # List of subjects
    subjects <- list.dirs(path = file.path(deriv_folder, der), full.names = FALSE, recursive = FALSE)
    subjects <- subjects[grep(pattern = "sub-", x = subjects)]

    # Loop over all subjects
    for (s in subjects) {

      subject_id <- gsub(s, pattern = "sub-", replacement = "")

      # Available sessions for the subject (if more than one)
      sessions <- list.dirs(path = file.path(deriv_folder, der, s), full.names = FALSE, recursive = FALSE)
      sessions <- sessions[grep(pattern = "ses-", x = sessions)]

      if (length(sessions) >= 1) {

        # Initialize data frame
        this_derivative <- data.frame(stringsAsFactors = FALSE)

        # Loop over all sessions
        for (ses in sessions) {

          # Find files in the session subfolder
          files <- list.files(path = file.path(deriv_folder, der, s, ses), full.names = TRUE)

          session_id <- gsub(ses, pattern = "ses-", replacement = "")

          # Append to data frame
          this_derivative <- dplyr::bind_rows(this_derivative,
                                              data.frame(subject = subject_id,
                                                         session = session_id,
                                                         condition = der,
                                                         this_der = files[1],
                                                         stringsAsFactors = FALSE))
        }

      } else {

        # Just a single session, just add files to the data frame
        files <- list.files(path = file.path(deriv_folder, der, s), full.names = TRUE)
        this_derivative <- data.frame(subject = subject_id,
                                      session = "nosess",
                                      condition = der,
                                      this_der = files[1],
                                      stringsAsFactors = FALSE)

      }

      # Rename columns
      colnames(this_derivative) <- c("subject", "session", "condition", "this_der")

      # Append to global data frame of derivatives.
      my_derivatives <- dplyr::bind_rows(my_derivatives, this_derivative)

    }

  }

  # Tidy data up!
  require(tidyr)
  my_derivatives <- my_derivatives %>% spread(key = "condition", value = "this_der")

  return(my_derivatives)

}