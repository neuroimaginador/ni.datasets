import_bids_dataset <- function(path) {

  # There should be sub-* subfolders for all subjects in the dataset
  # Bear in mind that there can be multiple sessions per subject (ses-*)
  # Inside each subject:
  # - folder: anat/ and sub-*..._<modality_label>.nii
  # - folder: func/ and sub-*..._task-<task_label>..._bold.nii
  # - folder: dwi/  and sub-*..._dwi.nii, .bval and .bvec
  # Then, maybe a derivatives/ folder with:
  # One or many folders such as <derivative_label>/ and one subfolder for each subject, e.g.:
  # derivatives/brain_mask/sub-01/[ses-*/]sub-01_brain_mask.nii


  subfolders <- list.dirs(path = path, full.names = FALSE, recursive = FALSE)

  subject_folders <- subfolders[grep(pattern = "sub-", x = subfolders)]
  derivative_folders <- subfolders[grep(pattern = "derivatives", x = subfolders)]

  available_subjects <- gsub(subject_folders, pattern = "sub-", replacement = "")

  my_subjects <- data.frame(stringsAsFactors = FALSE)

  for (s_id in seq_along(subject_folders)) {

    s <- subject_folders[s_id]
    sub_id <- available_subjects[s_id]

    tmp <- import_subject_bids(subject_folder = file.path(path, s))

    df <- data.frame(subject = rep(sub_id, times = nrow(tmp)), stringsAsFactors = FALSE)

    this_subject <- cbind(df, tmp)

    my_subjects <- dplyr::bind_rows(my_subjects,
                                    this_subject)

  }

  # Remove empty columns
  for (col in colnames(my_subjects)) {

    if (all(is.na(my_subjects[[col]])))
      my_subjects[[col]] <- NULL

  }

  if (length(derivative_folders) > 0) {

    my_derivatives <- import_derivatives_bids(deriv_folder = file.path(path, derivative_folders))

  }

  return(list(features = my_subjects, derivatives = my_derivatives))

}

#' Must return one row per session
import_subject_bids <- function(subject_folder) {

  # Check if there are sessions
  # Inside each possible session: look for anat/, func/ and dwi/ folders, and import them.

  subfolders <- list.dirs(path = subject_folder, full.names = FALSE, recursive = FALSE)
  subject_id <- regmatches(subject_folder, regexpr(pattern = "sub-\\w*", text = subject_folder))
  subject_id <- gsub(subject_id, pattern = "sub-", replacement = "")

  session_folders <- subfolders[grep(pattern = "ses-", x = subfolders)]
  session_id <- gsub(session_folders, pattern = "ses-", replacement = "")

  my_sessions <- data.frame(stringsAsFactors = FALSE)

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

  anat_folder <- subfolders[grep(pattern = "anat", x = subfolders)]
  func_folder <- subfolders[grep(pattern = "func", x = subfolders)]
  dwi_folder  <- subfolders[grep(pattern = "dwi", x = subfolders)]

  if (length(anat_folder) > 0) {

    anat_res <- import_anatomical_bids(anat_folder = file.path(session_folder, anat_folder))

  } else {

    anat_res <- data.frame("T1w" = NA)

  }

  if (length(func_folder) > 0) {

    func_res <- import_functional_bids(func_folder = file.path(session_folder, func_folder))

  } else {

    func_res <- data.frame("nofunc" = NA)

  }

  if (length(dwi_folder) > 0) {

    dwi_res <- import_diffusion_bids(dwi_folder = file.path(session_folder, dwi_folder))

  } else {

    dwi_res <- data.frame("dwi" = NA)

  }

  return(cbind(anat_res, func_res, dwi_res))

}

import_anatomical_bids <- function(anat_folder) {

  anat_files <- list.files(path = anat_folder, pattern = ".nii")

  available_modalities <- c("T1w", "T2w", "T1rho", "T1map", "T2map", "T2star", "FLAIR", "FLASH", "PD", "PDmap", "PDT2", "inplaneT1",
                            "inplaneT2", "angio", "defacemask", "SWImagandphase")

  my_modalities <- lapply(available_modalities,
                          function(mod) {

                            tmp <- anat_files[grep(anat_files, pattern = mod)]

                            if (length(tmp) > 0) {

                              return(file.path(anat_folder, tmp[1]))

                            } else {

                              return(NA)

                            }

                          })
  names(my_modalities) <- paste0("ANAT: ", available_modalities)

  my_modalities <- as.data.frame(my_modalities, stringsAsFactors = FALSE)

  return(my_modalities)

}

import_functional_bids <- function(func_folder) {

  func_files <- list.files(path = func_folder, pattern = "_bold.nii")

  tasks <- regmatches(func_files, regexpr(pattern = "task-[[:print:]]*\\_bold", text = func_files))

  tasks <- gsub(tasks, pattern = "task-", replacement = "")
  tasks <- gsub(tasks, pattern = "\\_bold", replacement = "")

  my_tasks <- lapply(tasks, function(t) {

    task_file <- list.files(path = func_folder, pattern = paste0("task-", t, "\\_bold.nii"), full.names = TRUE)

  })
  names(my_tasks) <- paste0("FUNC: ", tasks)

  my_tasks <- as.data.frame(my_tasks, stringsAsFactors = FALSE)

  return(my_tasks)

}

import_diffusion_bids <- function(dwi_folder) {

  dwi_files <- as.list(list.files(path = dwi_folder, pattern = "_dwi.nii", full.names = TRUE))

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

  derivatives <- list.dirs(path = deriv_folder, full.names = FALSE, recursive = FALSE)

  my_derivatives <- data.frame(stringsAsFactors = FALSE)

  for (der in derivatives) {

    subjects <- list.dirs(path = file.path(deriv_folder, der), full.names = FALSE, recursive = FALSE)
    subjects <- subjects[grep(pattern = "sub-", x = subjects)]

    this_derivative <- c()
    subject_id <- c()

    for (s in subjects) {

      # subject_id <- c(subject_id, gsub(s, pattern = "sub-", replacement = ""))
      subject_id <- gsub(s, pattern = "sub-", replacement = "")

      sessions <- list.dirs(path = file.path(deriv_folder, der, s), full.names = FALSE, recursive = FALSE)
      sessions <- sessions[grep(pattern = "ses-", x = sessions)]

      if (length(sessions) >= 1) {

        this_derivative <- data.frame(stringsAsFactors = FALSE)

        for (ses in sessions) {

          files <- list.files(path = file.path(deriv_folder, der, s, ses), full.names = TRUE)

          session_id <- gsub(ses, pattern = "ses-", replacement = "")

          this_derivative <- dplyr::bind_rows(this_derivative,
                                              data.frame(subject = subject_id,
                                                         session = session_id,
                                                         condition = der,
                                                         this_der = files[1],
                                                         stringsAsFactors = FALSE))
        }

      } else {

        files <- list.files(path = file.path(deriv_folder, der, s), full.names = TRUE)
        this_derivative <- data.frame(subject = subject_id,
                                      session = "nosess",
                                      condition = der,
                                      this_der = files[1],
                                      stringsAsFactors = FALSE)

      }

      colnames(this_derivative) <- c("subject", "session", "condition", "this_der")

      my_derivatives <- dplyr::bind_rows(my_derivatives, this_derivative)

    }

  }

  require(tidyr)
  my_derivatives <- my_derivatives %>% spread(key = "condition", value = "this_der")

  return(my_derivatives)

}