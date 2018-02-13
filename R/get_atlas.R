get_atlases_dir <- function() ni_globals$atlas_cache_dir

set_atlases_dir <- function(dir) ni_globals$atlas_cache_dir <- suppressWarnings(normalizePath(dir))

get_atlas <- function(atlas_name,
                      hosts = c("github", "bitbucket"),
                      force_download = FALSE) {

  # Where the dataset is stored
  repo_name <- "neuroimaginador/ni.atlases"
  destination_folder <- get_atlases_dir()

  # What happens when the output directory exists?
  if (dir.exists(destination_folder)) {

    if (force_download) {

      unlink(destination_folder, recursive = TRUE)
      message("Deleting previous folder.")

    } else {

      possible_atlases <- list.files(path = destination_folder, pattern = atlas_name, full.names = TRUE)

      return(possible_atlases)

    }

  }

  # Let's check possible hosts
  for (host in hosts) {

    if (host == "github") {

      remote <- devtools:::github_remote(repo_name)

      path <- try(devtools:::remote_download(remote), silent = TRUE)

      if (!inherits(path, "try-error")) {

        break

      }

    }

    if (host == "bitbucket") {

      remote <- devtools:::bitbucket_remote(repo_name)

      path <- try(devtools:::remote_download(remote), silent = TRUE)

      if (!inherits(path, "try-error")) {

        break

      }

    }

  }

  if (!inherits(path, "try-error")) {

    # Ok, unzip the dataset and create destination folder if it does not exist.
    dir.create(get_atlases_dir(), recursive = TRUE, showWarnings = FALSE)
    L <- unzip(zipfile = path, exdir = dirname(get_atlases_dir()), list = TRUE)
    unzip(zipfile = path, exdir = dirname(get_atlases_dir()))
    extracted_folder <- L$Name[1]

    file.rename(from = file.path(dirname(get_atlases_dir()), extracted_folder),
                to = destination_folder)

    # files_to_move <- list.files(file.path(get_atlases_dir(), extracted_folder), full.names = FALSE)
    #
    # foo <- lapply(files_to_move, function(f) file.rename(from = file.path(get_atlases_dir(), extracted_folder, f),
    #                                                      to = destination_folder))
    #
    # unlink(destination_folder)
    possible_atlases <- list.files(path = destination_folder, pattern = atlas_name, full.names = TRUE)

    return(invisible(possible_atlases))

  } else {

    stop("Atlas repository not found.")

  }

}

