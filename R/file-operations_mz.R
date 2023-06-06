#' Get paths to files
#'
#' @param directory directory to search for files
#'
#' @return a list of paths pointing to the overview and capture files
#' @export
#'
#' @description search for an overview file (*.png) and three files of captured data
#' core capture, white and dark references (*.raw).
#'
get_paths <- function(directory = NA) {
  # Invoke file browser with print to console to get core directory
  if (is.na(directory)) {
    # Console alert
    cli::cli_alert_info(cli::style_bold(cli::col_green("Choose an overview image file within the core directory")))

    # Get path to overview *.png file
    overview <- fs::fs_path(choose.files())
  }

  # Create empty list to store paths
  paths <- list()

  # Store root directory path in the list based on the overview path
  paths[["directory"]] <- fs::path_dir(overview)

  # Get core name from overview file
  paths[["corename"]] <- as.character(fs::path_ext_remove(fs::path_file(overview)))

  # Store overview path in list
  paths[["overview"]] <- overview

  # Construct path to the capture *.raw file
  capture <- fs::path(fs::path(paths[["directory"]], "capture", paths[["corename"]]), ext = "raw")

  # If paths is correct
  if (fs::file_exists(capture)) {
    paths[["capture"]] <- capture
    # If paths is incorrect choose file
  } else {
    cli::cli_alert_info(cli::style_bold(cli::col_green("Choose the capture .raw file")))
    paths[["capture"]] <- fs::fs_path(choose.files())
  }

  # Construct path to the white reference *.raw file
  whiteref <- fs::path(fs::path(paths[["directory"]], "capture", paste0("WHITEREF_", paths[["corename"]])), ext = "raw")

  # If paths is correct
  if (file.exists(whiteref)) {
    paths[["whiteref"]] <- whiteref
  } else {
    cli::cli_alert_info(cli::style_bold(cli::col_green("Choose the WHITEREF .raw file")))
    paths[["whiteref"]] <- fs::fs_path(choose.files())
  }

  # Construct path to the dark reference *.raw file
  darkref <- fs::path(fs::path(paths[["directory"]], "capture", paste0("DARKREF_", paths[["corename"]])), ext = "raw")

  # If paths is correct
  if (file.exists(darkref)) {
    paths[["darkref"]] <- darkref
    # If paths is incorrect choose file
  } else {
    cli::cli_alert_info(cli::style_bold(cli::col_green("Choose the DARKREF .raw file")))
    paths[["darkref"]] <- fs::fs_path(choose.files())
  }

  # Return list of the paths
  return(paths)
}

# I'm not sure whats happening here -M Is it replacement?
filechooseR <- function(id,directory = NA){
  Filters <- matrix(c("*",".raw"),1, 2, byrow = TRUE)
  data <- file.path(directory,paste(id,".raw",sep=""))
  if(missing(data)) data <- tcltk::tk_choose.files(caption="choose Data File", filter = Filters)
  filen <- raster::brick(data)
  return(filen)
}

# Use choose.files and store as sanitized FS that work across the different OSes

# Use fs:: to create directories later in the pipeline

# Use cli:: to print all messages to console across the files
