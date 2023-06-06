#' Read spectral indices data from directory
#'
#' @param path path to the root core directory containing the drives.
#'
#' @return named list of spectral indices from drives within the core.
#' @export
#'
read_hsi <- function(path) {
  # List drives
  drive_dirs <- fs::dir_ls(path = path, type = "directory")

  # Read data in
  data <- purrr::map(drive_dirs, \(x) fs::dir_ls(x, recurse = TRUE, glob = "*Indices.csv")) |>
    # Read at every directory, ragged because of 1n elements
    purrr::map_depth(.depth = 2, \(x) arrow::read_csv_arrow(x), .ragged = TRUE) |>
    # Bind to tibble by roi
    purrr::map_at(dplyr::where(\(x) length(x) > 1), \(x) purrr::list_rbind(x, names_to = "path")) |>
    # Extract roi where multiple rois
    purrr::map(\(x) dplyr::mutate(x, roi = stringr::str_extract(path, pattern = "(?<=roi-)\\d+"), .after = path)) |>
    # Add roi if NA (1 roi case)
    purrr::map(\(x) dplyr::mutate(x, roi = as.numeric(tidyr::replace_na(roi, "1")))) |>
    # Arrange by roi
    purrr::map(\(x) dplyr::arrange(x, roi))

  # Return
  return(data)
}

#' Read depth tables data from directory
#'
#' @param path path to the root core directory containing drives.
#'
#' @return named list of depth tables from drives within the core.
#' @export
#'
read_depth <- function(path) {
  # List drives
  drive_dirs <- fs::dir_ls(path = path, type = "directory")

  # Read data in
  data <- purrr::map(drive_dirs, \(x) fs::dir_ls(x, recurse = TRUE, glob = "*Table.csv")) |>
    # Read at every directory, ragged because of 1n elements
    purrr::map_depth(.depth = 2, \(x) arrow::read_csv_arrow(x), .ragged = TRUE) |>
    # Bind to tibble by roi
    purrr::map_at(dplyr::where(\(x) length(x) > 1), \(x) purrr::list_rbind(x, names_to = "path")) |>
    # Extract roi where multiple rois
    purrr::map(\(x) dplyr::mutate(x, roi = stringr::str_extract(path, pattern = "(?<=roi-)\\d+"), .after = path)) |>
    # Add roi if NA (1 roi case)
    purrr::map(\(x) dplyr::mutate(x, roi = as.numeric(tidyr::replace_na(roi, "1")))) |>
    # Arrange by roi
    purrr::map(\(x) dplyr::arrange(x, roi))

  # Return
  return(data)
}

#' Create composite drive from multiple rois
#'
#' @param depth a named list of tibbles with depths by roi.
#' @param indices a named list of tibble with spectral indices
#'
#' @return a tibble with depths translated for each drive.
#' @export
#'
composite_drive <- function(depth, indices) {
  # Calculate depth shift
  depth <- depth |>
    # Bind to tibble by drive id
    purrr::list_rbind(names_to = "drive") |>
    # Nest
    tidyr::nest(.by = c(drive, roi)) |>
    # For each drive calculate values
    dplyr::mutate(
      core_top = purrr::map(data, \(x) dplyr::filter(x, position == "coreLinerTop")$cm),
      core_bottom = purrr::map(data, \(x) dplyr::filter(x, position == "coreLinerBottom")$cm),
      roi_top = purrr::map(data, \(x) dplyr::filter(x, position == "roiTop")$cm),
      roi_bottom = purrr::map(data, \(x) dplyr::filter(x, position == "roiBottom")$cm)
    ) |>
    # Calculate distances from top
    dplyr::mutate(
      core_bottom = purrr::map2(core_bottom, core_top, \(x, y) x - y),
      roi_top = purrr::map2(roi_top, core_top, \(x, y) x - y),
      roi_bottom = purrr::map2(roi_bottom, core_top, \(x, y) x - y),
      core_top = purrr::map2(core_top, core_top, \(x, y) x - y)
    ) |>
    # Drop data
    dplyr::select(-data) |>
    # Unnest
    tidyr::unnest(cols = -c(drive, roi))

  # Get indices
  indices <- indices |>
    # Bind to tibble by drive id
    purrr::list_rbind(names_to = "drive") |>
    # Join with depth data
    dplyr::left_join(depth, by = dplyr::join_by(drive, roi)) |>
    # Calculate in liner depth
    dplyr::mutate(depth_liner = depth + roi_top, .after = depth) |>
    # Remove duplicated depths
    dplyr::distinct(depth_liner, .keep_all = TRUE)

  # Return indices
  return(indices)
}
