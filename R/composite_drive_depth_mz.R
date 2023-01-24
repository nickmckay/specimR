#' Create composite proxy profile from multiple ROIs
#'
#' @param directory character path to selected drive
#' @param .tofile if TRUE write output to file
#'
#' @return a tibble with composite record for selected drive
#' @export
#'
#' @description If there are multiple ROIs within single drive, merge them into composite
#' Works with purrr::map for multiple drives if directory is a vector of paths
#'
#' @examples
create_composite_drive <- function(directory = NA_character_, .tofile = TRUE) {
  # List depth table files in the root drive directory
  depth_data <- fs::dir_ls(directory, regexp = "depthTable", recurse = TRUE) |>
    # Filter so paths to photos are not recorded
    fs::path_filter(regexp = "photos", invert = TRUE) |>
    # Read files
    vroom::vroom(id = "path", .name_repair = "universal")

  # List files with measurements in the root drive directory
  spectral_data <- fs::dir_ls(directory, regexp = "spectralIndices", recurse = TRUE) |>
    # Read files
    vroom::vroom(id = "path", .name_repair = "universal")

  # Translate depths
  roi_top <- depth_data |>
    # Get only top of the ROI
    dplyr::filter(position == "roiTop") |>
    # Get ROI number
    dplyr::mutate(roi = stringr::str_extract(path, pattern = "roi-[:digit:]+"))

  # Join with the spectral indices
  translated_data <- spectral_data |>
    # Get ROI number
    dplyr::mutate(roi = stringr::str_extract(path, pattern = "roi-[:digit:]+")) |>
    # Full join
    dplyr::left_join(roi_top, by = "roi") |>
    # Composite depth
    dplyr::mutate(composite = depth + cm, .after = depth)

  # Should tibble be written to file
  if (.tofile == TRUE) {
    readr::write_csv(x = translated_data, file = paste0(directory, "/composite.csv"))
  } else {
    # Return data
    return(translated_data)
  }
}
