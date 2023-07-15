# General workflow
# Get capture raster from shiny output
# Get white reference raster from shiny output
# Get dark reference raster from shiny output
# Set workflow mode 1: normalize or not
# Set workflow mode 2: normalize full or cropped
# Set workflow mode 3: normalize rois only?
# Set workflow mode 4: normalize subset of layers
# Set workflow mode 5: different white reference capture time
#

# Get raster files into the environment - make it so shiny output is always called choices?
#' Prepare core based on shiny output
#'
#' @param path
#' @param .normalize
#'
#' @return
#' @export
#'
prepare_core <- function(path = choices$directory, .normalize = choices$analysisOptions$normalize) {
  # List data files in the directory
  files <- fs::dir_ls(paste0(path, "/capture"))

  # Check if file needs to be normalized from .raw
  if (.normalize == TRUE) {
    # List files: CAPTURE, DARKREF and WHITEREF
    files <- fs::path_filter(files, regexp = ".raw")

    # Read SpatRasters
    rasters <- files |>
      # Load SpatRasters
      purrr::map(\(x) terra::rast(x))

    # Get band positions - must be the same for all three SpatRasters
    band_position <- specimR::spectra_position(rasters[[1]], choices$layers)

    # Subset bands
    rasters_subset <- rasters |>
      purrr::map(\(x) specimR::spectra_sub(raster = x, spectra_tbl = band_position))
  } else {
    reflectance <- fs::path_filter(files, regexp = "REFLECTANCE")
  }
}
