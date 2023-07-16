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
  cli::cli_h1("{basename(path)}")

  # Create products directory and store path
  products <- fs::dir_create(paste0(path, "/products"))

  # List data files in the directory
  files <- fs::dir_ls(paste0(path, "/capture"))

  # Check if file needs to be normalized from .raw
  if (.normalize == TRUE) {
    # List files: CAPTURE, DARKREF and WHITEREF
    files <- fs::path_filter(files, regexp = ".raw")

    # SpatRaster types
    types <- list("darkref", "capture", "whiteref")

    # Extent
    big_roi <- terra::ext(choices$cropImage)

    cli::cli_alert_info("{format(Sys.time())}: reading rasters")

    # Read SpatRasters
    rasters <- files |>
      # Load SpatRasters
      purrr::map(\(x) terra::rast(x))

    # Get band positions - the same for all three SpatRasters
    band_position <- specimR::spectra_position(rasters[[1]], choices$layers)

    cli::cli_alert_info("{format(Sys.time())}: subsetting layers.")

    # Subset bands in the SpatRasters
    rasters_subset <- rasters |>
      purrr::map(\(x) specimR::spectra_sub(raster = x, spectra_tbl = band_position))

    cli::cli_alert_info("{format(Sys.time())}: cropping rasters")

    # Crop
    rasters_cropped <- purrr::map2(rasters_subset, types, \(x, y) specimR::raster_crop(x, y, big_roi, path = path))

    cli::cli_alert_info("{format(Sys.time())}: calculating reference rasters.")

    # Prepare reference SpatRasters
    rasters_references <- purrr::map2(rasters_cropped[c(1, 3)], types[c(1, 3)], \(x, y) specimR::create_reference_raster(raster = x, ref_type = y, roi = big_roi, path = path))

    cli::cli_alert_info("{format(Sys.time())}: calculating reflectance raster.")

    # Normalize data
    reflectance <- specimR::create_normalized_raster(capture = rasters_cropped[[2]],
                                                     whiteref = rasters_references[[2]],
                                                     darkref = rasters_references[[1]],
                                                     fun = normalization,
                                                     path = path)

    cli::cli_alert_info("{format(Sys.time())}: cleaning up.")

    # Remove temporary disaggregated files
    fs::dir_ls(products, regexp = "disaggregated") |>
      fs::file_delete()

    cli::cli_alert_success("{format(Sys.time())}: finished.")

  } else {
    reflectance <- fs::path_filter(files, regexp = "REFLECTANCE")
  }
}
