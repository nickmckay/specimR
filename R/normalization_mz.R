#' Find position of selected spectra
#'
#' @param raster a terra SpatRaster
#' @param spectra vector with choice of desired spectra
#'
#' @return positions (indices) of desired spectra in SpatRaster
#' @export
#'
#' @description find index position of the nearest spectra (band) in the dataset.
#' Match for the lowest difference between integer band and actual SpatRaster band.
#' This will produce duplicates with multiple bands. Drop.
spectra_position <- function(raster, spectra) {
  # Find index (position) of selected spectra by comparing choice and names
  spectraIndex <- purrr::map(spectra, \(x) which.min(abs(x - as.numeric(names(raster))))) |>
    # Get positions
    purrr::as_vector()

  # Create tibble with spectra of choice and respective position
  spectraIndex <- dplyr::tibble(
    spectra = spectra,
    position = spectraIndex) |>
    # Keep second observation if duplicates are present
    # From experience closer to desired product
    dplyr::slice_tail(by = position)

  # Return values
  return(spectraIndex)
}

#' Subset by spectra
#'
#' @param raster a terra SpatRaster to be subset
#' @param spectra_tbl a tibble with spectra positions from spectra_position
#'
#' @return SpatRaster subset to contain only required spectral bands
#' @export
#'
#' @description subset SpatRaster with spectra (bands) positions
#'
spectra_sub <- function(raster, spectra_tbl) {
  # Get spectra from tibble
  spectra <- dplyr::pull(spectra_tbl, 1)

  # Get positions from tibble
  position <- dplyr::pull(spectra_tbl, 2)

  # Subset raster by position
  raster <- terra::subset(raster, position)
  # Set raster names to match spectra
  names(raster) <- as.character(spectra)

  # Return raster
  return(raster)
}

#' Raster crop
#'
#' @param raster a terra SpatRaster to be cropped
#' @param type either data raster or reference raster
#' @param roi Region Of Interest: cropping extent
#'
#' @return a terra SpatRaster cropped to ROI
#' @export
#'
#' @description Crop SpatRaster to large ROI (entire core)
#' For capture (core) SpatRaster use full extent
#' For reference (white and dark) SpatRaster use only x-direction
#'
raster_crop <- function(raster, type, roi, ...) {
  # Store additional parameters
  params <- list(...)

  # If cropping entire capture SpatRaster use entire large ROI
  if (type == "capture") {
    raster <- terra::crop(raster,
                          roi,
                          filename = paste0(params$path, "/products/", basename(params$path), "_cropped.tif"),
                          overwrite = TRUE,
                          steps = terra::nrow(raster) * terra::nlyr(raster))

    # If cropping reference SpatRaster use only xmin and xmax from large ROI
    # White reference SpatRaster
  } else if (type == "whiteref") {
    raster <- terra::crop(raster,
                          c(terra::xmin(roi),
                            terra::xmax(roi),
                            terra::ymin(raster),
                            terra::ymax(raster)),
                          filename = paste0(params$path, "/products/WHITEREF_", basename(params$path), "_cropped.tif"),
                          overwrite = TRUE,
                          steps = terra::nrow(raster) * terra::nlyr(raster))

    # Dark reference SpatRaster
  } else if (type == "darkref") {
    raster <- terra::crop(raster,
                          c(terra::xmin(roi),
                            terra::xmax(roi),
                            terra::ymin(raster),
                            terra::ymax(raster)),
                          filename = paste0(params$path, "/products/DARKREF_", basename(params$path), "_cropped.tif"),
                          overwrite = TRUE,
                          steps = terra::nrow(raster) * terra::nlyr(raster))
  }

  # Return raster
  return(raster)
}

#' Create reference raster
#'
#' @param raster a terra SpatRaster of the captured reference
#' @param roi Region Of Interest: extent to match data raster
#'
#' @return a terra SpatRaster of reference matching the data raster extent
#' @export
#'
#' @description Creating reference SpatRaster covering core extent
#' Create one mean reference row SpatRaster by averaging data every column by aggregation
#' Create reference SpatRaster matching capture SpatRaster extent by disaggregation
#'
create_reference_raster <- function(raster, roi, ref_type, ...) {
  # Store additional parameters
  params <- list(...)

  if (ref_type == "whiteref") {
    name <- "WHITEREF"
  } else {
    name <- "DARKREF"
  }
  # Aggregate data into one row SpatRaster, divide by number of rows

  raster <- terra::aggregate(raster,
                             fact = c(terra::nrow(raster), 1),
                             fun = "mean",
                             overwrite = TRUE,
                             steps = terra::nrow(raster) * terra::nlyr(raster))

  # Set new extent to match extent of capture SpatRaster
  terra::ext(raster) <- roi

  # Disaggregate data over entire extent to match capture SpatRaster extent, multiply by ymax
  raster <- terra::disagg(raster,
                          fact = c(terra::ymax(raster), 1),
                          filename = paste0(params$path, "/products/", name, "_", basename(params$path), "_disaggregated.tif"),
                          overwrite = TRUE,
                          steps = terra::nrow(raster) * terra::nlyr(raster))

  # Return raster
  return(raster)
}

#' Raster normalization: calculation
#'
#' @param capture a terra SpatRaster of captured data
#' @param whiteref a terra SpatRaster of the white reference matching capture extent
#' @param darkref a terra SpatRaster of the dark reference matching capture extent
#'
#' @return a normalized terra SpatRaster of the capture
#' @export
#'
#' @description normalize captured hyperspectral data with white and dark reference according to equation from Butz et al 2016
#'
normalization <- function(capture = capture, whiteref = whiteref, darkref = darkref) {
  # Calculate numenator (above the bar)
  numenator <- capture - darkref

  # Coerce NA to 0
  numenator[is.na(numenator)] <- 0

  # Coerce negative values to 0
  numenator[numenator < 0] <- 0

  # Calculate denominator (below the bar)
  denominator <- whiteref - darkref

  # Normalize
  raster <- numenator / denominator

  # Return raster
  return(raster)
}

#' Raster normalization
#'
#' @param capture a terra SpatRaster of captured data
#' @param whiteref a terra SpatRaster of the white reference matching capture extent
#' @param darkref a terra SpatRaster of the dark reference matching capture extent
#' @param fun function to apply: normalization
#'
#' @return a normalized terra SpatRaster of the capture
#' @export
#'
#' @description apply normalization function over the combination of capture and reference SpatRasters using terra spatial dataset
#'
create_normalized_raster <- function(capture = capture, whiteref = whiteref, darkref = darkref, fun = normalization, ...) {
  # Store additional parameters
  params <- list(...)

  # Named list with write options
  wopts <- list(steps = terra::nrow(capture) * 10)

  # Create terra spatial dataset combining SpatRasters
  # Create list
  dataset <- list(capture, whiteref, darkref) |>
    # Create terra dataset
    terra::sds()

  # Apply function over the dataset and write to file
  raster <- terra::lapp(x = dataset,
                        fun = fun,
                        filename = paste0(params$path, "/products/REFLECTANCE_", basename(params$path), ".tif"),
                        overwrite = TRUE,
                        wopt = wopts)
}

#' Smooth raster with focal median
#'
#' @param capture a terra SpatRaster of captured data.
#' @param window focal window size, default is 3.
#'
#' @return smoothed SpatRaster
#' @export
#'
median_filtering <- function(raster = raster, window = 3, ...){
  # Store additional parameters
  params <- list(...)

  # Named list with write options
  wopts <- list(steps = terra::nrow(raster) * terra::nlyr(raster))

  # Apply terra focal statistic with 3 x 3 window
  reflectance <- terra::focal(raster,
                              w = window,
                              fun = \(x) median(x),
                              filename = paste0(params$path, "/products/REFLECTANCE_", basename(params$path), "_MEDIAN.tif"),
                              overwrite = TRUE,
                              wopt = wopts)

}

#' Apply a Savitzky-Golay smoothing filter
#'
#' @description
#' Smooth data with a Savitzky-Golay smoothing filter using \code{\link[signal]{sgolayfilt}}.
#'
#' @param raster a terra SpatRaster of normalized data
#' @param p filter order.
#' @param n filter length (must be odd).
#' @param m return the m-th derivative of the filter coefficients.
#' @param ts time scaling factor.
#'
#' @return A filtered terra SpatRaster.
#' @export
#'
filter_savgol <- function(raster, p = 3, n = p + 3 - p%%2, m = 0, ts = 1){
  # Store additional parameters
  params <- list(...)

  # Named list with write options
  wopts <- list(steps = terra::nrow(raster) * terra::nlyr(raster))

  # Extract names
  band_names <- names(raster)

  # Apply Savitzky-Golay filter
  raster <- terra::app(raster,
                       fun = \(raster) signal::sgolayfilt(raster, p = p, n = n, m = m, ts = ts),
                       filename = paste0(params$path, "/products/REFLECTANCE_", basename(params$path), "_SAVGOL.tif"),
                       overwrite = TRUE,
                       wopt = wopts)

  # Set names
  names(raster) <- as.character(band_names)

  # Update names on disk
  update(raster, names = TRUE)

  # Return raster to the environment
  return(raster)
}
