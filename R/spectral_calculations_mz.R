#' Calculate Relative Absorption Band Depth (RABD)
#'
#' @param raster a terra SpatRaster of normalized capture data
#' @param edges a numeric vector of two for the wide calculation window
#' @param trough a character vector of wavelenght to look for trough
#' @param rabd_name a character, name of calculated RABD
#'
#' @return a terra SpatRaster with one layer with calculated RABD values
#' @export
#'
#' @description # RABD655−680max = ( X × R590 + Y × R730 X+Y ) /R655−680min
#' Denominator: Find the lowest reflectance between 655 and 680
#' Numenator: find reflectance at 590, find reflectance at 730
#' Find how many bands there are between trough minimum and 730
#' Find how many bands there are between trough minimum and 590
#'
calculate_rabd <- function(raster, edges, trough, rabd_name) {
  # Create SpatRaster template from original, select one layer
  template <- terra::subset(raster, 1)

  # Set layer name based on the rabd_name argument
  names(template) <- rabd_name

  # Find trough position
  trough_position <- spectra_position(raster = raster, spectra = trough) |>
    # Pull vector with positions
    dplyr::pull(var = 2) |>
    # Subset normalized raster to match trough
    (\(x) terra::subset(raster, x))() |>
    # Find trough position
    (\(x) as.numeric(min(x)[1]))() |>
    # Find trough position in the original raster
    (\(x) terra::which.lyr(raster == x))() |>
    # Coerce to integer
    (\(x) as.integer(x[1]))()

  # Find minimum reflectance value in the trough (denominator)
  trough_reflectance <- raster[, , trough_position] |>
    # Coerce to numeric
    as.numeric()

  # Find edge positions
  edge_positions <- spectra_position(raster = raster, spectra = edges) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Find reflectance value of the left edge (lower wavelength)
  ledge_reflectance <- raster[, , edge_positions[1]]

  # Find reflectance value of the right edge (higher wavelength)
  redge_reflectance <- raster[, , edge_positions[2]]

  # Find number of the bands between through minimum and left edge (lower wavelength, Y)
  ledge_width <- abs(trough_position - edge_positions[1])

  # Find number of the bands between through minimum and right edge (higher wavelength, X)
  redge_width <- abs(trough_position - edge_positions[2])

  # Calculate equation nominator
  nominator <- (redge_width * ledge_reflectance + ledge_width * redge_reflectance) /
    (redge_width + ledge_width)

  # Calculate RABD
  rabd <- nominator / trough_reflectance

  # If there are inifinites coerce to 0
  rabd[is.infinite(rabd)] <- 0

  # Set rabd values onto SpatRaster template
  values(template) <- rabd

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(template, filename = glue::glue(
    {rabd_name}, ".tif"
  ), overwrite = TRUE)
}

#' Calculate band ratio
#'
#' @param raster a terra SpatRaster of normalized capture data
#' @param edges a numeric vector of two for the (nominator and denominator)
#' @param ratio_name a character, name of calculated ratio
#'
#' @return a terra SpatRaster with one layer with calculated ratio values
#' @export
#'
#' @description calculate band ratio of selected wavelengths
#'
calculate_band_ratio <- function(raster, edges, ratio_name) {
  # Find edge positions
  edge_positions <- spectra_position(raster = raster, spectra = edges) |>
    # Pull vector with positions
    dplyr::pull(var = 2)

  # Divide
  raster <- terra::subset(raster, edge_positions[1]) /
    terra::subset(raster, edge_positions[2])

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(raster, filename = glue::glue(
    {ratio_name}, ".tif"
  ), overwrite = TRUE)
}

#' calculate rMean
#'
#' @param raster a terra SpatRaster of normalized capture data
#'
#' @return a terra SpatRaster with one layer with calculated rMean values
#' @export
#'
#' @description calculate mean reflectance from all layers for given pixel
#'
calculate_rmean <- function(raster) {
  # Apply mean function over entire SpatRaster
  raster <- terra::app(raster, fun = "mean")

  # Write new raster to file based on paths stored in the environment
  terra::writeRaster(raster, filename = "rmean.tif", overwrite = TRUE)
}

# Function: calculate RABA - to do
calculate_raba <- function(raster) {

}

#' Extract average proxy profile
#'
#' @param raster a terra SpatRaster with one layer with calculated values
#'
#' @return a data frame with XY coordinates and averaged proxy values
#' @export
#'
extract_profile <- function(raster) {
  # Aggregate SpatRaster into average rows
  profile <- terra::aggregate(raster, fact = c(1, ncol(raster)), fun = "mean") |>
    # Coerce do data frame with coordinates
    terra::as.data.frame(xy = TRUE)

  # Return object
  return(profile)
}

# Create list of lists with proxies and their settings
# This way user can easily expand it in session to add or edit settings
# Use it later in the function, by extracting list elements in approriate places
proxies_settings <- list(
  "RABD655680" = list(
    edges = c(590, 730),
    trough = 655:680,
    rabd_name = "RABD655680"
  ),
  "RABD845" = list(
    edges = c(590, 730),
    trough = 655:680,
    rabd_name = "RABD845"
  ),
  "RABD615" = list(
    edges = c(590, 730),
    trough = 655:680,
    rabd_name = "RABD615"
  ),
  "R570R630" = list(
    edges = c(570, 630),
    ratio_name = "R570R630"
  ),
  "R590R690" = list(
    edges = c(590, 730),
    ratio_name = "R590R690"
  ),
  "R950R970" = list(
    edges = c(590, 730),
    ratio_name = "R950R970"
  )
)
