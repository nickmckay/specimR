pre_create_rgb <- function(path){
  # Get data
  raster <- terra::rast(x = path)

  # Named vector of RGB
  rgb_val <- c(r = 625, g = 525, b = 460)

  # Get closest to rgb_val
  rgb_positions <- specimR::spectra_position(raster, rgb_val) |>
    # Subset for pseudoRGB
    terra::subset(subset = _,
                  NSE = FALSE,
                  filename = "products/rgb_preview.tiff") # needs to be corrected for package use with core name etc.
}
