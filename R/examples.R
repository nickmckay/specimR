################################################################################
#                                                         Normalization workflow
# Set path so directory so temporary raster and other files are written
# Later subsitute with path selected interactivelly with get_paths
paths <- list()
paths[["directory"]] <- "C:/GitHub/STL14_1A_28C_top_2022-11-11_16-30-51/"


spectra <- c(550, 570, 590, 615, 630, 649:701, 730, 790, 845, 900)

capture <- terra::rast("C:/GitHub/STL14_1A_28C_top_2022-11-11_16-30-51/capture/STL14_1A_28C_top_2022-11-11_16-30-51.raw")
big_roi <- terra::ext(c(110, 1010, 0, 16000))

capture <- spectra_position(raster = capture, spectra = spectra) |>
  spectra_sub(raster = capture, spectra_tbl = _) |>
  raster_crop(raster = _, type = "capture", roi = big_roi)

whiteref <- terra::rast("C:/GitHub/STL14_1A_28C_top_2022-11-11_16-30-51/capture/WHITEREF_STL14_1A_28C_top_2022-11-11_16-30-51.raw")

whiteref <- spectra_position(raster = whiteref, spectra = spectra) |>
  spectra_sub(raster = whiteref, spectra_tbl = _) |>
  raster_crop(raster = _, type = "reference", roi = big_roi, ref_type = "whiteref") |>
  create_reference_raster(raster = _, roi = big_roi, ref_type = "whiteref")

darkref <- terra::rast("C:/GitHub/STL14_1A_28C_top_2022-11-11_16-30-51/capture/DARKREF_STL14_1A_28C_top_2022-11-11_16-30-51.raw")

darkref <- spectra_position(raster = darkref, spectra = spectra) |>
  spectra_sub(raster = darkref, spectra_tbl = _) |>
  raster_crop(raster = _, type = "reference", roi = big_roi, ref_type = "darkref") |>
  create_reference_raster(raster = _, roi = big_roi, ref_type = "darkref")

normalized <- create_normalized_raster(capture = capture, whiteref = whiteref, darkref = darkref, fun = normalization)

# Examples for worklfow with all spectra- blow up the pc?

capture <- terra::rast("C:/Users/maury/Downloads/STL14_1A_28C_top_2022-11-11_16-30-51/capture/STL14_1A_28C_top_2022-11-11_16-30-51.raw")
big_roi <- terra::ext(c(110, 1010, 0, 16000))

capture <- raster_crop(raster = capture, type = "capture", roi = big_roi)

whiteref <- terra::rast("C:/Users/maury/Downloads/STL14_1A_28C_top_2022-11-11_16-30-51/capture/WHITEREF_STL14_1A_28C_top_2022-11-11_16-30-51.raw")

whiteref <- raster_crop(raster = whiteref, type = "reference", roi = big_roi) |>
  create_reference_raster(raster = _, roi = big_roi)

darkref <- terra::rast("C:/Users/maury/Downloads/STL14_1A_28C_top_2022-11-11_16-30-51/capture/DARKREF_STL14_1A_28C_top_2022-11-11_16-30-51.raw")

darkref <- raster_crop(raster = darkref, type = "reference", roi = big_roi) |>
  create_reference_raster(raster = _, roi = big_roi)

normalized <- create_normalized_raster(capture = capture, whiteref = whiteref, darkref = darkref, fun = normalization)

################################################################################
#                                                              Spectral workflow

# Read normalized data
test <- terra::rast("C:/Users/maury/Downloads/STL14_1A_28C_top_2022-11-11_16-30-51/capture/test.tif")

# 1 px extent
ext_1p <- terra::ext(c(600, 620, 300, 320))

# Crop to 1 px
test_1p <- terra::crop(test, ext_1p)

test_rabd <- calculate_rabd(raster = test, edges = c(590, 730), trough = 655:680, rabd_name = "RABD655680")
test_ratio <- calculate_band_ratio(raster = test_1p, edges = c(570, 630), ratio_name = "R570R630")

profile <- extract_profile(raster = test_new)
ggplot(dplyr::filter(profile, RABD655680 <= 1.5), aes(y, RABD655680)) + geom_path() + coord_flip()
