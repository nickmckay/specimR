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

# Get raster files into the environment
prepare_core <- function(path = choices$directory, .normalize = choices$analysisOptions$normalize) {
  # List data files in the directory
  files <- fs::dir_ls(paste0(path, "/capture"))

  # Check if file needs to be normalized
  if (.normalize == TRUE) {
    files <- fs::path_filter(files, regexp = ".raw")
  } else {
    reflectance <- fs::path_filter(files, regexp = "REFLECTANCE")
  }
}
