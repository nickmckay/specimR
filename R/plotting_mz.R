# Plotting
# Plotting RGB file
# Plotting colored map of proxy (RABD, Ratio etc)
# Plotting profile
# Put core and profile/map pairs along - let user choose

#' Spatial map plots of calculated proxies
#'
#' @param raster a SpatRaster with calculated hyperspectral indices and RGB layers.
#' @param hsi_index a character indicating hyperspectral index layer to plot.
#' @param .palette a character indicating one of color palettes of choice. One of: "red", "orange", "yellow", "green", "cyan", "blue", "purple", "magenta".
#'
#' @return a plot with color map of selected hyperspectral index.
#' @export
plot_raster_proxy <- function(raster, hsi_index, .palette) {
  # Choose palette
  proxy_palette <- list(
    red = c(low = "#AF3029", high = "#FFFCF0"),
    orange = c(low = "#BC5215", high = "#FFFCF0"),
    yellow = c(low = "#AD8301", high = "#FFFCF0"),
    green = c(low = "#66800B", high = "#FFFCF0"),
    cyan = c(low = "#24837B", high = "#FFFCF0"),
    blue = c(low = "#205EA6", high = "#FFFCF0"),
    purple = c(low = "#5E409D", high = "#FFFCF0"),
    magenta = c(low = "#A02F6F", high = "#FFFCF0"))

  # Subset SpatRaster
  hsi_layer <- raster |>
    terra::subset(hsi_index)

  # Plot SpatRaster
  plot <- ggplot2::ggplot() +
    # Add raster layer
    tidyterra::geom_spatraster(data = hsi_layer) +
    # Define fill colors
    scale_fill_gradient(
      low = proxy_palette[[.palette]][1],
      high = proxy_palette[[.palette]][2],
      guide = guide_colorbar(
        title = hsi_index,
        title.position = "bottom",
        ticks = FALSE)
    ) +
    # Modify theme
    theme(
      panel.background = element_blank(),
      axis.line.y.left = element_line(color = "black"),
      axis.text.x = element_blank(),
      axis.ticks.x = element_blank(),
      legend.position = "bottom")

  # Return plot as an object
  return(plot)
}

# RGB plot of the object
plot_raster_rgb <- function(data){

}

# Overlay color plot of proxy on RGB
plot_raster_overlay <- function(data, hsi_index){

}

#' Composite hyperspectral indices plots
#' Can composite line profiles and SpatRasters
#'
#' @param plots a list of plots.
#'
#' @return a plot.
#' @export
plot_raster_composite <- function(plots) {
  # Create a plot composited from a list of plots
  plot <- plots |>
    # Wrap list into patchwork
    patchwork::wrap_plots() +
    # Setup layout and collect axes
    patchwork::plot_layout(
      nrow = 1,
      axes = "collect")

  # Return plot as an object
  return(plot)
}

#' Line plots of calculated proxies
#'
#' @param raster a SpatRaster with calculated hyperspectral indices and RGB layers.
#' @param hsi_index a character indicating hyperspectral index layer to plot.
#' @param .palette a character indicating one of color palettes of choice. One of: "red", "orange", "yellow", "green", "cyan", "blue", "purple", "magenta".
#'
#' @return a plot with color map of selected hyperspectral index.
#' @export
plot_profile_proxy <- function(data, hsi_index, .palette){
  # Choose palette
  proxy_palette <- list(
    red = c(low = "#AF3029", high = "#FFFCF0"),
    orange = c(low = "#BC5215", high = "#FFFCF0"),
    yellow = c(low = "#AD8301", high = "#FFFCF0"),
    green = c(low = "#66800B", high = "#FFFCF0"),
    cyan = c(low = "#24837B", high = "#FFFCF0"),
    blue = c(low = "#205EA6", high = "#FFFCF0"),
    purple = c(low = "#5E409D", high = "#FFFCF0"),
    magenta = c(low = "#A02F6F", high = "#FFFCF0"))
}
