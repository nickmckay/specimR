# Plotting
# Plotting RGB file
# Plotting colored map of proxy (RABD, Ratio etc)
# Plotting profile
# Put core and profile/map pairs along - let user choose

# Color plots of calculated proxies
plot_raster_proxy <- function(data, hsi_index){
  # Subset SpatRaster
  hsi_layer <- data |>
    terra::subset(hsi_index)

  # Plot SpatRaster
  plot <- ggplot2::ggplot() +
    tidyterra::geom_spatraster(data = hsi_layer) +
    scale_fill_gradient(guide = guide_colorbar(title = hsi_index,
                                               title.position = "bottom",
                                               ticks = FALSE)) +
    theme(panel.background = element_blank(),
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

# Composite plots
plot_raster_composite <- function(plots){
  plot <- plots |>
    patchwork::wrap_plots() +
    patchwork::plot_layout(nrow = 1,
                           axes = "collect")

  # Return plot as an object
  return(plot)
}

# Plots of profiles
plot_profile_proxy <- function(data, hsi_index){
}
