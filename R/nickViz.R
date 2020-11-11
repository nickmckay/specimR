#' Title
#'
#' @param rasDat
#' @param depthScale
#' @param palette
#'
#' @return
#' @export
#'
#' @examples
plotHeatmap <- function(rasDat,depthScale,palette = "Greens"){
  # Heatmap
  plotOut <- rasDat %>%
    as.matrix() %>%

    # Data wrangling
    as_tibble() %>%
    rowid_to_column(var="depthIndex") %>%
    gather(key="X", value="index", -1) %>%

    # Change X to numeric
    mutate(X=as.numeric(gsub("V","",X))*normalized$cmPerPixel) %>%

    #convert to depth
    mutate(depth = normalized$scaleY[depthIndex]) %>%
    ggplot(aes(X, depth, fill= index)) +
    geom_raster() +
    theme(legend.position="none")+
    coord_equal()+
    scale_y_continuous(expand = c(0,0))+
    scale_fill_distiller(palette = palette)+
    theme(axis.title.x=element_blank(),
          axis.text.x=element_blank(),
          axis.ticks.x=element_blank(),
          panel.background = element_blank())

  return(plotOut)

}

widthMult <- 3


#' Title
#'
#' @param ind
#' @param index.name
#' @param line.color
#' @param smooth.color
#' @param smooth.width
#'
#' @return
#' @export
#'
#' @examples
plotVerticalIndex <- function(ind,
                              index.name = "RABD660",
                              line.color = "gray70",
                              smooth.color = "black",
                              smooth.width = .5){
  linPlot <- ggplot(ind)+
    geom_path(aes_string(y = "depth",x = index.name),color = line.color)+
    geom_path(aes_string(y = "depth",x = paste0("smooth",index.name)),color = smooth.color,size = smooth.width)+
    theme_bw()+
    scale_y_reverse(expand = c(0,0))

  return(linPlot)

}


#' Title
#'
#' @param normalized
#' @param ind
#' @param index.name
#' @param depth.label
#' @param width.mult
#' @param plot.width
#' @param tol
#'
#' @return
#' @export
#'
#' @examples
plotSpectralDashboard <- function(normalized,
                                  ind,
                                  index.name = "RABD660",
                                  depth.label = "Depth (cm)",
                                  width.mult = 1,
                                  plot.width = 3,
                                  tol = 1){
#make a composite plot

#get the image
img <- magick::image_read(normalized$pngPath)

#crop it based on the roi
roi <- normalized$roi

iroi <- magick::geometry_area(width = width.mult*round(roi@xmax-roi@xmin),height = round(roi@ymax-roi@ymin),x_off = roi@xmin-(width.mult-1)/2*round(roi@xmax-roi@xmin),y_off = roi@ymin)

cimg <- magick::image_crop(img,geometry = iroi,gravity = "SouthWest") %>%
  magick::image_normalize()


info <- magick::image_info(cimg)

height <- info$height*cmPerPix
width <- info$width*cmPerPix

ggimg <- ggplot2::ggplot(data.frame(x = 0, y = 0), ggplot2::aes_string("x","y")) +
  ggplot2::geom_blank() +
  ggplot2::coord_fixed(expand = FALSE, xlim = c(0, width),ylim = c(-height,0)) +
  ggplot2::annotation_raster(cimg, 0, width, -height, 0, interpolate = TRUE)

ticks <- ggplot_build(ggimg)$layout$panel_params[[1]]$y$breaks

ggimg <- ggimg+scale_y_continuous(depth.label,labels = abs(ticks))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())

# make a line plot
# line plot
linPlot <- plotVerticalIndex(ind,index.name = index.name)+
  scale_y_reverse(depth.label,position = "right",expand = c(0,0))+theme(axis.title.y.right = element_text(angle = 90))

#make a heatmap
heat <- makeHeatmap(normalized, index = index.name,tol = tol) %>%
  plotHeatmap(depthScale = normalized$scaleY) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        plot.margin=unit(c(1,-.5,1,-0.5), "cm"))
#make a dashboard plot

#egg
outplot <- egg::ggarrange(ggimg,heat,linPlot,nrow = 1,widths = c(width.mult,1,plot.width),padding = 0)

return(outplot)

}


# 615
# 660/670
# 845
# R570R630
# R590R690
