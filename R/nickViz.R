#' Title
#'
#' @param index
#'
#' @return
#' @export
#'
#' @examples
getColorsByIndex <- function(index){

if("RABD615" == index){
  pall <- "GnBu"
  }
if("RABD660" == index){
  pall <- "BuGn"
}
if("RABD660670" == index){
  pall <- "Greens"
}
if("RABD845" == index){
  pall <- "Blues"
}


#band ratios
if("R570R630"== index){
  pall <- "YlOrRd"
}

if("R590R690" == index){
  pall <- "Purples"
}

  cols <- list(line = RColorBrewer::brewer.pal(name = pall,n = 7)[3],
               smooth = RColorBrewer::brewer.pal(name = pall,n = 7)[7],palette = pall)
return(cols)
}

#' Title
#'
#' @param rasDat
#' @param depthScale
#' @param palette
#' @param cmPerPixel
#'
#' @return
#' @export
#'
#' @examples
plotHeatmap <- function(rasDat,depthScale,cmPerPixel,palette = "Greens"){
  #depth
 syf <- rev(depthScale)
  # Heatmap
  plotOut <- rasDat %>%
    as.matrix() %>%

    # Data wrangling
    as_tibble() %>%
    rowid_to_column(var="depthIndex") %>%
    tidyr::gather(key="X", value="index", -1) %>%

    # Change X to numeric
    mutate(X=as.numeric(gsub("V","",X))*cmPerPixel) %>%

    #convert to depth
    mutate(depth = syf[depthIndex]) %>%
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
                                  width.mult = 2,
                                  plot.width = 30,
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

height <- info$height*normalized$cmPerPixel
width <- info$width*normalized$cmPerPixel

ggimg <- ggplot2::ggplot(data.frame(x = 0, y = 0), ggplot2::aes_string("x","y")) +
  ggplot2::geom_blank() +
  ggplot2::coord_fixed(expand = FALSE, xlim = c(0, width),ylim = c(-height,0)) +
  ggplot2::annotation_raster(cimg, 0, width, -height, 0, interpolate = TRUE)

ticks <- ggplot_build(ggimg)$layout$panel_params[[1]]$y$breaks

ggimg <- ggimg+scale_y_continuous(depth.label,labels = abs(ticks))+
  theme(axis.title.x=element_blank(),
        axis.text.x=element_blank(),
        axis.ticks.x=element_blank())+
  geom_rect(aes(xmin = roi@xmin,
                xmax = roi@xmax,
                ymin = roi@ymin,
                ymax = roi@xmax),
            color = "red",
            fill = "none")




plots <- vector(mode = "list",length = length(index.name)*2+1)
plots[[1]] <- ggimg
for(i in 1:length(index.name)){
  #get colors by index
  cols <- getColorsByIndex(index.name[i])
# make a line plot
# line plot
plots[[2*i+1]] <- plotVerticalIndex(ind,index.name = index.name[i],line.color = cols$line,smooth.color = cols$smooth)+scale_x_continuous(sec.axis = dup_axis())

if(i<length(index.name)){
  plots[[2*i+1]] <- plots[[2*i+1]] +   theme(axis.title.y=element_blank(),
                                             axis.text.y=element_blank(),
                                             axis.ticks.y=element_blank())
}else{
  plots[[2*i+1]] <- plots[[2*i+1]] +
    scale_y_reverse("Depth (cm)",position = "right",expand = c(0,0))+
    theme(axis.title.y.right = element_text(angle = 90))
}

#make a heatmap
plots[[2*i]] <- makeHeatmap(normalized, index = index.name[i],tol = tol) %>%
  plotHeatmap(depthScale = normalized$scaleY,cmPerPixel = normalized$cmPerPixel,palette = cols$palette) +
  theme(axis.title.y=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y=element_blank(),
        panel.background = element_blank(),
        plot.margin=unit(c(1,-.5,1,-0.5), "cm"))
#make a dashboard plot
}

widths <- c(width.mult,rep(c(1,plot.width),times = length(index.name)))

#egg
outplot <- egg::ggarrange(plots = plots,nrow = 1,widths = widths,padding = 0)

return(outplot)

}



