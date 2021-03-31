
#' Plot composite spectral dashboard
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
plotCompositeSpectralDashboard <- function(normList,
                                           coreTable,
                                           ind,
                                           processed.image.dir = file.path(normalized$outputDir,"photos"),
                                           index.name = "RABD660",
                                           depth.label = "Depth (cm)",
                                           core.width = 4,
                                           plot.width = 8,
                                           page.width = 10,
                                           y.tick.interval = 5,
                                           page.units = "cm",
                                           tol = 1,
                                           output.file.path = NA,
                                           output.dpi = 600){
  #make a composite plot

  #imagery first



  #get the image
  if(is.na(processed.image.dir)){
    #select it
  }

  #get the processed image path (want full png with scale so that ROI is in right spot)
  fullPath <- list.files(path = processed.image.dir,pattern = "fullImage*",full.names = TRUE)
  img <- magick::image_read(fullPath)

  #get the big ROI
  load(file.path(processed.image.dir,"bigRoi.Rdata"))

  #get ROI info!
  for(ni in 1:length(normList)){

    normalized <- normList[[ni]]

    roi <- normalized$roi

    #decide how to crop it.
    xOffset <- min(bigRoi@xmin,roi@xmin)
    yOffset <- roi@ymin #use ROI exactly
    rightPos <- max(bigRoi@xmax,roi@xmax)
    topPos <- roi@ymax #use ROI exactly
    width <- rightPos-xOffset
    height <- topPos-yOffset

    #crop it based on the roi
    roi <- normalized$roi#roi relative to the whole scan

    #adjust for compositee depths!
    #get roi boundaries in cm
    cmRoiTibRow <- tibble::tibble(
      x = 0,
      y = 0,
      xmin = max(roi@xmin - xOffset + 1,1)*normalized$cmPerPixel,
      xmax = min(roi@xmax - xOffset + 1,rightPos)*normalized$cmPerPixel,
      ymin =  max(roi@ymin - yOffset + 1,1)*normalized$cmPerPixel+coreTable$compositeRoiTopDepth[ni],
      ymax = min(roi@ymax - yOffset + 1,topPos)*normalized$cmPerPixel+coreTable$compositeRoiTopDepth[ni])

    if(ni == 1){
      cmRoiTib <- cmRoiTibRow
    }else{
      cmRoiTib <- dplyr::bind_rows(cmRoiTib,cmRoiTibRow)
    }
  }

  #now plot it!
  c.height <- max(coreTable$compositeDepthAtBottomCoreliner)
  c.width <- cinfo$width*normalized$cmPerPixel

  depth.ticks <- seq(0,c.height,by = y.tick.interval)

  for(ni in 1:length(normList)){
    normalized <- normList[[ni]]

    roi <- normalized$roi

    #decide how to crop it.
    xOffset <- min(bigRoi@xmin,roi@xmin)
    yOffset <- roi@ymin #use ROI exactly
    rightPos <- max(bigRoi@xmax,roi@xmax)
    topPos <- roi@ymax #use ROI exactly
    width <- rightPos-xOffset
    height <- topPos-yOffset

    #crop it based on the roi
    roi <- normalized$roi#roi relative to the whole scan


    iroi <- magick::geometry_area(width = width,height = height, x_off = xOffset,y_off = yOffset)

    cimg <- magick::image_crop(img,geometry = iroi,gravity = "SouthWest")


    cinfo <- magick::image_info(cimg)


    if(ni == 1){
      ggimg <- ggplot2::ggplot(cmRoiTib, ggplot2::aes_string("x","y")) +
        ggplot2::geom_blank() +
        ggplot2::coord_fixed(expand = FALSE, xlim = c(0, c.width),ylim = c(-c.height,0)) +
        ggplot2::scale_y_continuous(depth.label,labels = rev(depth.ticks),breaks = -rev(depth.ticks))+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
    }

    ggimg <- ggimg+
      ggplot2::annotation_raster(cimg, 0, c.width, -coreTable$compositeRoiBottomDepth[ni], -coreTable$compositeRoiTopDepth[ni], interpolate = FALSE)

  }

  ggimg <- ggimg +
    geom_rect(aes(xmin = xmin,
                  xmax = xmax,
                  ymin = -ymin,
                  ymax = -ymax),
              color = "red",
              fill = NA)
}



plots <- vector(mode = "list",length = length(index.name)*2+1)
plots[[1]] <- ggimg
for(i in 1:length(index.name)){
  #get colors by index
  cols <- getColorsByIndex(index.name[i])
  # make a line plot
  # line plot
  plots[[2*i+1]] <- plotVerticalIndex(ind,index.name = index.name[i],line.color = cols$smooth,smooth.color = cols$smooth,smooth.width = 0)+scale_x_continuous(sec.axis = dup_axis())

  if(i<length(index.name)){
    plots[[2*i+1]] <- plots[[2*i+1]] +   theme(axis.title.y=element_blank(),
                                               axis.text.y=element_blank(),
                                               axis.ticks.y=element_blank())
  }else{
    plots[[2*i+1]] <- plots[[2*i+1]] +
      scale_y_reverse("Depth (cm)",position = "right",expand = c(0,0),breaks = rev(depth.ticks))+
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

rel.widths <- c(core.width,rep(c(1,plot.width),times = length(index.name)))
widths <- unit(rel.widths/sum(rel.widths)*page.width,units = page.units)
page.length <- rel.widths[1]/sum(rel.widths)*page.width*c.height/c.width

#egg
outplot <- egg::ggarrange(plots = plots,nrow = 1,widths = widths,padding = 0,draw = FALSE,clip = "on")

if(!is.na(output.file.path)){
  ggsave(plot = outplot,
         filename = output.file.path,
         width = page.width*2,
         height = page.length*2,
         units = page.units,dpi = output.dpi,
         limitsize = FALSE)
}

return(outplot)

}
