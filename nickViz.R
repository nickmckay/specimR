library(ggplot2)
library(tidyr)
library(tibble)
library(dplyr)

plotSpectralDashboard <- function(){}
widthMult <- 3

# Heatmap
 plotData <- br %>%
  as.matrix() %>%

  # Data wrangling
  as_tibble() %>%
  rowid_to_column(var="depthIndex") %>%
  gather(key="X", value="index", -1) %>%

  # Change X to numeric
  mutate(X=as.numeric(gsub("V","",X))*normalized$cmPerPixel) %>%

   #convert to depth
   mutate(depth = ind$depth[depthIndex])



  # Viz
  rasPlot <- ggplot(plotData,aes(X, depth, fill= index)) +
  geom_raster() +
  theme(legend.position="none")+
  coord_equal()+
  scale_y_reverse(expand = c(0,0))+
    theme(axis.text        = element_blank(),
          axis.ticks       = element_blank(),
          axis.title       = element_blank(),
          panel.background = element_blank())


  # line plot
  linPlot <- ggplot(ind)+
    geom_path(aes(y = depth,x = RABD660),color = "gray70")+
    geom_path(aes(y = depth,x = smoothRABD660),color = "black")+
    theme_bw()+
    scale_y_reverse(expand = c(0,0))


  #image
  img <- magick::image_read(normalized$pngPath)

  #crop it based on the roi
  roi <- normalized$roi

  iroi <- magick::geometry_area(width = widthMult*round(roi@xmax-roi@xmin),height = round(roi@ymax-roi@ymin),x_off = roi@xmin-(widthMult-1)/2*round(roi@xmax-roi@xmin),y_off = roi@ymin)

  cimg <- magick::image_crop(img,geometry = iroi,gravity = "SouthWest") %>%
    magick::image_normalize()

  ggimg <- magick::image_ggplot(cimg)

  #egg
  outplot <- egg::ggarrange(ggimg,rasPlot,linPlot,nrow = 1,widths = c(widthMult,1,5))

print(outplot)


# 615
# 660/670
# 845
# R570R630
# R590R690
