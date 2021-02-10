#experiment with images

library(raster)
filen <- "/Users/npm4/Downloads/Lakes380_DOUGL_LC2U_2B_2020-11-12_21-55-33/capture/Lakes380_DOUGL_LC2U_2B_2020-11-12_21-55-33.raw"

overview <- raster::brick("/Users/npm4/Downloads/Lakes380_DOUGL_LC2U_2B_2020-11-12_21-55-33/capture/Lakes380_DOUGL_LC2U_2B_2020-11-12_21-55-33.raw")

rgbi <- specimR:::getNearestWavelengths(spectra = c(630,532,465),filen = overview)

rgb <- raster::subset(overview,subset = rgbi)

mat <- as.matrix(raster::mean(rgb))
across <- apply(mat,2,mean)
down <- apply(mat,1,mean)

line <- across


findCropEdges <- function(line,basereg =  c(0.25,0.75),cutthresh = .1, thresh = 5){

  starti <- c(round(c(0.25,0.75)*length(line)))

  baseline <- line[starti[1]:starti[2]]
  z <- scale(line,center = mean(baseline),scale = sd(baseline))

  outside <- (z > thresh | z < -thresh)
  so <- zoo::rollmean(as.numeric(outside),round(length(line)/10))

  allBad <- which(so > cutthresh)

  if(length(allBad) > 0){
    #check start
    midpoint <- round(0.5*length(line))
    if(any(allBad<midpoint)){
      cut1 <- max(allBad[allBad<midpoint])
    }else{
      cut1 <- 1
    }
    if(any(allBad>midpoint)){
      cut2 <- min(allBad[allBad>midpoint])
    }else{
      cut2 <- length(line)
    }
  }else{
    cut1 <- 1
    cut2 <- length(line)
  }

  return(c(cut1,cut2))
}

cropVert <- findCropEdges(down)
cropHor <- findCropEdges(across)


cropped <- raster::crop(rgb,raster::extent(rgb,cropVert[1],cropVert[2],cropHor[1],cropHor[2]))


cropim <- maptools::as.im(cropped)


mac <-  quantile(values(cropped),.99)
mic <- quantile(values(cropped),.01)
stretch <- (cropped-mic)/(mac-mic)
stretch[stretch > 1] <- 1
stretch[stretch < 0] <- 0
stretch <- (stretch * 254)+1
plotRGB(stretch)

stretchfun <- function(lay,na.rm = TRUE){
  mac <-  quantile((lay),.99,na.rm = na.rm)
  mic <- quantile((lay),.01,na.rm = na.rm)
  stretch <- (lay-mic)/(mac-mic)
  stretch[stretch > 1] <- 1
  stretch[stretch < 0] <- 0
  stretch <- (stretch * 254)+1
  return(stretch)
}

as.mat <- c(as.matrix(cropped))
test <- imager::as.cimg(as.mat,x = ncol(cropped),y = nrow(cropped),cc = 3)
hist.eq <- function(im) imager::as.cimg(ecdf(im)(im),dim=dim(im))
#split image into rgb channels
cn <- imager::imsplit(im,"c")
#equalise each channel individually
cn.eq <- imager::map_il(cn,hist.eq)
#recombine and plot
im2 <- imager::imappend(cn.eq,"c")

plotRGB(cropped,stretch = "hist")



plotRGB(rgb,stretch = "lin")
