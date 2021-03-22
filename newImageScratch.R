#experiment with images

library(raster)
filen <- "/Volumes/Lakes380 WKO/SCANNED CORES/WKO/Lake Ngapouri/Lakes380_NGAPO_LC4U_2B_S2_2019-11-05_02-02-28/capture/Lakes380_NGAPO_LC4U_2B_S2_2019-11-05_02-02-28.raw"

overview <- raster::brick(filen)

rgbi <- specimR:::getNearestWavelengths(spectra = c(630,532,465),filen = overview)


rgbnorm <- normalize(wavelengths =  c(630,532,465),output.dir = "~/Downloads/Lakes380_NGAPO_LC4U_2B_S2_2019-11-05_02-02-28/")


rgb <- raster::subset(overview,subset = rgbi)
#rgbs <- raster::subset(rgbnorm[[1]]$normalized,subset = rgbi)

mat <- as.matrix(raster::mean(rgbnorm[[1]]$normalized))
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


cropped <- raster::crop(rgbnorm[[1]]$normalized,raster::extent(rgb,cropVert[1],cropVert[2],cropHor[1],cropHor[2]))


# cropim <- maptools::as.im(cropped)
#
#
# mac <-  quantile(values(cropped),.99)
# mic <- quantile(values(cropped),.01)
# stretch <- (cropped-mic)/(mac-mic)
# stretch[stretch > 1] <- 1
# stretch[stretch < 0] <- 0
# stretch <- (stretch * 254)+1
# plotRGB(stretch)

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
im <- imager::as.cimg(as.mat,x = ncol(cropped),y = nrow(cropped),cc = 3)
hist.eq <- function(im) imager::as.cimg(ecdf(im)(im),dim=dim(im))
#split image into rgb channels
cn <- imager::imsplit(im,"c")
#equalise each channel individually
cn.eq <- imager::map_il(cn,stretchfun)
#recombine and plot
max(cn[[3]])
im2 <- imager::imappend(cn.eq,"c")

pngMed <- apply(ppng,4,median)
pngRat <- pngMed/max(pngMed)
im3 <- im2
for(ca in 1:3){
  im3[,,,ca] <- im3[,,,ca]*pngRat[ca]
}
plot(im3)

plot(im2)

plotRGB(cropped,stretch = "hist")



plotRGB(rgb,stretch = "lin")
