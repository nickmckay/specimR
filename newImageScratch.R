#experiment with images

library(raster)
library(magrittr)
library(shiny)


# auto detection ---------------------------------------------------------


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

# normalization -----------------------------------------------------------




filen <- "/Volumes/Lakes380 WTC 1/SCANNED CORES/Lake Kaurapataka/Lakes380_KAURA_LC1U_1A_S1_2020-06-03_21-47-37/capture/Lakes380_KAURA_LC1U_1A_S1_2020-06-03_21-47-37.raw"

#filen <- "/Users/nicholas/Downloads/Lakes380_FORSY_LC1U_2B_test_2020-06-05_04-05-39/capture/DARKREF_Lakes380_FORSY_LC1U_2B_test_2020-06-05_04-05-39.raw"

overview <- raster::brick(filen)

rgbi <- specimR:::getNearestWavelengths(spectra = c(630,532,465),filen = overview)

#get pre-normalized median levels?


rgbnorm <- normalize(
                     wavelengths =  c(630,532,465),
                     output.dir = "/Users/nicholas/Downloads/testNorm")


rgborigorig <- raster::subset(overview,  rgbi)

origcrop <- raster::crop(rgborigorig,rgbnorm$roi)


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



origmat <- c(as.matrix(origcrop))
mso <- stretchfun(origmat)
imo <- imager::as.cimg(mso,x = ncol(rgbnorm$normalized),y = nrow(rgbnorm$normalized),cc = 3)
imager::save.image(im1,"~/Downloads/testNorm/orig.png")

im02 <- imager::as.cimg(origmat,x = ncol(rgbnorm$normalized),y = nrow(rgbnorm$normalized),cc = 3)

cno <- imager::imsplit(im02,"c")
#equalise each channel individually
cn.eqo <- imager::map_il(cno,stretchfun)
#recombine and plot
imo2 <- imager::imappend(cn.eqo,"c")
imager::save.image(imo2,"~/Downloads/testNorm/origStretch.png")

plot(im2)

imager::save.image(im2,file = rgbnorm$)

as.mat <- c(as.matrix(rgbnorm$normalized))
ms <- stretchfun(as.mat)
im1 <-  imager::as.cimg(ms,x = ncol(rgbnorm$normalized),y = nrow(rgbnorm$normalized),cc = 3)

imager::save.image(im1,"~/Downloads/testNorm/fullStretch.png")

im <- imager::as.cimg(as.mat,x = ncol(rgbnorm$normalized),y = nrow(rgbnorm$normalized),cc = 3)
hist.eq <- function(im) imager::as.cimg(ecdf(im)(im),dim=dim(im))
#split image into rgb channels
cn <- imager::imsplit(im,"c")

cnRGB <- cn[c(3,2,1)]
#equalise each channel individually
cn.eq <- imager::map_il(cnRGB,stretchfun)
#recombine and plot
im2 <- imager::imappend(cn.eq,"c")

plot(im2)

imager::save.image(im2,file = "~/Downloads/testNorm/channelStretchrgb.png")


im3 <- im2
for(ca in 1:3){
  im3[,,,ca] <- im3[,,,ca]*mednormrat[ca]
}
plot(im3)

im4 <- im2
for(ca in 1:3){
  im4[,,,ca] <- im4[,,,ca]*medorigrat[ca]
}
plot(im4)


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


linearStretch <- function(lay,na.rm = TRUE,trunc.perc = 0.02){
  mac <-  quantile((lay),1-trunc.perc,na.rm = na.rm)
  mic <- quantile((lay),trunc.perc,na.rm = na.rm)

  stretch <- (lay-mic)/(mac-mic)
  stretch[stretch > 1] <- 1
  stretch[stretch < 0] <- 0
  stretch <- (stretch * 254)+1
  return(stretch)
}

histogramStretch <- function(im) imager::as.cimg(ecdf(im)(im),dim=dim(im))

#Create image from reflectance standardized data

createImage <- function(normalizedOutput, bigRoi = NA, directory = NA, wavelengths = c(630,532,465)){

  #print that you need to pick it.
  if(is.na(directory)){
    cat(crayon::bold("Choose a file within the Specim core directory\n"))
    Sys.sleep(1)
  }


  #get the appropriate paths
  paths <- getPaths(dirPath = directory)
  directory <- dirname(paths$overview)

  overview <- raster::brick(paths$overview)

  overviewPng <- imager::load.image(paths$overview)

  gs <- imager::grayscale(overviewPng) %>% as.matrix()

  across <- apply(gs,1,mean)
  down <- apply(gs,2,mean)

  cropVert <- findCropEdges(down)
  cropHor <- findCropEdges(across)

  bigRoiTry <- raster::extent(rgb,cropVert[1],cropVert[2],cropHor[1],cropHor[2])

  #check to see if the big ROI is good (new shiny app)


  # now crop out the mud.

  normRGB <- normalize(directory = directory,
            cmPerPixel = 0.004,
            wavelengths = wavelengths,
            roi = bigRoi,
            output.dir = tempdir(),
            corename = paths$corename
            )

  #make sure it's in RGB order?

  #rescale the image
  rescaledROI <- normRGB$normalized %>%
    as.matrix() %>%
    c() %>%
    imager::as.cimg(x = ncol(normRGB$normalized),y = nrow(normRGB$normalized),cc = 3) %>%
    imager::imsplit("c") %>%
    imager::map_il(linearStretch) %>%
    imager::imappend("c")

  #save the mud only image.


  # rescale the outer parts of the png
  rescaledOverview <- imager::imsplit(overviewPng,"c") %>%
    imager::map_il(linearStretch) %>%
    imager::imappend("c")

  imager::save.image(rescaledOverview,"~/Downloads/testNorm/rescaledOverviewTest.png")

  #plop the new rescaled mud back in.

  mudAndScale <- rescaledOverview

  mudAndScale[bigRoi@xmin:bigRoi@xmax,bigRoi@ymin:bigRoi@ymax, , ] <- NA


  imager::save.image(mudAndScale,"~/Downloads/testNorm/rescaledMudPlop.png")


}















