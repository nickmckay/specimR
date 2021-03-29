
# auto detection ---------------------------------------------------------

#' Title
#'
#' @param line
#' @param basereg
#' @param cutthresh
#' @param thresh
#'
#' @return
#' @export
#'
#' @examples
findCropEdges <- function(line,basereg =  c(0.25,0.75),cutthresh = .3, thresh = 3){

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


#' Title
#'
#' @param lay
#' @param na.rm
#' @param trunc.perc
#'
#' @return
#' @export
#'
#' @examples
linearStretch <- function(lay,na.rm = TRUE,trunc.perc = 0.02){
  mac <-  quantile((lay),1-trunc.perc,na.rm = na.rm)
  mic <- quantile((lay),trunc.perc,na.rm = na.rm)

  stretch <- (lay-mic)/(mac-mic)
  stretch[stretch > 1] <- 1
  stretch[stretch < 0] <- 0
  stretch <- (stretch * 254)+1
  return(stretch)
}

#' Title
#'
#' @param im
#'
#' @return
#' @export
#'
#' @examples
histogramStretch <- function(im){
  win <- which(!is.na(im))
  st <- ecdf(im[win])(im[win])
  im[win] <- st
  return(imager::as.cimg(im))
}

#' Create image from reflectance standardized data
#'
#' @param bigRoi
#' @param directory
#' @param wavelengths
#' @param image.output.dir
#' @param stretch.method
#' @param stretch.fun
#'
#' @return
#' @export
#'
#' @examples
createImages <- function(bigRoi = NA, directory = NA, wavelengths = c(630,532,465),image.output.dir = NA,stretch.method = "full",stretch.fun = "linear2pct"){

  #pick stretch function
  if(stretch.fun == "linear2pct"){
    stretchfun = linearStretch
  }else if(stretch.fun == "histogram"){
    stretchfun = histogramStretch
  }else{
    stop("stretchfun not recognized. Valid options are 'linear2pct' or 'histogram'")
  }



  #print that you need to pick it.
  if(is.na(directory)){
    cat(crayon::bold("Choose a file within the Specim core directory\n"))
    Sys.sleep(1)
  }


  #get the appropriate paths
  paths <- getPaths(dirPath = directory)

  #output info

  #output directory handling
  if(is.na(image.output.dir)){
    image.output.dir <- file.path(dirname(paths$overview),"products","photos")
  }

  directory <- dirname(paths$overview)

  overview <- raster::brick(paths$overview)

  overviewPng <- imager::load.image(paths$overview)

  if(is.na(bigRoi)){
    gs <- imager::grayscale(overviewPng) %>% as.matrix()

    across <- apply(gs,1,mean)
    down <- apply(gs,2,mean)

    cropVert <- findCropEdges(rev(down))
    cropHor <- findCropEdges(across)

    bigRoiTry <- raster::extent(cropHor[1],cropHor[2],cropVert[1],cropVert[2])

    #check to see if the big ROI is good (new shiny app)
    bigRoi <- pick_big_roi_shiny(overview,bigRoiTry, zh = nrow(overview)/5)

    bigRoi@xmin <- ceiling(bigRoi@xmin)
    bigRoi@ymin <- ceiling(bigRoi@ymin)
    bigRoi@xmax <- floor(bigRoi@xmax)
    bigRoi@ymax <- floor(bigRoi@ymax)
  }

  bigRoiStr <<- glue::glue("raster::extent(matrix(c({bigRoi@xmin},{bigRoi@xmax},{bigRoi@ymin},{bigRoi@ymax}),nrow = 2,byrow = T))")


  if(!dir.exists(image.output.dir)){
    dir.create(image.output.dir)
  }
  save(bigRoi,file = file.path(image.output.dir,"bigRoi.Rdata"))

  # now crop out the mud.

  normRGB <- normalize(directory = directory,
                       cmPerPixel = 0.004,
                       wavelengths = wavelengths,
                       roi = bigRoi,
                       output.dir = image.output.dir,
                       corename = paths$corename
  )


  if(stretch.method == "full"){#apply stretch to all channels together
    #rescale the image
    rescaledROI <- normRGB[[1]]$normalized %>%
      as.matrix() %>%
      stretchfun() %>%
      c() %>%
      imager::as.cimg(x = ncol(normRGB[[1]]$normalized),y = nrow(normRGB[[1]]$normalized),cc = 3) %>%
      imager::mirror("y")


  }else if(stretch.method == "channel"){#apply stretch channel by channel
    #rescale the image
    rescaledROI <- normRGB[[1]]$normalized %>%
      as.matrix() %>%
      c() %>%
      imager::as.cimg(x = ncol(normRGB[[1]]$normalized),y = nrow(normRGB[[1]]$normalized),cc = 3) %>%
      imager::imsplit("c") %>%
      imager::map_il(stretchfun) %>%
      imager::imappend("c") %>%
      imager::mirror("y")


  }

  #save the mud only image.
  if(!dir.exists(image.output.dir)){
    dir.create(image.output.dir)
  }

  imager::save.image(imager::mirror(rescaledROI,"y"),file = file.path(image.output.dir,paste0("coreOnly-",stretch.fun,".png")))



  # rescale the outer parts of the png
  rescaledOverview <- imager::mirror(overviewPng,"y")
  rescaledOverview[(bigRoi@xmin+1):bigRoi@xmax,(bigRoi@ymin+1):bigRoi@ymax, , ] <- NA
  rescaledOverview <-  imager::imsplit(rescaledOverview,"c") %>%
    imager::map_il(linearStretch) %>%
    imager::imappend("c")


  #plop the new rescaled mud back in.

  mudAndScale <- rescaledOverview

  mudAndScale[(bigRoi@xmin+1):bigRoi@xmax,(bigRoi@ymin+1):bigRoi@ymax, , ] <- rescaledROI

  #flip back
  mudAndScale <- imager::mirror(mudAndScale,"y")


  imager::save.image(mudAndScale,file = file.path(image.output.dir,paste0("fullImage-",stretch.fun,".png")))



}















