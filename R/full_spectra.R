# source('~/Documents/GitHub/specimR/R/roi.R')
# source('~/Documents/GitHub/specimR/R/shinyLength.R')
# source('~/Documents/GitHub/specimR/R/normalize.R')
# source('~/Documents/GitHub/specimR/R/coreImage.R')
# library(shiny)
# library(raster)
# library(specimR)
# library(magrittr)
# library(dplyr)
# library(readr)
# library(tidyr)
# library(tibble)

#' Normalize a hyperspectral image
#'
#' @param directory path to Specim core folder
#' @param cmPerPixel optionally specify the number of cm per pixel. Will determine interactively if NA. (default = NA)
#' @param spectra which spectra to normalize
#' @param roi rasterExtent object defining region of interest. NA will allow you to choose interactively (default = NA)
#' @param output.dir optionally save output raster by specifyign the path
#' @param corename optionally choose the length of the core
#'
#' @import raster crayon
#'
#' @return a normalized hyperspectral image
#' @export
full_spectra <- function(directory = NA,
                      length.out = NA,
                      wavelengths = NA,
                      cmPerPixel = NA,
                      roi = NA,#specify roi as raster extent
                      output.dir = NA,
                      corename = NA){


 # directory <- NA
  if(is.na(directory)){
    cat(crayon::bold("Choose a file within the Specim core directory\n"))
    Sys.sleep(1)
  }

  paths <- getPaths(dirPath = directory)
  directory <- dirname(paths$overview)

  dirString <- glue::glue("directory = '{directory}'")


  #output directory handling
#  output.dir <- NA
  if(is.na(output.dir)){
    output.dir <- file.path(dirname(paths$overview),"products")
  }
  outputdirString <- glue::glue("output.dir = '{output.dir}'")


  #folder name
 # corename <- NA
  if(is.na(corename)){
    corename <-  basename(dirname(paths$overview))
  }

  corenameString <- glue::glue("corename = '{corename}'")



  #load overview
  overview <- raster::brick(paths$overview)
#ADDED
#    roi <- NA
    #choose the ROI
  if(!class(roi)=="Extent"){
    roiList <- pick_roi_shiny(overview,nrow(overview)/5)
  }

  #record roi string
roi <- roiList[[1]]
     roiString <- glue::glue("roi = raster::extent(matrix(c({roi@xmin},{roi@xmax},{roi@ymin},{roi@ymax}),nrow = 2,byrow = T))")

  #load in the capture

  filen <- raster::brick(paths$capture)

  orig.ext <- raster::extent(filen)

  #save all band names for later
  allbands <- getBandInfo(filen)

  #find correct wavelengths
  wavelengthsOut <- gsub("X","",names(filen))%>%as.numeric()

    #get length, then subset
  #cmPerPixel <- NA
    if(is.na(cmPerPixel)){
   # roi <- roi[[1]]
    #try cropping the image with the same height, but on the right side to look at the top bottom
    tr_roi <- roi
    tr_roi@xmax <- raster::extent(overview)@xmax
    tr_roi@xmin <- raster::extent(overview)@xmax*.75
    tr_roi@ymin <- tr_roi@ymax - 1200
    tr_roi@ymax <- tr_roi@ymax + 1200
    tr_roi@ymin <- max(c(tr_roi@ymin,orig.ext@ymin))
    tr_roi@ymax <- min(c(tr_roi@ymax,orig.ext@ymax))


    tr.image <- raster::crop(overview,tr_roi)

    br_roi <- tr_roi
    br_roi@ymin <- roi@ymin - 1200
    br_roi@ymax <- roi@ymin + 1200
    br_roi@ymin <- max(c(br_roi@ymin,orig.ext@ymin))
    br_roi@ymax <- min(c(br_roi@ymax,orig.ext@ymax))


    br.image <- raster::crop(overview,br_roi)

    cmPerPixel <- pick_length_shiny(tr.image,br.image,roi)
  }

  cmPerPixelString <- glue::glue("cmPerPixel = {cmPerPixel}")


  #export parameters for rerunning
#  no spectra string right now, fix later
  #normParams <- glue::glue("{dirString},\n{cmPerPixelString},\n{spectraString},\n{roiString},\n{outputdirString},\n{corenameString}")

  #assign to global just incase it fails
 #also fails
# assign("normParams",normParams,envir = .GlobalEnv)

  #crop the image

if(is.finite(cmPerPixel) & cmPerPixel > 0){
    scaleY <- seq(from = cmPerPixel/2,to = (dim(filen)[1]*cmPerPixel)-cmPerPixel/2,by = cmPerPixel)
  }else{
    #calculate length interval of each pixel (necessary for indices calculations)
    scaleY <- coreLength(stripe = roi, length = length)
  }

  #crop_1 <- raster::crop
  #test how slow the crop function is for .5 cm interval (6 times) or jst 3 cm interval

  #chunk into .5 cm bits, normalizate against the white dark lines, average into a single spectra.

chunk.top <- 0
chunk.bot <- 1
length.out <- chunk.bot - chunk.top
chunk.step <- 0.25

pixel.top <- ceiling(.01+chunk.top/cmPerPixel)
pixel.bot <- floor(chunk.bot/cmPerPixel)

#figure out pixel range for depths
#replace this with approx and clickdepths

sub <- raster::extent(roi@xmin,roi@xmax,pixel.top,pixel.bot)


full_spectra <- raster::crop(filen,sub)
new_vals <- raster::aggregate(full_spectra,
                              fact=c(1,ceiling((full_spectra@extent@ymax-full_spectra@extent@ymin)/((length.out/chunk.step)))),
                              FUN=mean)
dim(new_vals)
dim(full_spectra)
#load in the white and dark refs
  whiteRef <- raster::brick(paths$whiteref)
  darkRef <- raster::brick(paths$darkref)
tic()
  white.ref <- processReference(whiteRef,stripe = sub,spectra = names(whiteRef))
  toc()
  tic()
  dark.ref <- processReference(darkRef,stripe = sub,spectra = names(whiteRef))
toc()
  #now normalize
  normalized <- whiteDarkNormalize(stripe = new_vals, white.ref = white.ref, dark.ref = dark.ref)

  if(!dir.exists(file.path(output.dir))){
    dir.create(file.path(output.dir))
  }
  if(!dir.exists(file.path(output.dir,corename))){
    dir.create(file.path(output.dir,corename))
  }

  #average across

  normData <- calculateMeanRows(normalized = normalized)

  #save normalized core image
  normalizedImage <- normalizeCoreImage(paths$overview)
  imager::save.image(normalizedImage,file = file.path(output.dir,"normalizedCoreImage.png"))


  raster::writeRaster(normalized,file.path(output.dir,"normalized.tif"),overwrite = TRUE)
  #save normalized data for future reference
  save(normalized,file = file.path(output.dir,"normalized.RData"))


  #save paths for images too?
  return(list(allbands = allbands,
              #spectra = spectra,
              wavelengths = wavelengthsOut,
              normalized = normalized,
              scaleY = scaleY,
            #  stripe = stripe,
              cmPerPixel = cmPerPixel,
              roi = roi,
              corename = corename,
              pngPath = paths$overview,
             # normParams = normParams,
              outputDir = output.dir))
}

