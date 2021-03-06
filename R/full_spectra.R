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
fullSpectra <- function(directory = NA,
                        length.out = NA,
                        wavelengths = NA,
                        cmPerPixel = NA,
                        roi = NA,#specify roi as raster extent
                        output.dir = NA,
                        corename = NA,
                        chunk.top = 0,
                        chunk.bot = 3,
                        chunk.step = .25){






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

  if(class(roi) == "list"){
    roi <- roi[[1]]
  }

  if(!class(roi)=="Extent"){
    roiList <- pick_roi_shiny(overview,nrow(overview)/5)

    #record roi string
    roi <- roiList[[1]]
  }
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
    cmPerPixel <- pick_length_shiny(overview,nrow(overview)/5)

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

  if(is.na(chunk.bot)){
    chunk.bot <- floor(max(scaleY))
  }


  pixel.top <- ceiling(.01+chunk.top/cmPerPixel)
  pixel.bot <- floor(chunk.bot/cmPerPixel)

  #figure out pixel range for depths
  #replace this with approx and clickdepths

  sub <- raster::extent(roi@xmin,roi@xmax,pixel.top,pixel.bot)

  length.out <- chunk.bot - chunk.top
  mid.points <- seq(chunk.top + chunk.step/2,chunk.bot - chunk.step/2, by = chunk.step)

  full.spectra <- raster::crop(filen,sub)
  new.vals <- raster::aggregate(full.spectra,
                                fact=c(1,ceiling((full.spectra@extent@ymax-full.spectra@extent@ymin)/((length.out/chunk.step)))),
                                FUN=mean)

  #load in the white and dark refs and pre-aggregate
  whiteRef <- raster::brick(paths$whiteref) #load

  white.ref <- processReferenceLarge(whiteRef,stripe = new.vals,spectra = names(whiteRef))


  darkRef <- raster::brick(paths$darkref)

  dark.ref <- processReferenceLarge(darkRef,stripe = new.vals,spectra = names(darkRef))

  #now normalize
  normalized <- whiteDarkNormalize(stripe = new.vals, white.ref = white.ref, dark.ref = dark.ref)

  if(!dir.exists(file.path(output.dir))){
    dir.create(file.path(output.dir))
  }
  if(!dir.exists(file.path(output.dir,corename))){
    dir.create(file.path(output.dir,corename))
  }

  #average across

  normData <- MeanRowSpectra(normalized = normalized)

  spectraOut <- tibble::tibble(depth.mid = mid.points)

  ndt <- as_tibble(normData)
  names(ndt) <- wavelengthsOut
  #create spectral output spreadsheet

  spectraOut <- bind_cols(spectraOut,ndt)
  readr::write_csv(spectraOut,file.path(output.dir,"fullSpectraChunks.csv"))

  raster::writeRaster(normalized,file.path(output.dir,"normalizedChunks.tif"),overwrite = TRUE)
  #save normalized data for future reference
  save(normalized,file = file.path(output.dir,"normalized.RData"))


  #save paths for images too?
  return(spectraOut)
}


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
fullSpectraWholeROI <- function(directory = NA,
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


  #output directory handling
  #  output.dir <- NA
  if(is.na(output.dir)){
    output.dir <- file.path(dirname(paths$overview),"products")
  }


  #folder name
  # corename <- NA
  if(is.na(corename)){
    corename <-  basename(dirname(paths$overview))
  }



  #load overview
  overview <- raster::brick(paths$overview)
  #ADDED
  #    roi <- NA
  #choose the ROI

  if(!class(roi) == "list"){

    if(class(roi) == "Extent"){
      roiList <- list(roi)
    }else{
      roiList <- pick_roi_shiny(overview,nrow(overview)/5)
    }

  }else{
    roiList <- roi
  }

  #make multiple output directories if multliple ROIs
  if(length(roiList)>1){
    output.dir <- file.path(output.dir,paste0("fullspectra-roi-",seq_along(roiList)))

  }

  nRoi <- length(roiList)
  cat(crayon::bold(glue::glue("Normalizing {nRoi} ROIs...\n\n")))



  #load in the capture

  filen <- raster::brick(paths$capture)

  orig.ext <- raster::extent(filen)

  #save all band names for later
  allbands <- getBandInfo(filen)

  #find correct wavelengths
  wavelengthsOut <- gsub("X","",names(filen))%>%as.numeric()




  #export parameters for rerunning
  #  no spectra string right now, fix later
  #normParams <- glue::glue("{dirString},\n{cmPerPixelString},\n{spectraString},\n{roiString},\n{outputdirString},\n{corenameString}")

  #assign to global just incase it fails
  #also fails
  # assign("normParams",normParams,envir = .GlobalEnv)

  #crop the image

  for(nroi in 1:length(roiList)){
    roi <- roiList[[nroi]]

    full.spectra <- raster::crop(filen,roi)
    new.vals <- raster::aggregate(full.spectra,
                                  fact=c(1,ceiling((full.spectra@extent@ymax-full.spectra@extent@ymin))),
                                  FUN=mean)

    #load in the white and dark refs and pre-aggregate
    whiteRef <- raster::brick(paths$whiteref) #load

    white.ref <- processReferenceLarge(whiteRef,stripe = new.vals,spectra = names(whiteRef))


    darkRef <- raster::brick(paths$darkref)

    dark.ref <- processReferenceLarge(darkRef,stripe = new.vals,spectra = names(darkRef))

    #now normalize
    normalized <- whiteDarkNormalize(stripe = new.vals, white.ref = white.ref, dark.ref = dark.ref)

    if(!dir.exists(file.path(output.dir[nroi]))){
      dir.create(file.path(output.dir[nroi]))
    }

    #average across

    normData <- MeanRowSpectra(normalized = normalized)


    ndt <- as_tibble(normData)
    names(ndt) <- wavelengthsOut
    #create spectral output spreadsheet

    spectraOut <- ndt
    readr::write_csv(spectraOut,file.path(output.dir[nroi],"fullSpectraChunks.csv"))

    raster::writeRaster(normalized,file.path(output.dir[nroi],"normalizedChunks.tif"),overwrite = TRUE)
    #save normalized data for future reference
    save(normalized,file = file.path(output.dir[nroi],"normalized.RData"))

  }
}
