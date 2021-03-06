#create an environment for package variables
specimEnv <- new.env()


getPaths <- function(dirPath=NA){
  if(is.na(dirPath)){
    cat(crayon::bold("Choose a file within the Specim core directory\n"))
    dirPath <-  dirname(file.choose())
  }



  #try to find the overview, capture, white and dark ref paths
  paths <- list()

  paths$corename <- coreName <- stringr::str_extract(pattern = "[^/]+$",string  = dirPath)

  #overview
  op <- file.path(dirPath,stringr::str_c(coreName,".png"))

  if(file.exists(op)){
    paths$overview <- op
  }else{
    cat(crayon::bold(crayon::red("Choose the overview .png file\n")))
    paths$overview <-  file.choose()
  }

  #capture
  ca <- file.path(dirPath,"capture",stringr::str_c(coreName,".raw"))

  if(file.exists(ca)){
    paths$capture <- ca
  }else{
    cat(crayon::bold(crayon::red("Choose the capture .raw file\n")))
    paths$capture <-  file.choose()
  }


  #white ref
  wr <- file.path(dirPath,"capture",stringr::str_c("WHITEREF_",coreName,".raw"))

  if(file.exists(wr)){
    paths$whiteref <- wr
  }else{
    cat(crayon::bold(crayon::red("Choose the WHITEREF .raw file\n")))
    paths$whiteref <-  file.choose()
  }

  #dark ref
  dr <- file.path(dirPath,"capture",stringr::str_c("DARKREF_",coreName,".raw"))

  if(file.exists(dr)){
    paths$darkref <- dr
  }else{
    cat(crayon::bold(crayon::red("Choose the DARKREF .raw file\n")))
    paths$darkref <-  file.choose()
  }

  return(paths)

}

filechooseR <- function(id,directory = NA){
  Filters <- matrix(c("*",".raw"),1, 2, byrow = TRUE)
  data <- file.path(directory,paste(id,".raw",sep=""))
  if(missing(data)) data <- tcltk::tk_choose.files(caption="choose Data File",filter = Filters)
  filen <- raster::brick(data)
  return(filen)
}

getBandInfo <- function(filen){
  allbands <- names(filen)
  allbands <- gsub("X","",allbands)%>%as.numeric()
  return(allbands)
}

getNearestWavelengths <- function(filen, spectra){
  bands <- names(filen) %>%
    stringr::str_remove("X") %>%
    as.numeric()

  spec.ind <- c()
  for(i in 1:length(spectra)){
    spec.ind[i] <- which(abs(spectra[i]-bands) == min(abs(spectra[i]-bands)))
  }

  spec.ind <- unique(spec.ind)

  names(spec.ind) <- names(filen)[spec.ind]

  return(spec.ind)
}

spectraR <- function(filen, wavelengths){
  bands <- names(filen)
  bands <- as.numeric(gsub("X","",bands))
  bands <- data.table::data.table(Value=bands)
  bands[,merge:=Value]
  wavelengths <- data.table::data.table(Value =c(wavelengths))
  wavelengths[,merge:=Value]
  data.table::setkeyv(wavelengths,c('merge'))
  data.table::setkeyv(bands,c('merge'))
  spectrum <- bands[wavelengths,roll='nearest']
  spectra <- paste0("X",as.character(spectrum$Value))
  return(spectra)
}







cropImage <- function(raw){
  image <- raster::subset(raw,c(1,5,8))
  raster::plotRGB(raw,c(1,5,8), axes=TRUE, stretch="hist", main="Raw Image")
  cropC <- raster::drawExtent()
  stripe <- raster::crop(raw,cropC)
  names(stripe) <- names(raw)
  return(stripe)
}

coreLength <- function(stripe,length = NA){
  if(is.na(length)){
    length <- readline(prompt = cat(crayon::bold("What is the length of the photographed section in cm?"))) %>% as.numeric()
  }
  len <- stripe@nrows
  ext <- length/len
  scale <- seq(ext/2,length-ext/2,by = ext)
  return(scale)
}

createReferenceMeanRow <- function(ref,e,outFile=NA,spectra){
  if(is.character(ref)){#load in if necessary
    ref <- raster::brick(ref)
  }
  refBrick <-  raster::subset(ref,spectra)
  #crop it by the earlier crop width
  ebb <- raster::extent(refBrick)
  ex <- raster::extent(e)
  ebb@xmin <- ex@xmin
  ebb@xmax <- ex@xmax


  refBrick <- raster::crop(refBrick,ebb)

  rbcm <- raster::colSums(refBrick)/nrow(refBrick)

  #preallocate
  r <- raster::brick(ncol=ncol(refBrick), nrow=1,nl = dim(refBrick)[3], xmn=ex@xmin, xmx=ex@xmax, ymn=0, ymx=1)
  r <- raster::setValues(r,rbcm)

  #save row for later processing.
  if(!is.na(outFile)){
    raster::writeRaster(r,filename = file.path("..",outFile), overwrite = TRUE)
  }else{
    return(r)
  }
}


createReferenceMeanRowLarge <- function(ref,e,outFile=NA,spectra){
  if(is.character(ref)){#load in if necessary
    ref <- raster::brick(ref)
  }

  #aggregate - average into 1 row
  refAgg <- raster::aggregate(ref,
                            fact = c(1,dim(ref)[1]),
                            FUN = mean)

  #subset by spectra
  refBrick <-  raster::subset(refAgg,spectra)

  #crop it by the earlier crop width
  ebb <- raster::extent(refBrick)
  ex <- raster::extent(e)
  ebb@xmin <- ex@xmin
  ebb@xmax <- ex@xmax

  #crop
  r <- raster::crop(refBrick,ebb)

  #save row for later processing.
  if(!is.na(outFile)){
    raster::writeRaster(r,filename = file.path("..",outFile), overwrite = TRUE)
  }else{
    return(r)
  }
}


WhiteRef <-function(whiteRef,stripe,spectra){

  whiteRow <- createReferenceMeanRow(refFile = whiteRef,e = stripe,outFile="WhiteRow.tif",spectra=spectra)
  names(whiteRow) <- names(stripe)
  #find length of core stripe to break up white
  len <- stripe@nrows
  #disaggregate whiterow to length
  white.ref <- disaggregate(whiteRow,fact = c(1,len))
  #set extents to be the same between stripe and reference files
  raster::extent(white.ref) <- raster::extent(stripe)
  return(white.ref)
}

DarkRef <- function(darkRef,stripe,spectra){
  darkRow <- createReferenceMeanRow(refFile=darkRef,e=stripe,outFile="DarkRow.tif",spectra=spectra)
  names(darkRow) <- names(stripe)
  len <- stripe@nrows
  dark.ref <- disaggregate(darkRow,fact = c(1,len))
  raster::extent(dark.ref) <- raster::extent(stripe)
  return(dark.ref)
}

#' Perform reference processing prior to normalizations
#'
#' @param reference a rasterBrick object to process
#' @param stripe a raster whose extent you want to mirror
#' @param spectra an index of spectra to subset
#' @import raster
#'
#' @return a standardized rasterBrick the same size and thickness as stripe
#' @export
processReference <- function(reference,stripe,spectra){
  row <- createReferenceMeanRow(ref = reference,e=stripe,outFile=NA,spectra=spectra)
  names(row) <- spectra
  #len <- extent(stripe)@ymax-extent(stripe)@ymin
  len <- dim(stripe)[1] #make it work after aggregation
  ref <- raster::disaggregate(row,fact = c(1,len))
  raster::extent(ref) <- raster::extent(stripe)
  return(ref)
}


#' Perform reference processing prior to normalizations for large numbers of spectra
#'
#' @param reference a rasterBrick object to process
#' @param stripe a raster whose extent you want to mirror
#' @param spectra an index of spectra to subset
#' @import raster
#'
#' @return a standardized rasterBrick the same size and thickness as stripe
#' @export
processReferenceLarge <- function(reference,stripe,spectra){
  row <- createReferenceMeanRowLarge(ref = reference,e=stripe,outFile=NA,spectra=spectra)
  names(row) <- spectra
  #len <- extent(stripe)@ymax-extent(stripe)@ymin
  len <- dim(stripe)[1] #make it work after aggregation
  ref <- raster::disaggregate(row,fact = c(1,len))
  raster::extent(ref) <- raster::extent(stripe)
  return(ref)
}

normFun <- function(data,white,dark){
  s1 <- (data - dark)
  s1[is.na(s1)] <- 0
  s1[s1<0] <- 0
  return(s1 / (white - dark))
}

whiteDarkNormalize <- function(stripe,white.ref,dark.ref,...){
  normalized <- raster::overlay(stripe,white.ref,dark.ref,fun = normFun,...)
  names(normalized) <- names(stripe)
  return(normalized)
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
normalize <- function(directory = NA,
                      cmPerPixel = NA,
                      wavelengths = c(550,570,590,615,630,650,659:671,690,730,790,845,900),
                      roi = NA,#specify roi as raster extent or list of raster extenst
                      output.dir = NA,
                      corename = NA){


  clickDepths <- try(get("clickDepths",envir = .GlobalEnv),silent = TRUE)
  if(is.data.frame(clickDepths)){
    cmPerPixel <- abs(diff(clickDepths$cm[1:2])/diff(clickDepths$pixel[1:2]))
  }else{
    cmPerPixel <- NA
  }


  if(length(wavelengths) == 3){#probably an image
    is.image <- TRUE
  }else{
    is.image <- FALSE
  }

  spectraString <- glue::glue("wavelengths = c({paste(as.character(wavelengths),collapse = ', ')})")

  #print that you need to pick it.
  if(is.na(directory)){
    cat(crayon::bold("Choose a file within the Specim core directory\n\n"))
    Sys.sleep(1)
  }


  #get the appropriate paths
  paths <- getPaths(dirPath = directory)
  directory <- dirname(paths$overview)

  dirString <- glue::glue("directory = '{directory}'")


  #output directory handling
  if(is.na(output.dir)){
    if(is.image){
      output.dir <- file.path(dirname(paths$overview),"products","photos")
    }else{
      output.dir <- file.path(dirname(paths$overview),"products")
    }
  }
  outputdirString <- glue::glue("output.dir = '{output.dir}'")


  #folder name
  if(is.na(corename)){
    corename <-  basename(dirname(paths$overview))
  }

  corenameString <- glue::glue("corename = '{corename}'")



  #load overview
  overview <- raster::brick(paths$overview)

  if(class(roi) == "Extent"){#make it a list of extents now
    newroi <- list()
    newroi[[1]] <- roi
    roi <- newroi
  }

  #choose the ROI
  if(!is.list(roi)){
    roiList <- pick_roi_shiny(overview,nrow(overview)/5)
  }else{
    roiList <- roi
  }

  nRoi <- length(roiList)


  normOut <- vector(mode = "list",length(nRoi))


  #load in the capture
  filen <- raster::brick(paths$capture)


  orig.ext <- raster::extent(filen)


  #save all band names for later
  allbands <- getBandInfo(filen)

  #find correct wavelengths
  spectra <- getNearestWavelengths(filen = filen, spectra = wavelengths)
  wavelengthsOut <- gsub("X","",names(spectra))%>%as.numeric()
  #subset by wavelengths
  raw <- raster::subset(filen,spectra)

  #grab one roi for this (should probably replace with max and min)
  roi <- roiList[[1]]
  #get length
  if(is.na(cmPerPixel)){

    cmPerPixel <- pick_length_shiny(overview,nrow(overview)/5)

    clickDepths <- try(get("clickDepths",envir = .GlobalEnv),silent = TRUE)


  }

  clickDepthsStr <- glue::glue('clickDepths = tibble::tibble(position = c("coreLinerTop","coreLinerBottom"),
                                   pixel = c({clickDepths$pixel[1]},{clickDepths$pixel[2]}),
                                   cm = c({clickDepths$cm[1]}, {clickDepths$cm[2]}))')

  cmPerPixelString <- glue::glue("cmPerPixel = {cmPerPixel}")

  #export parameters for rerunning
  #record roi string

  rs <- "roi = list("
  for(r in 1:length(roiList)){
    rs <- stringr::str_c(rs,glue::glue("raster::extent(matrix(c({roiList[[r]]@xmin},{roiList[[r]]@xmax},{roiList[[r]]@ymin},{roiList[[r]]@ymax}),nrow = 2,byrow = T))"))
    if(r != length(roiList)){
      rs <- stringr::str_c(rs,",")
    }else{
      rs <- stringr::str_c(rs,")")
    }
  }


  roiString <- rs

  normParams <- glue::glue("{dirString},\n{clickDepthsStr},\n{cmPerPixelString},\n{spectraString},\n{roiString},\n{outputdirString},\n{corenameString}")

  #save normalized core image
  # normalizedImage <- normalizeCoreImage(paths$overview)
  # assign("im",normalizedImage,envir = specimEnv)

  if(!dir.exists(file.path(output.dir))){
    dir.create(file.path(output.dir))
  }


  #assign to global just incase it fails
  if(!is.image){
    assign("normParams",normParams,envir = .GlobalEnv)
  }
  #load in the white and dark refs
  whiteRef <- raster::brick(paths$whiteref)
  darkRef <- raster::brick(paths$darkref)

  #make multiple output directories if multliple ROIs
  if(length(roiList)>1){
    output.dir <- file.path(output.dir,paste0("roi-",seq_along(roiList)))

  }

  if(is.image){
    cat(crayon::bold(glue::glue("Normalizing data for a core image\n\n")))
    cat(crayon::green(glue::glue("This will take a bit more time. Have you got your tea yet?\n\n")))
  }else{
    cat(crayon::bold(glue::glue("Normalizing {nRoi} ROIs...\n\n")))
    cat(crayon::green(glue::glue("This will take a bit. Maybe you should take a break and have some tea...\n\n")))
  }


  for(nroi in 1:nRoi){
    roi <- roiList[[nroi]]

    #crop the image
    stripe <- raster::crop(raw,roi)

    if(is.finite(cmPerPixel) & cmPerPixel > 0){
      scaleY <- seq(from = cmPerPixel/2,to = (nrow(stripe)*cmPerPixel)-cmPerPixel/2,by = cmPerPixel)
    }else{
      #calculate length interval of each pixel (necessary for indices calculations)
      scaleY <- coreLength(stripe = stripe, length = length)
    }

    white.ref <- processReference(whiteRef,stripe = stripe,spectra = spectra)
    dark.ref <- processReference(darkRef,stripe = stripe,spectra = spectra)

    #now normalize
    normalized <- whiteDarkNormalize(stripe = stripe, white.ref = white.ref, dark.ref = dark.ref)

    if(!dir.exists(file.path(output.dir[nroi]))){
      dir.create(file.path(output.dir[nroi]))
    }

    raster::writeRaster(normalized,file.path(output.dir[nroi],"normalized.tif"),overwrite = TRUE)


    clickDepths <- try(get("clickDepths",envir = .GlobalEnv),silent = TRUE)
    if(is.data.frame(clickDepths)){
      #calculate top and bottom depths of ROI
      ymin <- min(clickDepths$pixel)
      ymax <- max(clickDepths$pixel)
      depthTop <- min(clickDepths$cm)
      depthBot <- max(clickDepths$cm)

      roiBotDepth <- Hmisc::approxExtrap(clickDepths$pixel,clickDepths$cm,roi@ymin)$y
      roiTopDepth <- Hmisc::approxExtrap(clickDepths$pixel,clickDepths$cm,roi@ymax)$y

      clickDepths <- clickDepths %>%
        dplyr::bind_rows(tibble::tibble(position = c("roiTop","roiBottom"),
                                  pixel = c(roi@ymax,roi@ymin),
                                  cm = c(roiTopDepth,roiBotDepth)))

      readr::write_csv(clickDepths,file.path(output.dir[nroi],"depthTable.csv"))



    }else{
      roiBotDepth <- NA
      roiTopDepth <- NA
    }

    normalizationOutput <- list(allbands = allbands,
                                spectra = spectra,
                                wavelengths = wavelengthsOut,
                                normalized = normalized,
                                scaleY = scaleY,
                                stripe = stripe,
                                cmPerPixel = cmPerPixel,
                                roi = roi,
                                roiTopDepth = roiTopDepth,
                                roiBotDepth = roiBotDepth,
                                corename = corename,
                                pngPath = paths$overview,
                                normParams = normParams,
                                inputDir = directory,
                                outputDir = output.dir[nroi])

    #save normalized data for future reference
    save(normalizationOutput,file = file.path(output.dir[nroi],"normalized.RData"))
    normOut[[nroi]] <- normalizationOutput


    roiWidth <- round(normOut[[nroi]]$cmPerPixel*(normOut[[nroi]]$roi@xmax-normOut[[nroi]]$roi@xmin),2)

    #write metadata table
    metaOut <- glue::glue("{corename}\n\n") %>%
      stringr::str_c(glue::glue("Processed: {date()}\n\n")) %>%
      stringr::str_c("\n") %>%
      stringr::str_c(glue::glue("ROI top depth: {roiTopDepth} cm\n\n")) %>%
      stringr::str_c(glue::glue("ROI bottom depth: {roiBotDepth} cm\n\n")) %>%
      stringr::str_c(glue::glue("ROI width: {roiWidth} cm\n\n")) %>%
      stringr::str_c(glue::glue("Pixel resolution: {cmPerPixel} cm\n\n")) %>%
      stringr::str_c("\n") %>%
      stringr::str_c(glue::glue("Wavelengths normalized: {paste(wavelengths,collapse = ', ')}\n\n")) %>%
      stringr::str_c("\n") %>%
      stringr::str_c(glue::glue("Output directory: {output.dir[nroi]}"))


    readr::write_lines(metaOut,file.path(output.dir[nroi],"processing_metadata.txt"),)

    cat(crayon::bold(glue::glue("ROI {nroi} of {nRoi} completed...\n\n")))

  }

  #save paths for images too?
  return(normOut)
}

