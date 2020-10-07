#create an environment for package variables
specimEnv <- new.env()


getPaths <- function(dirPath=NA){
  if(is.na(dirPath)){
    cat(crayon::bold("Choose a file within the Specim core directory\n"))
    dirPath <-  dirname(file.choose())
  }

  coreName <- stringr::str_extract(pattern = "[^/]+$",string  = dirPath)

  #try to find the overview, capture, white and dark ref paths
  paths <- list()


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

  spec.ind <- sort(unique(spec.ind))

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
  names(row) <- names(stripe)
  len <- stripe@nrows
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
#' @param wavelengths a vector of wavelengths to extract from the hyperspectral image (cho)
#' @param data.file optionally specify the path to the hyperspectral .raw image
#' @param white.ref.file
#' @param dark.ref.file
#' @param another.param
#' @import raster tcltk
#'
#' @return a normalized hyperspectral image
#' @export
#inputs to function are core name, wavelengths of interest, directory for core data location, and visual length of core (that you are subsetting!)
normalize <- function(directory = NA,
                      length = NA,
                      spectra = NA,
                      roi = NA,#specify roi as raster extent
                      tif.path.to.write = NA){
  #get the appropriate paths
  paths <- getPaths(dirPath = directory)

  #load overview
  overview <- raster::brick(paths$overview)

  #choose the ROI
  if(is.na(roi)){
  roi <- pick_roi_shiny(overview)
}
  #load in the capture
  filen <- raster::brick(paths$capture)

  #save all band names for later
  allbands <- getBandInfo(filen)

  #find correct wavelengths
  spectra <- getNearestWavelengths(filen = filen, spectra = spectra)

  #subset by wavelengths
  raw <- raster::subset(filen,spectra)

  #crop the image
  stripe <- raster::crop(raw,roi)

  #try cropping the image with the same height, but on the right side to look at the top bottom
  tr_roi <- roi
  tr_roi@xmax <- raster::extent(overview)@xmax
  tr_roi@xmin <- raster::extent(overview)@xmax*.75
  tr_roi@ymin <- tr_roi@ymax - (tr_roi@xmax-tr_roi@xmin)
  tr.image <- raster::crop(overview,tr_roi)

  br_roi <- tr_roi
  br_roi@ymin <- raster::extent(overview)@ymin
  br_roi@ymax <- br_roi@ymin + (br_roi@xmax-br_roi@xmin)
  br.image <- raster::crop(overview,br_roi)

  cmPerPixel <- pick_length_shiny(tr.image,br.image,roi)

  if(is.finite(cmPerPixel) * cmPerPixel > 0){
    scaleY <- seq(cmPerPixel/2,(nrow(stripe)*cmPerPixel)-cmPerPixel/2,by = cmPerPixel)
  }else{
  #calculate length interval of each pixel (necessary for indices calculations)
    scaleY <- coreLength(stripe = stripe, length = length)
  }

  #load in the white and dark refs
  whiteRef <- raster::brick(paths$whiteref)
  darkRef <- raster::brick(paths$darkref)

  white.ref <- processReference(whiteRef,stripe = stripe,spectra = spectra)
  dark.ref <- processReference(darkRef,stripe = stripe,spectra = spectra)

  #now normalize
  normalized <- whiteDarkNormalize(stripe = stripe, white.ref = white.ref, dark.ref = dark.ref)

#  list(filen,normalized)
  #optionally write to a tif
  if(!is.na(tif.path.to.write)){
    writeTif(normalized, path = tif.path.to.write)
  }

  return(list(allbands = allbands,spectra = spectra,normalized = normalized,scaleY = scaleY,raw=raw))
}

