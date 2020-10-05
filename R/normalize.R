

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
#'

#'
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

subsetR <- function(filen, spectra){
raw <- raster::subset(filen,spectra)
return(raw)
}

cropImage <- function(raw){
  image <- raster::subset(raw,c(1,5,8))
  raster::plotRGB(raw,c(1,5,8), axes=TRUE, stretch="hist", main="Raw Image")
  cropC <- raster::drawExtent()
  stripe <- raster::crop(raw,cropC)
  names(stripe) <- names(raw)
return(stripe)
}

coreLength <- function(stripe,length){
  len <- stripe@nrows
  ext <- length/len
  scale <- seq(0,length,by = ext)
  scaleY <- scale[-1]
  return(scaleY)
}

createReferenceMeanRow <- function(refFile,e,outFile,spectra){

  refBrick <- raster::brick(refFile)
  refBrick <-  raster::subset(refBrick,spectra)
  #crop it by the earlier crop width
  ebb <- raster::extent(refBrick)
  ex <- raster::extent(e)
  ebb@xmin <- ex@xmin
  ebb@xmax <- ex@xmax


  refBrick <- raster::crop(refBrick,ebb)

  rbcm <- colSums(refBrick)/nrow(refBrick)

  #preallocate
  r <- raster::brick(ncol=ncol(refBrick), nrow=1,nl = dim(refBrick)[3], xmn=ex@xmin, xmx=ex@xmax, ymn=0, ymx=1)
  r <- setValues(r,rbcm)

  #save row for later processing.
  raster::writeRaster(r,filename = file.path("..",outFile), overwrite = TRUE)

}


WhiteRef <-function(stripe,directory,id,spectra){
  white <- file.path(directory,paste("WHITEREF_",id,".raw",sep=""))
  if(missing(white))  white <- tcltk::tk_choose.files(caption="choose 'WHITEREF' File",filter = Filters)
    whiteRow <- createReferenceMeanRow(refFile = white,e = stripe,outFile="WhiteRow.tif",spectra=spectra)
  names(whiteRow) <- names(stripe)
  #find length of core stripe to break up white
  len <- stripe@nrows
  #disaggregate whiterow to length
  white.ref <- disaggregate(whiteRow,fact = c(1,len))
  #set extents to be the same between stripe and reference files
  extent(white.ref) <- extent(stripe)
  return(white.ref)
}

DarkRef <- function(stripe,directory,id,spectra){
  dark <- file.path(directory,paste("DARKREF_",id,".raw",sep=""))
  if(missing(dark))  dark <- tcltk::tk_choose.files(caption="choose 'DARKREF' File",filter = Filters)
  darkRow <- createReferenceMeanRow(refFile=dark,e=stripe,outFile="DarkRow.tif",spectra=spectra)
  names(darkRow) <- names(stripe)
  len <- stripe@nrows
  dark.ref <- disaggregate(darkRow,fact = c(1,len))
  extent(dark.ref) <- extent(stripe)
return(dark.ref)
}


normFun <- function(data,white,dark){
  s1 <- (data - dark)
  s1[is.na(s1)] <- 0
  s1[s1<0] <- 0
  return(s1 / (white - dark))
}

overlayR <- function(stripe,white.ref,dark.ref){
  normalized <- raster::overlay(stripe,white.ref,dark.ref,fun = normFun,filename = "normalized.tif",overwrite = TRUE)
  names(normalized) <- names(stripe)
  return(normalized)
}

#inputs to function are core name, wavelengths of interest, directory for core data location, and visual length of core (that you are subsetting!)
normalize <- function(id,wavelengths,directory,length,tif.path.to.write = NA){
  #choose and load the file
  filen <- filechooseR(id = id,
                     directory = directory)

  #save all band names for later
  allbands <- getBandInfo(filen)

  #find correct wavelengths
  spectra <- spectraR(filen = filen, wavelengths = wavelengths)

  #subset by wavelengths

  raw <- subsetR(filen = filen, spectra = spectra)

  #crop the image
  stripe <- cropImage(raw = raw)

  #calculate length interval of each pixel (necessary for indices calculations)
  scaleY <- coreLength(stripe = stripe, length = length)

  #load in the white and dark refs
  white.ref <- WhiteRef(stripe = stripe,id = id,directory = directory,spectra = spectra)
  dark.ref <- DarkRef(stripe = stripe,id = id,directory = directory,spectra = spectra)

  #now normalize
  normalized <- overlayR(stripe = stripe, white.ref = white.ref, dark.ref = dark.ref)

#  list(filen,normalized)
  #optionally write to a tif
  if(!is.na(tif.path.to.write)){
    writeTif(normalized[[3]], path = tif.path.to.write)
  }
  list(allbands,spectra,normalized,scaleY)
}

