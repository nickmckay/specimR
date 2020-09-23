

#' Normalize a hyperspectral image
#'
#' @param spectra a vector of wavelengths to extract from the hyperspectral image
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
filechooseR <- function(id,
                        directory = NA,
                      spectra){
    Filters <- matrix(c("*",".raw"),1, 2, byrow = TRUE)
data <- file.path(directory,paste(id,".raw",sep=""))
  if(missing(data)) data <- tcltk::tk_choose.files(caption="choose Data File",filter = Filters)
  filen <- raster::brick(data)
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

WhiteRef <-function(stripe,directory,id){
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

DarkRef <- function(stripe,directory,id){
  dark <- file.path(directory,paste("DARKREF_",id,".raw",sep=""))
  if(missing(dark))  dark <- tcltk::tk_choose.files(caption="choose 'DARKREF' File",filter = Filters)
  darkRow <- createReferenceMeanRow(refFile=dark,e=stripe,outFile="DarkRow.tif",spectra=spectra)
  names(darkRow) <- names(stripe)
  len <- stripe@nrows
  dark.ref <- disaggregate(darkRow,fact = c(1,len))
  extent(dark.ref) <- extent(stripe)
return(dark.ref)
}

overlayR <- function(stripe,white.ref,dark.ref){
  normalized <- raster::overlay(stripe,white.ref,dark.ref,fun = normFun,filename = "normalized.tif",overwrite = TRUE)
  names(normalized) <- names(stripe)
  return(normalized)
}


normalize <- function(id,spectra,directory,tif.path.to.write = NA){
  #choose and load the file
  raw <- filechooseR(id = id,
                     spectra = spectra,
                     directory = directory)

  #crop the image
  stripe <- cropImage(raw = raw)

  #load in the white and dark refs
  white.ref <- WhiteRef(stripe = stripe,id = id,directory = directory)
  dark.ref <- DarkRef(stripe = stripe,id = id,directory = directory)

  #now normalize
  normalized <- overlayR(stripe = stripe, white.ref = white.ref, dark.ref = dark.ref)

  #optionally write to a tif
  if(!is.na(tif.path.to.write)){
    writeTif(normalized, path = tif.path.to.write)
  }

  return(normalized)
}
