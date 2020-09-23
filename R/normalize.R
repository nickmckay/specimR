
#function
#' Title
#'
#' @param spectra
#' @param data.file
#' @param white.ref.file
#' @param dark.ref.file
#'
#' @return
#' @export
#'
#' @examples



filechooseR <- function(id,
                        directory = NA,
                      spectra,
                      data.file = NA,
                      white.ref.file = NA,
                      dark.ref.file = NA){
    Filters <- matrix(c("*",".raw"),1, 2, byrow = TRUE)
data <- file.path(directory,paste(id,".raw",sep=""))
  if(missing(data)) data <- tcltk::tk_choose.files(caption="choose Data File",filter = Filters)
  filen <- raster::brick(data)

dark <- file.path(directory,paste("DARKREF_",id,".raw",sep=""))
  if(missing(dark))  dark <-tcltk::tk_choose.files(caption="choose 'DARKREF' File",filter = Filters)

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
    whiteRow <- createReferenceMeanRow(white,stripe,"WhiteRow.tif",spectra)
  names(whiteRow) <- names(stripe)
  #find length of core stripe to break up white
  len <- stripe@nrows
  #disaggregate whiterow to length
  whiteRef <- disaggregate(whiteRow,fact = c(1,len))
  #set extents to be the same between stripe and reference files
  extent(whiteRef) <- extent(stripe)
  return(whiteRef)
}

DarkRef <- function(stripe,dark){
  darkRow <- createReferenceMeanRow(dark,stripe,"DarkRow.tif",spectra)
  names(darkRow) <- names(stripe)
  len <- stripe@nrows
  darkRef <- disaggregate(darkRow,fact = c(1,len))
  extent(darkRef) <- extent(stripe)
return(darkRef)
}

overlayR <- function(stripe,whiteRef,darkRef){
  normalized <- raster::overlay(stripe,whiteRef,darkRef,fun = normFun,filename = "normalized.tif",overwrite = TRUE)
  names(normalized) <- names(stripe)
  return(normalized)
}


normalize <- function(id,spectra,directory,tif.path.to.write = NA){
  #choose and load the file
  raw <- filechooseR(id = id,
                     spectra = spectra,
                     directory = directory)

  #crop the image
  stripe <- cropImage(raw)

  #load in the white and dark refs
  white <- whiteRef(stripe,id,directory)
  dark <- darkRef(stripe,id,directory)

  #now normalize
  normalized <- overlayR(stripe, white, dark)

  #optionally write to a tif
  if(!is.na(tif.path.to.write)){
    writeTif(normalized, path = tif.path.to.write)
  }

  return(normalized)
}
