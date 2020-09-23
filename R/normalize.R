#load libraries
library(raster)
library(tictoc)
#library(devtools)
#devtools::install_github("timelyportfolio/svgPanZoom")
#devtools::install_github("duncantl/SVGAnnotation")
library(svglite)
library(svgPanZoom)
library(SVGAnnotation)
library(tcltk)
library(progress)

#internal functions
source('~/Desktop/INF_550_Notes/standardize.R')

#go to directory where raw files are saved
(directory <- setwd("/Volumes/easystore/Forsy"))

#choose band_names
spectra <- c("X569.71","X589.88","X615.22","X630.49","X659.89","X665.02","X689.43","X730.74","X790.43","X845.12","X899.86")

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

id <- "Lakes380_FORSY_LC1U_2B_test_2020-06-05_04-05-39"

normalize <- function(id,
                      spectra,
                      data.file = NA,
                      white.ref.file = NA,
                      dark.ref.file = NA){
    Filters <- matrix(c("*",".raw"),1, 2, byrow = TRUE)
data <- file.path(directory,paste(id,".raw",sep=""))
  if(missing(data)) data <- tcltk::tk_choose.files(caption="choose Data File",filter = Filters)
  filen <- raster::brick(data)
white <- file.path(directory,paste("WHITEREF_",id,".raw",sep=""))
  if(missing(white))  white <- tcltk::tk_choose.files(caption="choose 'WHITEREF' File",filter = Filters)
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

WhiteRef <-function(stripe,white){
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

core_name <- normalize(spectra = spectra)
core_name
