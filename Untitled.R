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


#choose band_names
spectra <- c("X569.71","X589.88","X615.22","X630.49","X659.89","X665.02","X689.43","X730.74","X790.43","X845.12","X899.86")
id <- "Lakes380_FORSY_LC1U_2B_test_2020-06-05_04-05-39"
(directory <- setwd("/Volumes/easystore/Forsy"))
core <- normalize(directory=directory,id=id,spectra=spectra)

raw <- filechooseR(id=id,directory=directory,spectra=spectra)
stripe <- cropImage(raw)
whiteRef <-WhiteRef(stripe=stripe,directory = directory,id=id)
darkRef <-DarkRef(stripe,directory=directory,id=id)
normalized <- overlayR(stripe,whiteRef,darkRef)

