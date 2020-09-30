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
#choose bands of interest



spectra <- c("X569.71","X589.88","X615.22","X630.49","X659.89","X665.02","X689.43","X730.74","X790.43","X845.12","X899.86")


spectra <- c("X569.71","X589.88","X615.22","X630.49","X659.89","X665.02","X689.43","X730.74","X790.43","X845.12","X899.86")

wavelengths = c(570,590,615,630,660,665,690,730,790,845,900)

(directory <- setwd("~/Desktop/test"))
id <- "Lakes380_FORSY_LC1U_2B_test_2020-06-05_04-05-39"

directory
id
core <- normalize(directory=directory,id=id,wavelengths = wavelengths)

filen <- filechooseR(id=id,directory=directory)
filen <- brick("Lakes380_FORSY_LC1U_2B_test_2020-06-05_04-05-39.raw")


spectra

subset(filen,nlayers(filen))

stripe <- cropImage(raw)
whiteRef <-WhiteRef(stripe=stripe,directory = directory,id=id)
darkRef <-DarkRef(stripe,directory=directory,id=id)
normalized <- overlayR(stripe,whiteRef,darkRef)

