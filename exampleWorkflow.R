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
source("~/Documents/GitHub/specimR/R/normalize.R")
source("~/Documents/GitHub/specimR/R/spectralCalculations.R")

#chose wavelengths of interest
wavelengths = c(570,590,615,630,660,665,690,730,790,845,900)

#set directory
(directory <- setwd("~/Desktop/test"))

#id of core names
id <- "Lakes380_FORSY_LC1U_2B_test_2020-06-05_04-05-39"

#length of core (cm or mm, important that this is what you subset interactively in the photo window!)
length = 36

#function for normalization
normalized <- normalize(directory=directory,id=id,wavelengths = wavelengths,length = length)

#choose indices
indices <- c("RABD660","RABD845","R570R630","R590R690")

#calculate indices and y-axis
core_output <- CalcIndices(normalized = normalized,indices = indices)

normalize()
gc()
rm(list = ls(all.names = TRUE))
