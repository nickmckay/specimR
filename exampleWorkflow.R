#load library
library(specimR)
library(magrittr)
library(dplyr)
library(raster)
library(shiny)
#chose wavelengths of interest
wavelengths <- c(570,590,615,630,660,665,690,730,790,845,900)

#function for normalization
normalized <- normalize(spectra = wavelengths)


