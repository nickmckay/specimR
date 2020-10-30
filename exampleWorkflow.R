#load library
library(specimR)
library(magrittr)
library(dplyr)
library(raster)
library(shiny)
library(readr)

#specify output path
out.path <- "~/Downloads/testFolder/"

#chose wavelengths of interest
wavelengths <- c(570,590,615,630,660,665,690,730,790,845,900)

#function for normalization
normalized <- normalize(spectra = wavelengths,output.dir = out.path)


#calculate indices
indexTable <- calculateIndices(normalized,indices = c("RABD660","RABD845","R570R630","R590R690"))

#write indices to a csv file
readr::write_csv(indexTable,file.path(out.path,normalized$corename,"spectralIndices.csv"))
