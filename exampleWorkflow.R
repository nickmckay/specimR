#load library
library(specimR)
library(magrittr)
library(dplyr)
library(raster)
library(shiny)
library(readr)
library(tidyr)
library(ggplot2)
library(tibble)

#chose wavelengths of interest
wavelengths <- c(550,570,590,615,630,650,659:671,690,730,790,845,900)
indices <-  c("RABD615","RABD660670","RABD845","R570R630","R590R690")

specimR::spectralWorkflow(wavelengths = wavelengths,indices = c("RABD615","RABD660670","RABD845","R570R630","R590R690"),output.dir = "~/Downloads/testNorm/")
