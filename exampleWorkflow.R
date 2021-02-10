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

spectralWorkflow(indices = indices,
                 directory = '/Users/npm4/Downloads/Lakes380_DOUGL_LC2U_2B_2020-11-12_21-55-33',
                 cmPerPixel = 0.00413770317379107,
                 wavelengths = c(550, 570, 590, 615, 630, 650, 659, 660, 661, 662, 663, 664, 665, 666, 667, 668, 669, 670, 671, 690, 730, 790, 845, 900),
                 roi = raster::extent(matrix(c(910,1274,351.112290367009,12154.1667392251),nrow = 2,byrow = T)),
                 output.dir = '/Users/npm4/Downloads/Lakes380_DOUGL_LC2U_2B_2020-11-12_21-55-33/products',
                 corename = 'Lakes380_DOUGL_LC2U_2B_2020-11-12_21-55-33')
