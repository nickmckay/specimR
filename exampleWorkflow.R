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
                 directory = '/Users/npm4/Downloads/Lakes380_DOUGL_LC2U_1B_2020-11-12_22-15-52',

                 cmPerPixel = 0.00413962241393703,

                 wavelengths = c(550, 570, 590, 615, 630, 650, 659, 660, 661, 662, 663, 664, 665, 666, 667, 668, 669, 670, 671, 690, 730, 790, 845, 900),

                 roi = raster::extent(matrix(c(921.444118123633,1121.44411812363,341.453536417403,22048.059534175),nrow = 2,byrow = T)),

                 output.dir = '/Users/npm4/Downloads/Lakes380_DOUGL_LC2U_1B_2020-11-12_22-15-52/products',

                 corename = 'Lakes380_DOUGL_LC2U_1B_2020-11-12_22-15-52')
