#load library
library(specimR)
library(raster)
library(shiny)
library(tidyverse)

#chose wavelengths of interest
wavelengths <- c(550,570,590,615,630,650,659:671,690,730,790,845,900)
indices <-  c("RABD615","RABD660670","RABD845","R570R630","R590R690")

specimR::spectralWorkflow(directory = '/Users/nicholas/Downloads/Lakes380_FORSY_LC1U_2B_test_2020-06-05_04-05-39',
                          clickDepths = tibble::tibble(position = c("coreLinerTop","coreLinerBottom"),
                                                       pixel = c(666.579310344828,187.241379310345),
                                                       cm = c(1, 3)),
                          cmPerPixel = 0.00417242173112339,
                          wavelengths = c(550, 570, 590, 615, 630, 650, 659, 660, 661, 662, 663, 664, 665, 666, 667, 668, 669, 670, 671, 690, 730, 790, 845, 900),
                          roi = list(raster::extent(matrix(c(506.696926087958,1173.01053419295,-1.40179599355224,866.063090029933),nrow = 2,byrow = T))),
                          output.dir = '~/Downloads/testNorm/',
                          corename = 'Lakes380_FORSY_LC1U_2B_test_2020-06-05_04-05-39')
