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

specimR::spectralWorkflow(indices = c("RABD615","RABD660670","RABD845","R570R630","R590R690"),
                          directory = '/Users/nicholas/Downloads/Lakes380_FORSY_LC1U_2B_test_2020-06-05_04-05-39',
                          cmPerPixel = NA,
                          wavelengths = c(550, 570, 590, 615, 630, 650, 659, 660, 661, 662, 663, 664, 665, 666, 667, 668, 669, 670, 671, 690, 730, 790, 845, 900),
                          roi = NA,
                          output.dir = '/Users/nicholas/Downloads/Lakes380_FORSY_LC1U_2B_test_2020-06-05_04-05-39/products',
                          corename = 'Lakes380_FORSY_LC1U_2B_test_2020-06-05_04-05-39',
                          overall.width = 3,
                          individual.width = 2,
                          width.mult = 2)
