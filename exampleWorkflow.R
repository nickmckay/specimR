#load library
library(specimR)
library(raster)
library(shiny)
library(dplyr)
library(tidyverse)

#chose wavelengths of interest
wavelengths <- c(550,570,590,615,630,650,659:671,690,730,790,845,900)
indices <-  c("RABD615","RABD660670","RABD845","R570R630","R590R690")
specimR::spectralWorkflow(directory = '/Working_Core',wavelengths = wavelengths, indices = indices)

specimR::spectralWorkflow(directory = 'C:/Working Core/T97_TANG97_53A_1V_4_A/',
                          wavelengths = c(550, 570, 590, 615, 630, 650, 659, 660, 661, 662, 663, 664, 665, 666, 667, 668, 669, 670, 671, 690, 730, 790, 845, 900),
                          output.dir = 'C:/Working Core/output')
