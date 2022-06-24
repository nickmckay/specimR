#load library
library(specimR)
library(raster)
library(shiny)
library(dplyr)
library(tidyverse)

#chose wavelengths of interest
wavelengths <- c(550,570,590,615,630,650,659:671,690,730,790,845,900)
indices <-  c("RABD615","RABD660670","RABD845","R570R630","R590R690")
specimR::spectralWorkflow(directory = '/Volumes/data/Downloads/Lakes380_TEN6m1_May21_3-4,5m_A_2022-06-09_23-54-44',
                          clickDepths = tibble::tibble(position = c("coreLinerTop","coreLinerBottom"),
                                                       pixel = c(36459.9204501236,265.020282183699),
                                                       cm = c(1.55, 151.5)),
                          cmPerPixel = 0.0041428488351743,
                          imageRoi = raster::extent(matrix(c(292,1447,280,36423),nrow = 2,byrow = T)),
                          wavelengths = c(550, 570, 590, 615, 630, 650, 659, 660, 661, 662, 663, 664, 665, 666, 667, 668, 669, 670, 671, 690, 730, 790, 845, 900),
                          roi = list(raster::extent(matrix(c(815.988405181892,1017.03901055373,346.842904402071,36493.626372557),nrow = 2,byrow = T))),
                          output.dir = '/Volumes/data/Downloads/Lakes380_TEN6m1_May21_3-4,5m_A_2022-06-09_23-54-44/products',
                          corename = 'Lakes380_TEN6m1_May21_3-4,5m_A_2022-06-09_23-54-44')

