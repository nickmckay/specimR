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


plotRGB(normalized$raw)
normalized$filen
indices <- c("RABD660","RABD845","R570R630","R590R690")
source('~/Documents/GitHub/specimR/R/spectralCalculations.R')
core_output <- GetIndices(normalized = normalized,indices = indices)
indices <- core_output$indicesVals
indices$scaleYmm <- indices$scaleY*10

library(ggplot2)
dat <- ggplot(data=indices)+geom_point(aes(x=RABD660,y=scaleYmm))
dev.print(png, file = "/Users/ethanyackulic/Desktop/test/Lakes380_FORSY_LC1U_2B_test_2020-06-05_04-05-39.png", width=1024,height=768)
plot(1:2, type='n', main="", xlab="x", ylab="y")
image <- png::readPNG("/Users/ethanyackulic/Desktop/test/Lakes380_FORSY_LC1U_2B_test_2020-06-05_04-05-39.png")

lim <- par()

plot.new()
rasterImage(image,0,0,1,1)
extemt
#plot(image)



ggplot()+geom_raster(data = equal)
e1
multiplot(dat,plot(equal))
plot(normalized$raw,add=TRUE)

levelplot(core_output$)
library(rasterVis)
rasterVis::levelplot(normalized$raw, layers=1)
levelplot(normalized$normalized,layers=1,add = TRUE)

library(png)
library(grid)
img <- readPNG(system.file("img", "Rlogo.png", package="png"))
g <- rasterGrob(img, interpolate=TRUE)

qplot(1:10, 1:10, geom="blank") +
  annotation_custom(g, xmin=-Inf, xmax=Inf, ymin=-Inf, ymax=Inf) +
  geom_point()



theme_set(theme_cowplot())
library(cowplot)
library(magick)


extent(equal1) <- extent(normalized$raw)

ggdraw() +
  draw_image(equal)+
  draw_plot(normalized$raw)
  geom_rect(aes(xmin=box@xmin,xmax=box@xmax, ymin=box@ymin,ymax=box@ymax),fill=NA)

draw_image(normalized$raw)
+geom_rect(aes(xmin=0,xmax=2184, ymin=0,ymax=1448),fill=NA)
extent(equal)

library(jpeg)
writeJPEG(raw1)
, filename = "fullCore.jpeg", options="INTERLEAVE=BAND", overwrite=TRUE)
im <- imager::load.image('/Users/ethanyackulic/Documents/GitHub/specimR/normalized.tif')
ggdraw()+draw_image('/Users/ethanyackulic/Documents/GitHub/specimR/normalized.tif')

box <- extent(normalized$stripe)

plot.new()
plot(normalized$stripe)

rasterImage(equal,xleft= 0,xright =2184,ybottom=0,ytop=1448)
rasterImage(normalized$stripe,xleft= 0,xright =2184,ybottom=0,ytop=1448)
rgb <- c("X470.57", "X539.66","X630.49")
raw <- (normalized$normalized)
raw1 <- as.array(subset(raw,rgb))
plot(raw1)
dim(raw1)
extent(equal) <- extent(normalized$raw)
normalized$stripe@extent@ymin <- normalized$raw@extent@ymax - normalized$stripe@extent@ymax
normalized$stripe@extent@ymax <- normalized$raw@extent@ymax

im <- imager::load.image("/Users/ethanyackulic/Desktop/test/Lakes380_FORSY_LC1U_2B_test_2020-06-05_04-05-39.png")
plot(im,rescale = FALSE)
cscale <- function(r,g,b) rgb(g,r,b)
bscale <- function(r,g,b) rgb(b,g,r)
plot(im,colourscale=bscale,rescale=FALSE)

im.g <- imager::grayscale(im)
f <- ecdf(im.g)
plot(f)
library(tidyverse);library(imager)
f(im.g) %>% imager::as.cimg(dim=dim(im.g)) %>% plot(main="With histogram equalisation")

hist.eq <- function(im) as.cimg(ecdf(im)(im),dim=dim(im))
cn <- imsplit(im,"c")
cn.eq <- map_il(cn,hist.eq) #run hist.eq on each
equal <- imappend(cn.eq,"c")
raster::plot(equal)

library(purrr)
#Convert to HSV, reduce saturation, convert back
RGBtoHSV(im.g) %>% imsplit("c") %>%
  modify_at(2,~ . / 2) %>% imappend("c") %>%
  HSVtoRGB %>% plot(rescale=FALSE)


desat <- function(im) RGBtoHSV(im) %>% imsplit("c") %>%
  modify_at(2,~ . / 2) %>% imappend("c") %>%
  HSVtoRGB

#Split image into 3 blocks, reduce saturation in middle block, recombine
imsplit(im.g,"x",3) %>% modify_at(2,desat) %>%
  imappend("x") %>% plot(rescale=FALSE)


