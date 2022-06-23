library(raster)
library(magrittr)

tif <- raster::brick("~/Download/normalized.tif")
rda <- load("~/Download/normalized.RData")

#alternate test
values <- getValuesBlock(tif,lyrs = 3,nrows = dim(tif)[1]) %>% matrix(nrow = dim(tif)[1],byrow = TRUE)


R590 <- rowMeans(values)

plot(R90)
