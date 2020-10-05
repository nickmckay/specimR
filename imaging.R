gaussianize <- function(im){
  g <- geoChronR::gaussianize(im,jitter = T)
  out <- (g+min(g))/diff(range(g))
    return(as.cimg(out))
}
hist.eq <- function(im) as.cimg(ecdf(im)(im),dim=dim(im))
hist.eq.trunc <- function(im){
  im[im>0.2] <- NA
  as.cimg(ecdf(im)(im),dim=dim(im))
}

sat <- function(im){
  out <- RGBtoHSV(im) %>%
  imsplit("c")

  V <- out[[3]]
  newV <- hist.eq(V)
  out[[3]] <- newV
  out <- imappend(out,"c")
  return(HSVtoRGB(out))
}

aphist <- function(im){
  cn <- imsplit(im,"c")
cn.eq <- map_il(cn,gaussianize) #run hist.eq on each
return(imappend(cn.eq,"c"))
}


overview <- imager::resize(thumbnail,-5,-5) %>% aphist()

top <- imsub(thumbnail, y < width)

bot <- imsub(thumbnail, y > height-width) %>% aphist()



