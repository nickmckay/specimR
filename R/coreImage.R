#' Create a normalized core image
#'
#' @param png.path
#' @import imager
#' @return
#' @export
normalizeCoreImage <- function(png.path){
  im <- imager::load.image(png.path)
  #function to equalise intensity values
  hist.eq <- function(im) imager::as.cimg(ecdf(im)(im),dim=dim(im))
  #split image into rgb channels
  cn <- imager::imsplit(im,"c")
  #equalise each channel individually
  cn.eq <- imager::map_il(cn,hist.eq)
  #recombine and plot
  im2 <- imager::imappend(cn.eq,"c")
return(im2)

}


