#' choose the layers to use for plotting red, green, and blue
#'
#' @param wavelengths
#'
#' @return best matched layers for the three colors
#'
defineRGB <- function(wavelengths){
  wavelengths <- as.numeric(wavelengths)
  flags1 <- c(0,0,0)

  redMin <- min(abs(wavelengths-700))
  red1 <- which(redMin==abs(wavelengths-700))[1]

  greenMin <- min(abs(wavelengths-546.))
  green1 <- which(greenMin==abs(wavelengths-546.))[1]

  blueMin <- min(abs(wavelengths-435.8))
  blue1 <- which(blueMin==abs(wavelengths-435.8))[1]

  RGB1 <- c(red1, green1, blue1)

  if (redMin>25){
    flags1[1] <- 1
  } else if (greenMin>25) {
    flags1[2] <- 1
  } else if (blueMin>25) {
    flags1[3] <- 1
  }

  layersAndFlags <- list(layers=RGB1,flags=flags1)

  return(layersAndFlags)
}
