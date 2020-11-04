#
#' Transpose normalized dataset and get normalized values
#'
#' @param normalized the output of specimR::normalize()
#'
#' @return matrix of normalized values averaged across the core
#' @export
getNormValues <- function(normalized){
  norm <- t(normalized$normalized)
  data <- raster::getValues(norm,row = 1)
  colnames(data) <- names(normalized$spectra)
  return(data)
}

#' get the wavelengths present in the normalized matrix
#'
#' @param normValues the output of `specimR::getNormValues()`
#'
#' @return a vector of wavelengths
#' @export
getNormWavelengths <- function(normValues){
  return(gsub("X","",colnames(normValues))%>%as.numeric())
}

#' Extract the depth series for a specific wavelength
#'
#' @param normData Normalized spectral data (the output of `specimR::getNormValues()`)
#' @param normWavelengths A vector of wavelengths (the output of `specimR::getNormValues()`)
#' @param wavelengthToGet What wavelength to do you want to extract
#' @param tol Tolerance for distance from the requested wavelength. If no values are present within the tolerance, this returns an error. If multiple are present, this returns the average of those values.
#'
#' @return The depth sequence of normalized reflectance values for the requested wavelength.
#' @export
getNormWavelengthData <- function(normData,normWavelengths,wavelengthToGet,tol = 1){
  ind <- which(abs(normWavelengths-wavelengthToGet) <= tol)

  if(length(ind)==0){
    stop(glue::glue("Cannot get wavelength data, nearest wavelength {normWavelengths[ind]} not within {tol} nm of {wavelengthToGet}. Either add more wavelengths or increase the tolerance."))
  }
  if(length(ind)==1){
    return(normData[,ind])
  }else if(length(ind) > 1){
    return(rowMeans(normData[,ind]))

  }
}

#' Calculate Relative Absorption Band Depth (RABD)
#'
#' @inheritParams getNormWavelengthData
#' @param trough the wavelength for the RABD trough (default = 660)
#' @param edges the edges (as a 2-element vector) of the RABD trough (default = c(590,730))
#' @return a depth series of the RABD index
#' @export
calculateRABD <- function(normData,normWavelengths,tol = 1,trough = 660, edges = c(590,730)){
  troughVals <- getNormWavelengthData(normData,normWavelengths,trough,tol)
  lowVals <- getNormWavelengthData(normData,normWavelengths,min(edges),tol)
  highVals <- getNormWavelengthData(normData,normWavelengths,max(edges),tol)

  dLo <- trough - min(edges)
  dHi <- max(edges) - trough
  dTot <- dLo + dHi

index <- (dHi*lowVals + dLo*highVals)/(dTot*troughVals)

return(index)
}

#' Calculate a Band Ratio
#'
#' @inheritParams getNormWavelengthData
#' @param top the wavelength in the top of the ratio (the numerator) (default = 570)
#' @param bot the wavelength in the top of the ratio (the denominator) (default = 630)
#' @return a depth series of the band ratio
#' @export
calculateBandRatio <- function(normData,normWavelengths,tol = 1,top = 570, bot = 630){
  topVals <- getNormWavelengthData(normData,normWavelengths,top,tol)
  botVals <- getNormWavelengthData(normData,normWavelengths,bot,tol)
  return(topVals/botVals)
}

#' calculate a suite of spectral indices on normalized data
#'
#' @param normalized Normalized spectral data (the output of `specimR::getNormValues()`)
#' @param indices a character vector of the requested indices. Current options are: "RABD660","RABD845","R570R630"and "R590R690" (default = c("RABD660","RABD845","R570R630","R590R690"))
#' @param tol the tolerance to use when matching wavelengths (in nm)
#'
#' @return a data.frame with depth and the requested indices
#' @export
calculateIndices <- function(normalized,indices = c("RABD660","RABD845","R570R630","R590R690"),tol = 1){
  normData <- getNormValues(normalized = normalized)
  normWavelengths <- getNormWavelengths(normData)

  #initialize indices
  outTable <- data.frame(depth = normalized$scaleY)

  #check for indices
  if("RABD660" %in% indices){
    outTable$RABD660 <- calculateRABD(normData,normWavelengths, tol = tol,trough = 660,edges = c(590,730))
  }
  if("RABD845" %in% indices){
    outTable$RABD845 <- calculateRABD(normData,normWavelengths, tol = tol,trough = 845,edges = c(790,900))
  }
  if("R570R630" %in% indices){
    outTable$R570R630 <- calculateBandRatio(normData,normWavelengths, tol = tol,top = 570, bot = 630)
  }

  if("R590R690" %in% indices){
    outTable$R590R690 <- calculateBandRatio(normData,normWavelengths, tol = tol,top = 590, bot = 690)
  }

  #add running means

  smoothOutTable <- mutate(outTable,across(starts_with("R"), smooth)) %>%
    rename_with(~ stringr::str_c("smooth", .x)) %>%
    dplyr::select(-smoothdepth)

  out <- dplyr::bind_cols(outTable,smoothOutTable)

  return(out)
}

getNearestWavelengthIndex <- function(wavelengths,wavelengthToGet,tol=1){
  li <-  which.min(abs(wavelengths-wavelengthToGet))[1]
  if(abs(wavelengths[li]-wavelengthToGet) > tol){
    stop("no wavelengths within tolerance")
  }
  return(li)
}

#' calculate RABD over a raster
#'
#' @param normalized output list of specimR::normalize()
#' @param trough wavelength for the RABD trough
#' @param edges a 2-element vector marking the edges of the trough
#' @param tol tolerance for finding the nearest wavelength (default = 1)
#'
#' @return
#' @export
#'
#' @examples
rasterRABD <- function(normalized,trough = 660, edges = c(590,730),tol = 1){
#find wavelength indices and make rasters

  #low edge
  li <- getNearestWavelengthIndex(normalized$wavelengths,min(edges))
  l <- raster::subset(normalized$normalized,li)

  #high edge
  hi <- getNearestWavelengthIndex(normalized$wavelengths,max(edges))
  h <- raster::subset(normalized$normalized,hi)

  #trough
  mi <- getNearestWavelengthIndex(normalized$wavelengths,trough)
  m <- raster::subset(normalized$normalized,hi)

  #distances
  ld <- normalized$wavelengths[mi]-normalized$wavelengths[li]
  hd <- normalized$wavelengths[hi]-normalized$wavelengths[mi]
  td <- normalized$wavelengths[hi]-normalized$wavelengths[li]

  #rabd
  rabd <- (l*hd+h*ld)/(m*td)

  return(rabd)

}


#' calculate band ratio over a raster
#'
#' @inheritParams calculateBandRatio
#' @param trough wavelength for the RABD trough
#' @param edges a 2-element vector marking the edges of the trough
#' @param tol tolerance for finding the nearest wavelength (default = 1)
#'
#' @return a raster object with the spatial band ratio
#' @export
rasterBandRatio <- function(normalized,top = 570, bot = 630,tol = 1){
  #find wavelength indices and make rasters

  #top
  ti <- getNearestWavelengthIndex(normalized$wavelengths,top)
  t <- raster::subset(normalized$normalized,ti)

  #bot
  bi <- getNearestWavelengthIndex(normalized$wavelengths,bot)
  b <- raster::subset(normalized$normalized,bi)

  #band ratio
  br <- t/b

  return(br)

}

