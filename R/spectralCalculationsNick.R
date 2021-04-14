#
#' Transpose normalized dataset and get normalized values
#'
#' @param normalized the output of specimR::normalize()
#'
#' @return matrix of normalized values averaged across the core
#' @export
calculateMeanRows <- function(normalized){

  averageRow <- function(rn,r){
    a <- apply(raster::getValues(r,rn,1),2,mean)
  }

  data <- purrr::map_df(seq_len(nrow(normalized$normalized)),averageRow,normalized$normalized) %>%
    as.matrix

  colnames(data) <- names(normalized$spectra)
  return(data)
}

#' get the wavelengths present in the normalized matrix
#'
#' @param normValues the output of `specimR::calculateMeanRows()`
#'
#' @return a vector of wavelengths
#' @export
getNormWavelengths <- function(normValues){
  return(gsub("X","",colnames(normValues))%>%as.numeric())
}

#' Extract the depth series for a specific wavelength
#'
#' @param normData Normalized spectral data (the output of `specimR::calculateMeanRows()`)
#' @param normWavelengths A vector of wavelengths (the output of `specimR::calculateMeanRows()`)
#' @param wavelengthToGet What wavelength to do you want to extract
#' @param tol Tolerance for distance from the requested wavelength. If no values are present within the tolerance, this returns an error. If multiple are present, this returns the average of those values.
#'
#' @return The depth sequence of normalized reflectance values for the requested wavelength.
#' @export
getNormWavelengthData <- function(normData,normWavelengths,wavelengthToGet,tol = 1,agg.fun = "mean"){
  ind <- which(abs(normWavelengths-wavelengthToGet) <= tol)

  if(length(ind)==0){
    stop(glue::glue("Cannot get wavelength data, nearest wavelength {normWavelengths[ind]} not within {tol} nm of {wavelengthToGet}. Either add more wavelengths or increase the tolerance."))
  }
  if(length(ind)==1){
    return(normData[,ind])
  }else if(length(ind) > 1){
    if(agg.fun == "mean"){
      if(nrow(normData) > 1){
      return(rowMeans(normData[,ind]))
      }else{
        return(mean(normData[,ind]))
      }
    }else if(agg.fun == "min"){
      if(nrow(normData) > 1){
      return(apply(normData[,ind],1,min))
      }else{
        return(min(normData[,ind]))
      }
    }
  }
}

#' Calculate Relative Absorption Band Depth (RABD)
#'
#' @inheritParams getNormWavelengthData
#' @param trough the wavelength for the RABD trough (default = 660)
#' @param edges the edges (as a 2-element vector) of the RABD trough (default = c(590,730))
#' @return a depth series of the RABD index
#' @export
calculateRABD <- function(normData,normWavelengths,tol = 5,trough = 665, edges = c(590,730),trough.agg.fun = "min"){
  troughVals <- getNormWavelengthData(normData,normWavelengths,trough,tol,agg.fun = trough.agg.fun)
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
#' @param normalized Normalized spectral data (the output of `specimR::calculateMeanRows()`)
#' @param indices a character vector of the requested indices. Current options are: "RABD660","RABD845","R570R630"and "R590R690" (default = c("RABD660","RABD845","R570R630","R590R690"))
#' @param tol the tolerance to use when matching wavelengths (in nm)
#'
#' @return a data.frame with depth and the requested indices
#' @export
calculateIndices <- function(normalized,
                             indices = c("RABD660","RABD660670","RABD845","R570R630","R590R690"),
                             tol = 1,
                             smooth.win = round(.2/normalized$cmPerPixel)){
  normData <- calculateMeanRows(normalized = normalized)
  normWavelengths <- getNormWavelengths(normData)

  #initialize indices
  outTable <- data.frame(depth = normalized$scaleY)


  #check for indices
  if("RABD615" %in% indices){
    outTable$RABD615 <- calculateRABD(normData,normWavelengths, tol = tol,trough = 615,edges = c(590,730))
  }
  #check for indices
  if("RABD660" %in% indices){
    outTable$RABD660 <- calculateRABD(normData,normWavelengths, tol = tol,trough = 660,edges = c(590,730))
  }
  if("RABD660670" %in% indices){
    outTable$RABD660670 <- calculateRABD(normData,normWavelengths, tol = 5,trough = 665,edges = c(590,730))
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

  smoothOutTable <- mutate(outTable,across(starts_with("R"), smoother::smth,window = smooth.win)) %>%
    rename_with(~ stringr::str_c("smooth", .x)) %>%
    dplyr::select(-smoothdepth)

  out <- dplyr::bind_cols(outTable,smoothOutTable)

  return(out)
}

getNearestWavelengthIndex <- function(wavelengths,wavelengthToGet,tol=1){
  li <-  which(abs(wavelengths-wavelengthToGet) <= tol)
  if(length(li) == 0){
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
rasterRABD <- function(normalized,
                       trough = 660,
                       edges = c(590,730),
                       tol = 1,
                       trough.agg.fun = "min",
                       edge.agg.fun = "mean"){
#find wavelength indices and make rasters

  #low edge
  li <- getNearestWavelengthIndex(normalized$wavelengths,min(edges),tol = tol)
  l <- raster::subset(normalized$normalized,li)
  if(length(li)>1){
    if(edge.agg.fun == "mean"){
      l <- mean(l)
    }else if(edge.agg.fun == "min"){
      l <- min(l)
    }else if(edge.agg.fun == "max"){
      l <- max(l)
    }
  }

  #high edge
  hi <- getNearestWavelengthIndex(normalized$wavelengths,max(edges),tol = tol)
  h <- raster::subset(normalized$normalized,hi)
  if(length(hi)>1){
    if(edge.agg.fun == "mean"){
      h <- mean(h)
    }else if(edge.agg.fun == "min"){
      h <- min(h)
    }else if(edge.agg.fun == "max"){
      h <- max(h)
    }
  }


  #trough
  mi <- getNearestWavelengthIndex(normalized$wavelengths,trough,tol = tol)
  m <- raster::subset(normalized$normalized,mi)
  if(length(mi)>1){
    if(trough.agg.fun == "mean"){
      m <- mean(m)
    }else if(trough.agg.fun == "min"){
      m <- min(m)
    }else if(trough.agg.fun == "max"){
      m <- max(m)
    }
  }


  #distances
  ld <- mean(normalized$wavelengths[mi])-mean(normalized$wavelengths[li])
  hd <- mean(normalized$wavelengths[hi])-mean(normalized$wavelengths[mi])
  td <- mean(normalized$wavelengths[hi])-mean(normalized$wavelengths[li])

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
  ti <- getNearestWavelengthIndex(normalized$wavelengths,top,tol = tol)
  t <- raster::subset(normalized$normalized,ti)
  if(length(ti)>1){
    t <- mean(t)
  }

  #bot
  bi <- getNearestWavelengthIndex(normalized$wavelengths,bot,tol = tol)
  b <- raster::subset(normalized$normalized,bi)
  if(length(bi)>1){
    b <- mean(bi)
  }

  #band ratio
  br <- t/b

  return(br)

}



#' Title
#'
#' @param normalized
#' @param index
#' @param tol
#' @param smooth
#' @param smooth.sigma
#' @param smooth.n
#'
#' @return
#' @export
#'
#' @examples
makeHeatmap <- function(normalized,index = "RABD660",tol = 1,smooth = TRUE, smooth.sigma = 3,smooth.n = 7){
  #decide what function to use to make the raster
    #check for indices
  if("RABD615" == index){
    heatmap <- rasterRABD(normalized = normalized, tol = tol,trough = 615,edges = c(590,730))
  }
    if("RABD660" == index){
      heatmap <- rasterRABD(normalized = normalized, tol = tol,trough = 660,edges = c(590,730))
    }
  if("RABD660670" == index){
    heatmap <- rasterRABD(normalized = normalized, tol = 5,trough = 665,edges = c(590,730))
  }
    if("RABD845" == index){
      heatmap <- rasterRABD(normalized = normalized, tol = tol,trough = 845,edges = c(790,900))
    }


  #band ratios
  if("R570R630"== index){
    heatmap <- rasterBandRatio(normalized = normalized, tol = tol,top = 570, bot = 630)
  }

  if("R590R690" == index){
    heatmap <- rasterBandRatio(normalized = normalized, tol = tol,top = 590, bot = 690)
  }

  if(smooth){
    heatmap <- spatialEco::raster.gaussian.smooth(heatmap,sigma = smooth.sigma,n = smooth.n)
  }

  return(heatmap)



}

