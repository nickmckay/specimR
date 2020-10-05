  createReferenceMeanRow <- function(refFile,e,outFile,spectra){

  refBrick <- raster::brick(refFile)
  refBrick <-  raster::subset(refBrick,spectra)
  #crop it by the earlier crop width
  ebb <- raster::extent(refBrick)
  ex <- raster::extent(e)
  ebb@xmin <- ex@xmin
  ebb@xmax <- ex@xmax


  refBrick <- raster::crop(refBrick,ebb)

  rbcm <- colSums(refBrick)/nrow(refBrick)

  #preallocate
  r <- raster::brick(ncol=ncol(refBrick), nrow=1,nl = dim(refBrick)[3], xmn=ex@xmin, xmx=ex@xmax, ymn=0, ymx=1)
  r <- setValues(r,rbcm)

  #save row for later processing.
  raster::writeRaster(r,filename = file.path("..",outFile), overwrite = TRUE)

}


createReferenceTif <- function(meanRow,targetExtent,fileOut){
  #calculate number of rows in target
  nrs <- floor(targetExtent@ymax - targetExtent@ymin)

  #load in the row
  if(is.character(meanRow)){#load in the file
    meanRow <- brick(meanRow)
  }
  ref <- meanRow
  print(ref)
  while(nrow(ref)<nrs){
    #duplicate prior chunk
    newChunk <- ref

    #adjust the extent to go below the last chunk
    re <- extent(ref)
    nr <- nrow(ref)

    re@ymin <- re@ymax
    re@ymax <- re@ymin+nr

    extent(newChunk) <- re


    #check to see if it will be too big
    if(nrow(newChunk)*2 > nrs){
      needed <-  nrs-nrow(newChunk)
      ne <- extent(ref)
      ne@ymax <- ne@ymax+needed
      newChunk <- crop(newChunk,ne)
    }

    ref <- merge(ref,newChunk)
    print(nrow(ref))
  }
  #adjust extent to match
  extent(ref) <- targetExtent


  #options   = "COMPRESS=LZW",
  writeRaster(ref,filename = file.path("..",fileOut),overwrite = TRUE,progress = "text")

}


normFun <- function(data,white,dark){
  s1 <- (data - dark)
  s1[is.na(s1)] <- 0
  s1[s1<0] <- 0
  return(s1 / (white - dark))
}

createReferenceTif2 <- function(meanRow,targetExtent,fileOut){
  #calculate number of rows in target
  nrs <- floor(targetExtent@ymax - targetExtent@ymin)

  #load in the row
  if(is.character(meanRow)){#load in the file
    meanRow <- brick(meanRow)
  }
  ref <- meanRow
  print(ref)
  while(nrow(ref)<nrs){
    #duplicate prior chunk
    newChunk <- ref

    #chunk 1
    #adjust the extent to go below the last chunk
    re <- extent(ref)
    nr <- nrow(ref)

    re@ymin <- re@ymax
    re@ymax <- re@ymin+nr

    extent(newChunk) <- re


    #chunk 2
    newChunk2 <- newChunk
    re <- extent(newChunk)
    nr <- nrow(newChunk)

    re@ymin <- re@ymax
    re@ymax <- re@ymin+nr

    extent(newChunk2) <- re

    #chunk 3
    newChunk3 <- newChunk2
    re <- extent(newChunk2)
    nr <- nrow(newChunk2)

    re@ymin <- re@ymax
    re@ymax <- re@ymin+nr

    extent(newChunk3) <- re


    #check to see if it will be too big
    if(nrow(newChunk)*4 > nrs){
      needed <-  nrs-nrow(newChunk)
      ne <- extent(ref)
      ne@ymax <- ne@ymax+needed
      newChunk <- crop(newChunk,ne)
    }

    ref <- merge(ref,newChunk,newChunk2,newChunk3)
    print(nrow(ref))
  }
  #adjust extent to match
  extent(ref) <- targetExtent


  #options   = "COMPRESS=LZW",
  writeRaster(ref,filename = file.path("..",fileOut),overwrite = TRUE,progress = "text")

}

