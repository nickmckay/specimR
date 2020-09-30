##Script for taking normalized data output from normalize.R function and calculating indices

#choose desired outputs


#transpose normalized dataset and get normalized values
getNormValues <- function(normalized){
  norm <- t(normalized[[3]])
  data <- raster::getValues(norm,row = 1)
  colnames(data) <- as.character(normalized[[2]])
return(data)
}

coreLengthInfo <- function(normalized){
  scaleY <- normalized[[4]]
  return(scaleY)
}

#collect band information for calculations-all bands first
allBandInfo <- function(normalized){
allbands <- normalized[[1]]
return(allbands)
}

#and subsetted bands
subsetBands <- function(data){
bands <- gsub("X","",colnames(data))%>%as.numeric()
bands <- data.table::data.table(Value=bands)
bands[,merge:=Value]
data.table::setkeyv(bands,c('merge'))
return(bands)
}

#RABD660
RABD660 <- function(data,bands, allbands,indices){
  if (indices[grepl("RABD660",indices)]!="RABD660"){ print("next")
} else{
vals_660 <- data.table::data.table(Value = c(590,660,730))
vals_660[,merge:=Value]
data.table::setkeyv(vals_660,c('merge'))
r660 <-bands[vals_660,roll='nearest']
r660n <- as.character(paste0("X",r660$Value))
r660v <- data[,r660n]

dBtwnLo <- sum(allbands < r660$Value[2] & allbands > r660$Value[1])
dBtwnHi <- sum(allbands < r660$Value[3] & allbands > r660$Value[2])
dBtwnTot <- sum(allbands < r660$Value[3] & allbands > r660$Value[1])
RABD_660 <- as.data.frame((((dBtwnLo*r660v[,3])+(dBtwnHi*r660v[,1]))/dBtwnTot)/r660v[,2])
colnames(RABD_660) <- "RABD660"
return(RABD_660)
}
}
#RABD 845
RABD845 <- function(data,bands, allbands,indices){
  if (indices[grepl("RABD845",indices)]!="RABD845"){ print("next")
  } else{
vals_845 <- data.table::data.table(Value = c(790,845,900))
vals_845[,merge:=Value]
data.table::setkeyv(vals_845,c('merge'))
r845 <-bands[vals_845,roll='nearest']
r845n <- as.character(paste0("X",r845$Value))
r845v <- data[,r845n]

dBtwnLo <- sum(allbands < r845$Value[2] & allbands > r845$Value[1])
dBtwnHi <- sum(allbands < r845$Value[3] & allbands > r845$Value[2])
dBtwnTot <- sum(allbands < r845$Value[3] & allbands > r845$Value[1])
RABD_845 <- as.data.frame((((dBtwnLo*r845v[,3])+(dBtwnHi*r845v[,1]))/dBtwnTot)/r845v[,2])
colnames(RABD_845) <- "RABD845"
return(RABD_845)
}
}

R570R630 <-function(data,bands,indices){
  if (indices[grepl("R570R630",indices)]!="R570R630"){ print("next")
  } else{
  vals_r570r630 <- data.table::data.table(Value = c(570,630))
  vals_r570r630[,merge:=Value]
  data.table::setkeyv(vals_r570r630,c('merge'))
  r570r630 <-bands[vals_r570r630,roll='nearest']
  r570r630n <- as.character(paste0("X",r570r630$Value))
  r570r630v <- data[,r570r630n]
  R570_R630 <-as.data.frame(r570r630v[,1]/r570r630v[,2])
  colnames(R570_R630) <- "R570_R630"
return(R570_R630)
}
}

R590R690 <-function(data,bands, indices){
  if (indices[grepl("R590R690",indices)]!="R590R690"){ print("next")
  } else{
  vals_r590r690 <- data.table::data.table(Value = c(590,690))
  vals_r590r690[,merge:=Value]
  data.table::setkeyv(vals_r590r690,c('merge'))
  r590r690 <-bands[vals_r590r690,roll='nearest']
  r590r690n <- as.character(paste0("X",r590r690$Value))
  r590r690v <- data[,r590r690n]
  R590_R690 <-as.data.frame(r590r690v[,1]/r590r690v[,2])
  colnames(R590_R690) <- "R590_R690"
  return(R590_R690)
}
}
CalcIndices <- function(normalized,indices){
  data <- getNormValues(normalized = normalized)
  scaleY <-coreLengthInfo(normalized = normalized)
  allbands <- allBandInfo(normalized = normalized)
  bands <- subsetBands(data = data)
  RABD_660 <-RABD660(data=data,bands = bands,allbands = allbands,indices=indices)
  RABD_845 <-RABD845(data=data,bands = bands,allbands = allbands,indices=indices)
  R570_R630 <- R570R630(data=data,bands=bands,indices=indices)
  R590_R690 <- R590R690(data=data,bands=bands,indices = indices)
  indicesVals <- cbind(scaleY,RABD_660,RABD_845,R570_R630,R590_R690)
  return(indicesVals)
}

