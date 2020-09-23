#######################################################################################################
#                                                                                                     #
#                            Functions for spectral index calculations                                #
#                                                                                                     #
#######################################################################################################
#                                                                                                     #
#                                   created by Christoph Butz                                         #
#                                        09.October 2015                                              #
#                                 christoph.butz@giub.unibe.ch                                        #
#                                                                                                     #
# Latest fix: axes labels are now consistent                                                          #
#######################################################################################################

#######################################################################################################
#                                    Libraries & Packages                                             #
#######################################################################################################
libraries <- function(){

  #########################

  # necessary package to read and write ENVI files
  # also provides moving window capabilities and more
  lib1 <- library("caTools", logical.return=TRUE)
  if(lib1==FALSE) install.packages("caTools"); library("caTools")

  # package to read tiff images
  lib2 <- library("rtiff", logical.return=TRUE)
  if(lib2==FALSE) install.packages("rtiff"); library("rtiff")

  # package to create hash tables
  lib3 <- library("hash", logical.return=TRUE)
  if(lib3==FALSE) install.packages("hash"); library("hash")

  # package for mean absolute deviation (MAD) Filters
  lib4 <- library("pracma", logical.return=TRUE)
  if(lib4==FALSE) install.packages("pracma"); library("pracma")

  # package for mean absolute deviation (MAD) Filters
  lib5 <- library("schoolmath", logical.return=TRUE)
  if(lib5==FALSE) install.packages("schoolmath"); library("schoolmath")

  # package for creating reports in Open Document format
  lib6 <- library("odfWeave", logical.return=TRUE)
  if(lib6==FALSE) install.packages("odfWeave"); library("odfWeave")

  # package for minor tick marks
  lib7 <- library("Hmisc", logical.return=TRUE)
  if(lib7==FALSE) install.packages("Hmisc"); library("Hmisc")

  # package for minor tick marks
  lib8 <- library("magicaxis", logical.return=TRUE)
  if(lib8==FALSE) install.packages("magicaxis"); library("magicaxis")

  # package for minor tick marks
  lib9 <- library("Cairo", logical.return=TRUE)
  if(lib9==FALSE) install.packages("Cairo"); library("Cairo")

  # Remove unneeded variables
  rm(list=c("lib1","lib2","lib3","lib4","lib5","lib6","lib7","lib8","lib9"))

}


#######################################################################################################
#                       Function to find the closest match for a value in a vector                    #
#######################################################################################################

closest_values <- function(vector, value, Zero){

  # This function finds and returns the closest values in a vector and returns the index subscripts
  # vector = vector to search in
  # value  = vector of search values
  # Zero   = if TRUE index is zero based
  if (Zero==TRUE) l <- (1) else l <- (0)

  vector  <-  as.vector(vector, mode="numeric")
  Result  <-  as.vector(length(value), mode= "integer")


  for(i in 1:(length(value))){
    near  <- which.min(abs(vector-value[i]))
    Result[i] <- near
  }
  Result <- (Result-l)
  return(Result)
}

#######################################################################################################
#                               Function to split a Path into its elements                            #
#######################################################################################################

Pathbreakdown <- function(string){
  if (length(grep("\\",string,fixed=TRUE))==1){
    X   <- gregexpr("\\",string,fixed=TRUE)
    Arr <- strsplit(string,"\\",fixed=TRUE)
    Arr <- as.character(Arr[[1]][])
  }else{
    if (length(grep(("\"") , string,fixed=TRUE))==1){
       X   <- gregexpr("\"",string,fixed=TRUE)
       Arr <- strsplit(string,"\"",fixed=TRUE)
       Arr <- as.character(Arr[[1]][])
    }else{
      if(length(grep("/",string,fixed=TRUE))==1){
         X   <- gregexpr("/",string,fixed=TRUE)
         Arr <- strsplit(string,"/",fixed=TRUE)
         Arr <- as.character(Arr[[1]][])
      }
    }
  }
  return(Arr)
}

#######################################################################################################
#               Function to calculate number of pixels for A4 Papersize depending on dpi              #
#######################################################################################################

Dims_A4 <- function(dpi=300,orient="p"){

  #A4 Size [mm]
  A4dims    <- c(297,210)
  drawdimsP <- c(204,170)
  drawdimsL <- c(271,126)

  #Pixel size in mm
  PS <- c(25.4/dpi)

  if(orient=="p"){
    #Paper dims portrait
    pdimsy <- c(A4dims[1])
    pdimsx <- c(A4dims[2])
    #plot area dims
    ddimsy <- c(drawdimsP[1])
    ddimsx <- c(drawdimsP[2])
  }else{
    #Paper dims landscape
    pdimsy <- c(A4dims[2])
    pdimsx <- c(A4dims[1])
    #plot area dims
    ddimsy <- c(drawdimsL[2])
    ddimsx <- c(drawdimsL[1])
  }
  #Pixelvalues for plotarea
  ddimsyPX <- ceil(ddimsy/PS)
  ddimsxPX <- ceil(ddimsx/PS)
  #Pixelvalue for maximum tiff x size
  Tiffxmax <- ceil(40/PS)
  #pixelvalues for page size
  pdimsyPX <- ceil(pdimsy/PS)
  pdimsxPX <- ceil(pdimsx/PS)

  return(c(ddimsyPX,ddimsxPX,Tiffxmax,pdimsyPX,pdimsxPX))

}

#######################################################################################################
#                                   Function to read an ENVI header                                   #
#######################################################################################################

readENVI.hdr <- function(Header_File){

  # package to create hash tables
  lib3 <- library("hash", logical.return=TRUE)
  if(lib3==FALSE) install.packages("hash"); library("hash")
  rm(list=c("lib3"))

  Header <- readLines(Header_File)

  # create hash table
  ENVI_header = hash()

  # Add metadata from header file

    # Define standard numeric variables in a Envi header file
    numvars  <- c("samples","lines","bands","x start","y start","data type",
                  "header offset","byte order","fov","resolution","fps","tint","sensorid")
    numvars1 <- c("samples = ","lines   = ","bands   = ","x start = ","y start = ",
                  "data type = ","header offset = ","byte order = ","fov = ","resolution = ",
                  "fps = ","tint = ","sensorid = ")
    numvars2 <- c("samples","lines","bands","x_start","y_start","data_type","header_offset",
                  "byte_order","fov","resolution","fps","tint","sensorid")

    # Read numeric vars into hash
    for (i in 1:length(numvars)){

      temp <- grep(numvars[i],Header, fixed=TRUE)
      if (length(temp)!=0){
        temp <- c(substr(Header[temp[1]],start=nchar(numvars1[i]), stop=nchar(Header[temp[1]])))
        .set(ENVI_header,keys=numvars2[i],values=as.numeric(temp[1]))
      }
    }
    #cleanup
    rm(list=c("numvars","numvars1","numvars2","i"))

    ################################################################################################
    # Define std. text vars
    strvars  <- c("sensor type","file type","wavelength units","acquisition date","lake name","interleave")
    strvars1 <- c("sensor type = ","file type = ","wavelength units = ","acquisition date = ","lake name = ",
                  "interleave = ")
    strvars2 <- c("sensor_type","file_type","wavelength_units","acquisition_date","lake_name","interleave")
    # Read text meta data
    for (i in 1:length(strvars)){

      temp <- grep(strvars[i],Header, fixed=TRUE)
      if (length(temp)!=0){
        temp <- c(substr(Header[temp[1]],start=nchar(strvars1[i]), stop=nchar(Header[temp[1]])))
        .set(ENVI_header,keys=strvars2[i],values=temp[1])
      }
    }
     #cleanup
    rm(list=c("strvars","strvars1","strvars2","i"))

    ################################################################################################
    # Special metadata
    # default bands
    temp <- grep("default bands",Header, fixed=TRUE)
    temp <- c(substr(Header[temp],start=(nchar("default bands = {")+1), stop=(nchar(Header[temp])-1)))
    .set(ENVI_header,keys="default_bands",values=as.numeric(unlist(strsplit(temp, "[,]"))))
    #cleanup
    rm(list="temp")

    # wavelength
    temp <- grep("wavelength =",Header, fixed=TRUE)
    var1 <- grep("{",Header, fixed=TRUE)
    var2 <- grep("}",Header, fixed=TRUE)
    var3 <- which(var1==temp)
    if (length(temp)!=0){
      wl <- c()
      for (i in (var1[var3]+1):var2[var3]){
        wl <- paste(wl,Header[i],sep="")
      }
      wl <- gsub("}","",wl,fixed=TRUE)
      .set(ENVI_header,keys="wavelength",values=as.numeric(unlist(strsplit(wl, "[,]"))))
      #cleanup
      rm(list=c("wl","i"))
    }
    #cleanup
    rm(list=c("temp","var3"))

    #Full width half maximum (fwhm)
    temp <- grep("fwhm =",Header, fixed=TRUE)
    var3 <- which(var1==temp)
    if (length(temp)!=0){
      fwhm <- c()
      for (i in (var1[var3]+1):var2[var3]){
        fwhm <- paste(fwhm,Header[i],sep="")
      }
      fwhm <- gsub("}","",fwhm,fixed=TRUE)
      .set(ENVI_header,keys="fwhm",values=as.numeric(unlist(strsplit(fwhm, "[,]"))))
      #cleanup
      rm(list=c("temp","var3","fwhm","i"))
    }

    # Description
    temp <- grep("description =",Header, fixed=TRUE)
    var3 <- which(var1==temp)
    if (length(temp)!=0){
      Desc <- c()
      for (i in (var1[var3]+1):var2[var3]){
        Desc <- paste(Desc,Header[i],sep="")
      }
      Desc <- gsub("}","",Desc,fixed=TRUE)
      .set(ENVI_header,keys="description",values=Desc)
      #cleanup
      rm(list=c("Desc","i"))
    }
    rm(list=c("temp","var3"))

  return(ENVI_header)

}


#######################################################################################################
#                          Plot function for Spectral Index output                                    #
#######################################################################################################
SpectralIndices <- function(Parameters,PATH,dims,General,cracks,nsi,graphout,Age_Depth, Highres) {

  # General length parameters of the Core
  #######################################
    # Pixel number in output matrices
    Oid             <- seq(from=dims[3], to=(dims[4]-1), by=1)
    # number of pixels in the subset
    nsub            <- dims[4]-dims[3]
    # Core.length in millimeter (from starting point (Y.Start))
    Subset.Length   <- nsub*General$PS
    Subset.Y        <- seq(from=0, to=Subset.Length-General$PS, by=General$PS)
    # length of the subset in millimeter
    Subset.Start    <- dims[3]*General$PS
    Subset.Stop     <- dims[4]*General$PS
    # Vector of Core length per millimeter (whole image)
    Core.Y          <- seq(from=Subset.Start, to=Subset.Stop-General$PS, by=General$PS)
    Core.length     <-  General$n*General$PS
    # Sediment length in millimeter (multiple cores)
    Sediment.Length <- General$M.core.st + Core.length
    Sediment.Y      <- seq(from = General$M.core.st + Subset.Start,
                           to   = General$M.core.st + Subset.Stop - General$PS, by=General$PS)
    Sediment.Start  <- General$M.core.st + Subset.Start
    Sediment.Stop   <- General$M.core.st + Subset.Stop
    # y axis labels and position 10mm/100mm based. if more then 20 tickmarks are needed switch to 100 base.
    if(Highres==FALSE){
      nticks <- length(seq(from=(round(Sediment.Start,digits=0)), to=(round(Sediment.Stop, digits=0)), by=10))
      if(nticks>20){
        yt <- seq(from=round(Sediment.Start,digits=0), to=round(Sediment.Stop, digits=0), by=100)
        yt <- pretty(yt, n=nticks/10)
        if(yt[1]<Sediment.Start){yt[1] <- ceiling(Sediment.Start)}
        if(yt[length(yt)]> Sediment.Stop){yt[length(yt)] <- floor(Sediment.Stop)}else{yt <- c(yt,floor(Sediment.Stop))}
        prettyb <- 50
      }else{
        yt <- seq(from=(round(Sediment.Start,digits=0)), to=(round(Sediment.Stop, digits=0)), by=10)
        yt <- pretty(yt, n=nticks)
        if(yt[1]<Sediment.Start){yt[1] <- Sediment.Start}
        if(yt[length(yt)]> Sediment.Stop){yt[length(yt)] <- floor(Sediment.Stop)}else{yt <- c(yt,floor(Sediment.Stop))}
        prettyb <- 5
      }
    }else{
      nticks  <- 10
      prettyb <- 5
      yt      <- seq(from=(round(Sediment.Start,digits=0)), to=(round(Sediment.Stop, digits=0)), by=10)
      yt      <- pretty(yt, n=length(yt))
      if(yt[1]<Sediment.Start){yt[1] <- Sediment.Start}
      if(yt[length(yt)]> Sediment.Stop){yt[length(yt)] <- floor(Sediment.Stop)}else{yt <- c(yt,floor(Sediment.Stop))}

    }
    if(length(Age_Depth)>1){
        yt1 <- closest_values(Age_Depth[,2],yt,0)
        yt1 <- Age_Depth[yt1,1]
        yt2 <- yt
      }else{
        yt1 <- yt
        yt2 <- yt
    }

    # Today's date
    current.date <- format(Sys.time(), "%d %b %Y")

  # Load Tiff & metadata
  ##########################

  # create Path to image
  Tiff  <- file.path(PATH$Tiff,General$Tiffmode[1],"\\",General$Filebasename,"_",
                     General$Tiffmode[1],"_",General$Tiffmode[2],".tif",fsep="")
  # Open HD image
  if (Highres==TRUE){
    reduce   <- 0
    reduce.m <- 0
    Photo  <- readTiff(Tiff)
    Photo.m <- Photo
    #retrieve pixmap size (use "@" instead of "$" to address class4 structure)
    pmx <- Photo@size[2]
    pmy <- Photo@size[1]
    # Native resolution of data pixels-per-inch (ppi)
    Resolution  <- (1/General$PS)*25.4

    # HD plot settings concerning the size of the plot
    ##################################################
      # Y frame size of the image (2*1inch*Resolution) upper frame + lower frame
      Fsize_y       <- ceil(2*1.25*Resolution)
      # Size of the Y outer margin in inch
      outermargin   <- c(1.5,0.5,1,0.5)
      outermargin.m <- c(1.5,0.5,1,0.5)
      # Size of the title characters depending on image resolution
      if (Resolution < 400){Tcex  <- 2}else{Tcex  <- 1}
      # Character size
      Chars         <-  5
      Chars.m       <-  5
      # Main textbody expension factor
      Mcex <- 1
      # Axis label expansion factor
      Acex <- 1.5
      # X Frame size of the image: left frame + right frame
      Fsize_x     <- ceil(2*0.5*Resolution)
      # Size of inner margin in pixel needed for index titles in multiplots
      Mai_size    <- ceil(0.8*Resolution)
      # Line of Text coordinates
      Lines <- c(1.0,1.5,4.5,3,1.5,2.5,3.5,5,6,7,5,6,8,9,10,6)

      # Tiff size from file: standard tiff size is 600 Pixels c(start,End)
      Tiff.x   <- c(1,pmx-1)
      Tiff.x.m <- c(1,pmx-1)
      # Layout width (relative)
      lwidth <- c(1,1,2)
      # Size of output png's in pixel
      ###############################
      # Width (1600=2*400+1*800=Tiff1+Tiff2+Graph)
      png.width    <- 4*pmx+Fsize_x                   # Pixel
      # size of sample, size of frame
      png.height   <- nsub+2+Fsize_y                     # Pixel
      # Size of multi plot
      png.width.m  <- (nrow(Parameters)+1)*pmx+Fsize_x     # Pixel
      png.height.m <- nsub+2+Fsize_y+Mai_size              # Pixel

    if(General$output!="png"){
      png.width    <- png.width/Resolution               # inch
      # size of sample, size of frame
      png.height   <- png.height/Resolution              # inch
      # Size of multi plot
      png.width.m  <- png.width.m/Resolution             # inch
      png.height.m <- png.height.m/Resolution            # inch
      General$File.unit <- c("in")
      Resolution   <- c("auto")
    }
  }

  # Open SD Image
  if (Highres==FALSE){
    # Calculate A4 dimensions
    Resolution <- 300
    A4dimsP    <- Dims_A4(Resolution,"p")
    A4dimsL    <- Dims_A4(Resolution,"l")

    # Reducing factor for low res (2400= number of y-pixels in output=DINA4@300dpi)
    reduce   <- 1-(A4dimsP[1]/nsub)
    if(nsub<=A4dimsP[1]){reduce <- 0}
    Photo    <- readTiff(Tiff, reduce = reduce)
    # Check if image width is in correct bounds
    pmx      <- Photo@size[2]
    pmy      <- Photo@size[1]
    # Set Tiff.x (single graphs)
    if(pmx > A4dimsP[3]){
      Tiff.x <- c(round((pmx/2)-A4dimsP[3]/2),round((pmx/2)+A4dimsP[3]/2))
      lwidth <- c(1,1,((A4dimsP[2]-(2*A4dimsP[3]))/A4dimsP[3]))
    }else{
      Tiff.x <- c(1,pmx-1)
      lwidth <- c(1,1,((A4dimsP[2]-(2*pmx))/pmx))
    }

    # 1490=2480-o_margin(750)-inner_margin(240)
    reduce.m <- 1-(A4dimsL[1]/nsub)
    if(nsub<=A4dimsL[1]){reduce.m <- 0}
    Photo.m  <- readTiff(Tiff, reduce = reduce.m)
    pmx.m <- Photo.m@size[2]
    pmy.m <- Photo.m@size[1]
    # Set Tiff.x.m (multiple graphs)
    iw <- floor(A4dimsL[2]/(nrow(Parameters)+1))
    if(pmx.m > iw){Tiff.x.m <- c((round(pmx.m/2)-floor(iw/2)),(round(pmx.m/2)+floor(iw/2)))
    }else{Tiff.x.m <- c(1,pmx.m-1)}

    Chars   <- 12
    Chars.m <- 10
    #check if image is to small and make the margin bigger if yes.
    #Happens in lowres when the subset size is so small that the image does not need to be reduced
    # 296.333: Papersize in mm for 3500 Px@300dpi
    if(nsub<=A4dimsP[1]){small   <- (A4dimsP[1]-nsub-2)*(297/A4dimsP[4])/25.4}else{small   <- 0}
    if(nsub<=A4dimsL[1]){small.m <- (A4dimsL[1]-nsub-2)*(210/A4dimsL[4])/25.4}else{small.m <- 0}
    #Margin: 2inch+small,3cm in inch, 500Pixels in inch for top, 1.0cm in inch
    outermargin   <- c((2+small),(2.8/2.54),1.6667,(1.2/2.54))
    outermargin.m <- c((1.5+small.m),0.5,1,0.5)
    # Title character expansion factor
    Tcex <- 1.5
    # Main textbody expension factor
    Mcex <- 1
    # Axis label expansion factor
    Acex <- 1.1

    Lines <- c(1.5,2.5,6.5,4.5,2.5,5.5,7,5,6.5,8,5,6.5,9.5,11,12.5,9.5)
    #A4_h <- (297/25.4)*300
    #A4_w <- (210/25.4)*300

    png.width    <- A4dimsP[5] # =DINA4 Portrait  @ Resolution dpi
    png.height   <- A4dimsP[4] # =DINA4 Portrait  @ Resolution dpi
    png.width.m  <- A4dimsP[4] # =DINA4 Landscape @ Resolution dpi
    png.height.m <- A4dimsP[5] # =DINA4 Landscape @ Resolution dpi
    if(General$output!="png"){
      png.width    <- png.width/Resolution               # inch
      # size of sample, size of frame
      png.height   <- png.height/Resolution              # inch
      # Size of multi plot
      png.width.m  <- png.width.m/Resolution             # inch
      png.height.m <- png.height.m/Resolution            # inch
      General$File.unit <- c("in")
      Resolution <- c("auto")
    }
  }
  ##############################################################################################
  # Calculation of time series for individual indices                                         #
  ##############################################################################################
  # Create data frames with Y - sequences + add individual data later in the loop -> output for
  # multiple plots
      Indices    <- as.data.frame(cbind(Oid,Sediment.Y,Core.Y))
      IndicesMAD <- as.data.frame(cbind(Oid,Sediment.Y,Core.Y))

  for (i in 1:nrow(Parameters)) {

    #i <- 1
    ############################################################################################
    #                           Load Data                                                      #
    ############################################################################################
    if(substr(Parameters[i,2],start=(nchar(Parameters[i,2])-3),stop=nchar(Parameters[i,2]))==".dat"){

      if (General$sub_type=="big"){
        s_type <- 2
        Bsub <- General$Bsub
      }else{
        s_type <- 3
        Bsub <- c(1,dims[8]-dims[7])
      }

        Data <- read.ENVI(filename=   Parameters[i,s_type],
                          headerfile= paste(substring(Parameters[i,s_type],
                                                      first=1,
                                                      last=nchar(Parameters[i,s_type],"chars")-3
                                                      ),"hdr", sep="")
                          )

        Data <- Data[(dims[3]+1):dims[4],Bsub[1]:Bsub[2]]

    }else{
      Data <- read.table(file=Parameters[i,2], header=TRUE, sep = ",", quote="\"")
      Data <- Data[,c(1,(i-nsi+1))]
      sta  <- closest_values(Data[,1],Sediment.Start,Zero=FALSE)
      sto  <- closest_values(Data[,1],Sediment.Stop,Zero=FALSE)

      # Find Na's in data and preserve them during approx
      NA_BIN <- which(is.na(Data[,2])==TRUE)
      if (length(NA_BIN)>0){
      # create vector of zeros and bind to data
      Data <- cbind(Data,rep.int(0,nrow(Data)))
      # Fill in 1's at location of NA's
      Data[NA_BIN,3] <- 1
      }
      #Approximate data
      Dataapprox <- approx(Data[sta:sto,1],Data[sta:sto,2], n=(nsub)) #-dims[3])
      if (length(NA_BIN)>0){
      # Approximate NA's (vector of one's)
      NA_BIN <- approx(Data[sta:sto,1],Data[sta:sto,3], n=(nsub)) #-dims[3])
      NA_BIN1 <- which(NA_BIN$y>0)
      }
      #Write approximated data to DATA vector
      Data <- Dataapprox$y
      # Fill in approximated NA's
      if (length(NA_BIN)>0){Data[NA_BIN1] <- NA}
      # replicate Data for compatibility with usual routine (needs to calculate a mean from rows)
      Data <- cbind(Data,Data,Data)

    }

    Image  <- readTiff(Parameters[i,7],reduce=reduce)

    ############################################################################################
    #                     Calculated Parameters                                                #
    ############################################################################################

    # remove impossible Data
    ########################
      #  - Set infinite (INF) values to NA
      Data[!is.finite(Data)] <-NA # "!" means "not"

      if (length(cracks)==1){
        if (is.na(cracks)==FALSE) {
          for (j in 1:nrow(cracks)){Data[cracks[j,1]:cracks[j,2],] <- NA}
        }
      }else{for (j in 1:nrow(cracks)){Data[cracks[j,1]:cracks[j,2],] <- NA}}
    # Calculate means
    ##################
      # Calculate rowMeans (Sample)
      Mean.X <- rowMeans(Data, na.rm=TRUE)

      #Calculate column Mean (sample)
      Mean <- mean(Mean.X, na.rm=TRUE)

      # remove outliers
      MAD <- runmean(Mean.X, General$k,alg="C",endrule="mean")
      #MAD <- hampel(na.omit(Mean.X), k=General$k)
    #############################################################################################
    #                                      Export Data                                          #
    #############################################################################################

      # Export X-Y to *.csv (or *.xls) Header= Oid,Sid,Core Depth[mm],index value, MAD value
      ######################################################################################
      # Create Matrix with x-coordinate (Pixel) of original ENVI-File (Oid) and subset-file (Sid),
      # core position (core.Y), index value (Mean.X), and outlier corrected Mean (MAD)

      # Reverse na.omit function
      if(is.null(attr(MAD, "na.action"))==TRUE){MAD.X <- MAD
      }else{MAD.X <- rep(NA,length(MAD)+length(attr(MAD,"na.action")))
            MAD.X[-attr(MAD,"na.action")] <- MAD
            }
      MeanMAD <- mean(MAD.X, na.rm=TRUE)
      # data frame with output data -> single output
      mat     <- cbind(Oid,Sediment.Y,Core.Y,Mean.X,MAD.X)
      # Fill matrix for multiplot
      Indices    <- as.data.frame(cbind(Indices,   Mean.X))
      IndicesMAD <- as.data.frame(cbind(IndicesMAD,MAD.X))

      # Where are NA?s in the Data
      NA_BIN <- which(is.na(mat[,5])==TRUE)
      # Where are values > 3* std dev
      MAX <- which(mat[5]>(3*sd(mat[,5])))

      # Create a .csv file using the original filename that stores the Header
      Head <- c("Oid",
                "Sediment Depth[mm]",
                "Core Depth [mm]",
                Parameters[i,1],
                "Moving Average"
                )

      write.table(t(Head),
                  file=paste(Parameters[i,8],".csv", sep=""),
                  append = FALSE,
                  quote = FALSE,
                  sep = ",",
                  na = "NA",
                  dec = ".",
                  row.names = FALSE,
                  col.names = FALSE,
                  qmethod = c("escape", "double"),
                  fileEncoding = "UTF-8"
                  )

      # write Data into the same .csv file
      write.table(mat,
                  file=paste(Parameters[i,8],".csv", sep=""),
                  append = TRUE,
                  quote = FALSE,
                  sep = ",",
                  na = "NA",
                  dec = ".",
                  row.names = FALSE,
                  col.names = FALSE,
                  qmethod = c("escape", "double"),
                  fileEncoding = "UTF-8"
                  )

    #############################################################################################
    #                                  Export SINGLE Plots to png                               #
    #############################################################################################
    subnames <- c("(uncorrected)","Moving average - Filter, k=")
    subnames <- gsub("k=",paste("k=",General$k,sep=""),subnames,fixed=TRUE)

    if (graphout[1]==TRUE){
    if (Highres==TRUE){
      tempnames <- c(paste("_hires.",General$output,sep=""),
                     paste("_MA_hires.",General$output,sep=""))
    }else{
      tempnames <- c(paste(".",General$output,sep=""),
                     paste("_MA.",General$output,sep=""))
    }

    for(j in 1:2){
      # single plots
      Cairo(file=file.path(Parameters[i,8],tempnames[j], fsep=""),
           width=     png.width,
           height=    png.height,
           units=     General$File.unit,
           type=      General$output,
           bg=        General$Background,
           dpi=       Resolution
           )

      layout(matrix(c(1,2,3), 1, 3, byrow = TRUE),widths=lwidth)
      #layout.show(3)

      par(mar     =c(0,0,0,0),
          omi     =outermargin,
          yaxs    ="i", # this extents the y axis to the boundaries of the box
          mgp     =c(2,.3,0),
          ps      =Chars,
          las     =1
          )
      if(General$output=="svg"){
        plot(runif(nsub),Sediment.Y,type="n",axes=FALSE)
      }else{
      plot (Photo[round(dims[3]*(1-reduce)):round(dims[4]*(1-reduce)),(Tiff.x[1]:Tiff.x[2])])
      }
      box(which = "plot", lty = "solid")
      if(General$output=="svg"){
        plot(runif(nsub),Sediment.Y,type="n",axes=FALSE)
      }else{
      plot (Image[round(dims[3]*(1-reduce)):round(dims[4]*(1-reduce)),(Tiff.x[1]:Tiff.x[2])])
      abline(v=(General$Bsub[1]*(1-reduce)), untf=FALSE,col="red",lwd="1.5")
      abline(v=(General$Bsub[2]*(1-reduce)), untf=FALSE,col="red", lwd="1.5")
      }
      box(which = "plot", lty = "solid")

      par(tcl=-0.2)
      plot (mat[,(3+j)], -mat[,2],
            type=General$Line.style,
            axes=FALSE,
            col=Parameters[i,4]
            )
      axis(side=1,labels=T, outer=T, mgp=c(2,0.1,0),cex.axis=Acex)
      axis(side=2,labels=yt1,at=-yt2, outer=T,cex.axis=Acex)
      axis(side=4,labels=yt2,at=-yt2, outer=T,cex.axis=Acex)
      magaxis(majorn=(length(yt)*2), minorn=5, side=c(2,4), prettybase=prettyb, labels=FALSE)
      magaxis(side=c(1,3), prettybase=10, labels=FALSE)

      box(which = "plot", lty = "solid")
      title(xlab=General$X.label,line=Lines[1], adj=0.75, cex.lab=Acex, outer=T)
      #title(ylab=General$Y.label1, line=Lines[2], adj=0.5, cex.lab=Acex, outer=T)
      mtext(General$Y.label1, side=2, line=Lines[2], adj=0.5, outer=T, cex=Acex, las=0)
      mtext(General$Y.label2, side=4, line=Lines[2], adj=0.5, outer=T, cex=Acex, las=0)
      abline(v=Mean, untf = FALSE, col=General$Mean.color)
      abline(v=Mean-sd(Mean.X, na.rm=TRUE), untf = FALSE, col=General$Sd.color)
      abline(v=Mean+sd(Mean.X, na.rm=TRUE), untf = FALSE, col=General$Sd.color)

      mtext(Parameters[i,5], side=3, line=Lines[3], adj=0.5, outer=T, cex=Tcex)
      mtext(subnames[j], side=3, line=Lines[4], adj=0.5, outer=T, cex=(Tcex-0.3))
      mtext(Parameters[i,6],
            side=3, line=Lines[5], adj=0.5, outer=T, cex=(Tcex-0.3))
      mtext(General$Lake.name,
            side=1, line=Lines[6], adj=0.5, outer=T, cex=Mcex, font=2)
      mtext(paste("Core:",General$Core.name,sep=" "),
            side=1, line=Lines[7], adj=0.5, outer=T, cex=Mcex, font=2)

      mtext(paste("n = ", nsub, sep=""),
            side=1, line=Lines[8], adj=1, outer=T, cex=Mcex)
      mtext(paste("sd = ", round(sd(mat[,4], na.rm=TRUE), digits=3), sep=""),
            side=1, line=Lines[9], adj=1, outer=T, col=General$Sd.color, cex=Mcex)
      mtext(paste("Mean = ", round(Mean, digits=3), sep=""),
            side=1, line=Lines[10], adj=1, outer=T, col=General$Mean.color, cex=Mcex)
      mtext(paste("Start = ", round(Sediment.Start, digits=2)," mm", sep=""),
            side=1, line=Lines[11], adj=0, outer=T, cex=Mcex)
      mtext(paste("End = ", round(Sediment.Stop, digits=2)," mm", sep=""),
            side=1, line=Lines[12], adj=0, outer=T, cex=Mcex)

      if(Core.length==Subset.Length & Core.length==Sediment.Length){
        mtext(paste("Core length [mm] = ", round(Core.length, digits=2), sep=""),
            side=1, line=Lines[13], adj=1, outer=T, cex=Mcex)
      }else{

      mtext(paste("Core length [mm] = ", round(Core.length, digits=2), sep=""),
            side=1, line=Lines[13], adj=1, outer=T, cex=Mcex)
      mtext(paste("Subset length [mm] = ", round(Subset.Length, digits=2), sep=""),
            side=1, line=Lines[14], adj=1, outer=T, cex=Mcex)
      mtext(paste("Sediment length [mm] = ", round(Sediment.Length, digits=2), sep=""),
            side=1, line=Lines[15], adj=1, outer=T, cex=Mcex)
      }
      mtext(current.date,
            side=3, line=Lines[16], adj=1, outer=T, cex=Acex)

      dev.off()

      info <- sprintf("%d%% done", round((((i-1)*4)+j)/((nrow(Parameters)*4)+2)*100))
      setWinProgressBar(.pb, round((((i-1)*4)+j)/((nrow(Parameters)*4)+2)*100), label=info)

    }
    if(General$output=="svg"){
      if(Highres==TRUE){ext <- c("_index_hires.png")}else{ext <- c("_index.png")}
      Cairo(file=file.path(Parameters[i,8],ext, fsep=""),
            width=     Tiff.x[2]-Tiff.x[1],
            height=    round(nsub*(1-reduce)),
            units=     "px",
            type=      c("png"),
            bg=        General$Background,
            dpi=       Resolution
            )

      par(mar     =c(0,0,0,0),
          omi     =c(0,0,0,0))
      plot (Image[round(dims[3]*(1-reduce)):round(dims[4]*(1-reduce)),(Tiff.x[1]:Tiff.x[2])])
      dev.off()
    }
    }
    rm(list=c("tempnames","j"))
   ###############################################################################################
   #                                  Export Overplot Plots to png                               #
   ###############################################################################################
   if (graphout[3]==TRUE){
   if (Highres==TRUE){
    if(General$output=="png"){
     tempnames <- c(paste("_oplot.",General$output,sep=""),
                     paste("_MA_oplot.",General$output,sep=""))

      for(j in 1:2){
        # Overplot - uncorrected
        png(filename = file.path(Parameters[i,8],tempnames[j], fsep=""),
             width   = pmx+ceil(2*0.5*Resolution),
             height  = png.height,
             units   = General$File.unit,
             type    = "windows",
             bg      = General$Background,
             res     = Resolution
             )

        par(mar     =c(0,0,0,0),
            omi     =outermargin,
            yaxs    ="i", # this extents the y axis to the boundaries of the box
            mgp     =c(2,.3,0),
            ps      =Chars,
            las     =1
            )

        plot (Photo[round(dims[3]):round(dims[4]),])
        box(which = "plot", lty = "solid")
        abline(v=General$Bsub[1],untf=FALSE,col="red",lwd="1.5")
        abline(v=General$Bsub[2], untf=FALSE,col="red", lwd="1.5")

        par(ps=Chars,
            tcl=-0.2,
            new=T)
        plot (mat[,j+3], -mat[,2],
              type=General$Line.style,
              axes=FALSE,
              col="white",
              lwd="1.5"
              )
        par(ps=Chars,
            tcl=-0.2,
            new=T,
            omi=outermargin
            )
        plot (mat[,j+3], -mat[,2],
              type=General$Line.style,
              axes=FALSE,
              col=Parameters[i,4],
              lwd="0.5"
              )
        axis(side=1,labels=T, outer=T, mgp=c(2,0.1,0),cex.axis=1.5)
        axis(side=2,labels=yt1,at=-yt2, outer=T,cex.axis=1.5)
        axis(side=4,labels=yt2,at=-yt2, outer=T,cex.axis=1.5)
        magaxis(majorn=(length(yt)*2), minorn=5, side=c(2,4), prettybase=prettyb, labels=FALSE,tcl=-0.2)
        magaxis(side=c(1,3), prettybase=10, labels=FALSE,tcl=-0.2)
        box(which = "plot", lty = "solid")
        title(xlab=General$X.label,line=1.0, adj=0.5, cex.lab=1.5, outer=T)
        #title(ylab=General$Y.label1, line=1.5, adj=0.5, cex.lab=1.5, outer=T)
        mtext(General$Y.label1, side=2, line=1.5, adj=0.5, outer=T, cex=Acex, las=0)
        mtext(General$Y.label2, side=4, line=1.5, adj=0.5, outer=T, cex=Acex, las=0)
        abline(v=Mean, untf = FALSE, col="gray80")
        abline(v=Mean-sd(Mean.X, na.rm=TRUE), untf = FALSE, col=General$Sd.color)
        abline(v=Mean+sd(Mean.X, na.rm=TRUE), untf = FALSE, col=General$Sd.color)

        mtext(Parameters[i,5], side=3, line=3.0, adj=0.5, outer=T, cex=Tcex)
        mtext(subnames[j],side=3, line=2.0, adj=0.5, outer=T, cex=(Tcex-0.2))
        mtext(Parameters[i,6],
              side=3, line=1.0, adj=0.5, outer=TRUE, cex=(Tcex-0.2))
        mtext(General$Lake.name,
              side=1, line=2, adj=0.5, outer=TRUE, cex=1, font=2)
        mtext(paste("Core:",General$Core.name,sep=" "),
              side=1, line=2.5, adj=0.5, outer=TRUE, cex=1, font=2)

        mtext(paste("n = ", nsub, sep=""),
              side=1, line=3, adj=0.5, outer=TRUE, cex=1)
        mtext(paste("sd = ", round(sd(mat[,4], na.rm=TRUE), digits=3), sep=""),
              side=1, line=3.5, adj=0.5, outer=TRUE, col=General$Sd.color, cex=1)
        mtext(paste("Mean = ", round(Mean, digits=3), sep=""),
              side=1, line=4, adj=0.5, outer=TRUE, col="gray80", cex=1)
        mtext(paste("Start = ", round(Sediment.Start, digits=2)," mm", sep=""),
              side=1, line=4.5, adj=0.5, outer=TRUE, cex=1)
        mtext(paste("End = ", round(Sediment.Stop, digits=2)," mm", sep=""),
              side=1, line=5, adj=0.5, outer=TRUE, cex=1)

        if(Core.length==Subset.Length & Core.length==Sediment.Length){
          mtext(paste("Core length [mm] = ", round(Core.length, digits=2), sep=""),
              side=1, line=5.5, adj=0.5, outer=TRUE, cex=1)
        }else{

        mtext(paste("Core length [mm] = ", round(Core.length, digits=2), sep=""),
              side=1, line=5.5, adj=0.5, outer=TRUE, cex=1)
        mtext(paste("Subset length [mm] = ", round(Subset.Length, digits=2), sep=""),
              side=1, line=6, adj=0.5, outer=TRUE, cex=1)
        mtext(paste("Sediment length [mm] = ", round(Sediment.Length, digits=2), sep=""),
              side=1, line=6.5, adj=0.5, outer=TRUE, cex=1)
        }
        mtext(current.date,
              side=3, line=4, adj=0.5, outer=TRUE, cex=1)

        dev.off()
        info <- sprintf("%d%% done", round((((i-1)*4)+(j+2))/((nrow(Parameters)*4)+2)*100))
        setWinProgressBar(.pb, round((((i-1)*4)+(j+2))/((nrow(Parameters)*4)+2)*100), label=info)
      }# concludes overplot
    }else{print("overplot not supported for svg output")}# concludes if clause ("png")
   }# concludes Highres/lowres of overplot
   }# concludes if graphout=TRUE
  }# concludes cycle through parameters
    rm=(list=c("i","Mean.X","MAD.X","Tiff.x","j","mat","Data","png.height","png.width","tempnames","Head"))
    ###############################################################################################
    ###############################################################################################
    #                                     Multiple Graphs                                         #
    ###############################################################################################
    if(nrow(Parameters)>1){
      if (graphout[2]==TRUE){
        if(General$output=="png"){
      #############################################################################################
      #                           Load Data                                                       #
      #############################################################################################
      # Create data frames with Y - sequences and add Data to these dfs
      #Indices    <- as.data.frame(cbind(Oid,Sediment.Y,Core.Y))
      #IndicesMAD <- as.data.frame(cbind(Oid,Sediment.Y,Core.Y))

      #for(i in 1:nrow(Parameters)) {
       # G <- read.table(file=paste(Parameters[i,8], ".csv", sep=""),
        #                                                header=TRUE,
         #                                               sep=";",
          #                                              dec=".",
           #                                             encoding="UTF-8",
            #                                            )
        #Indices[,3+i] <- G[,4]
        #IndicesMAD[,3+i] <- G[,5]
      #}
      colnames(Indices) <- c("Oid","Sediment.Y [mm]","Core.Y [mm]",t(Parameters[,1]))
      colnames(IndicesMAD) <- c("Oid","Sediment.Y [mm]","Core.Y [mm]",t(Parameters[,1]))

      ###############################################################################################
      #                                      Export Data                                            #
      ###############################################################################################

      write.table(Indices,
                  file=file.path(PATH$Index,"Results\\",General$Core.name,"_uncorrected.csv",fsep=""),
                  append = FALSE,
                  quote = FALSE,
                  sep = ";",
                  na = "NA",
                  dec = ".",
                  row.names = FALSE,
                  col.names = TRUE,
                  qmethod = c("escape", "double"),
                  fileEncoding = "UTF-8"
                  )
      write.table(IndicesMAD,
                  file=file.path(PATH$Index,"Results\\",General$Core.name,"_MA.csv",fsep=""),
                  append = FALSE,
                  quote = FALSE,
                  sep = ";",
                  na = "NA",
                  dec = ".",
                  row.names = FALSE,
                  col.names = TRUE,
                  qmethod = c("escape", "double"),
                  fileEncoding = "UTF-8"
                  )

    ###############################################################################################
    #                                       Plot data                                             #
    ###############################################################################################

    if (Highres==TRUE){
      tempnames <- c(paste("_hires.",General$output,sep=""),
                     paste("_MA_hires.",General$output,sep=""))
    }else{
      tempnames <- c(paste(".",General$output,sep=""),
                     paste("_MA.",General$output,sep=""))
    }
    Sub.Main.title.m <- c(General$Sub.Main.title.m, General$Sub.Main.title.MAD.m)


    Indexcount   <- nrow(Parameters)
    Layoutmatrix <- matrix(t(c(seq(from=1, to=Indexcount+1))),
                           nrow=1,
                           ncol=Indexcount+1,
                           byrow=TRUE
    )
    #l <- 1 # debug
    for(l in 1:2){
      if(l==2){Indices <- IndicesMAD}
      png(filename    = file.path(PATH$Index,"Results\\",General$Core.name,tempnames[l], fsep=""),
           width      = png.width.m,
           height     = png.height.m,
           units      = General$File.unit,
           #pointsize = Point.size,
           #type      = General$output,
           bg         = General$Background,
           res        = Resolution
           )

      layout(Layoutmatrix, widths=c(rep(1,Indexcount+1)))
      #layout.show()


      par(mar  = c(0,0,0,0),
          mai  = c(0,0,0.8,0),
          omi  = outermargin.m,
          yaxs = "i",
          xaxs = "r",
          mgp  = c(2,.3,0),
          ps   = Chars.m,
          tcl  = -0.2,
          las  = 1
          )


      plot (Indices[,4],-Indices[,2],
            type=General$Line.style,
            axes=F,
            col =Parameters[1,4]
      )
      title(main=Parameters[1,1], line=2, outer=F)
      axis(side=2, outer=T,labels=yt1,at=-yt2, line=0, cex.axis=Acex)
      axis(side=1, cex.axis=Acex, outer=T)
      magaxis(majorn=(length(yt)*2), minorn=5, side=c(2), prettybase=prettyb, labels=FALSE)
      magaxis(side=c(1), prettybase=10, labels=FALSE)

      mtext(General$Y.label1.m, side=2, line=2.5, adj=0.5, outer=T, cex=Acex, las=0)

      abline(v=mean(Indices[,4], na.rm=TRUE), untf = FALSE, col=General$Mean.color)
      abline(v=mean(Indices[,4], na.rm=TRUE) - sd(Indices[,4], na.rm=TRUE),
             untf = FALSE, col=General$Sd.color)
      abline(v=mean(Indices[,4], na.rm=TRUE) + sd(Indices[,4], na.rm=TRUE),
             untf = FALSE, col=General$Sd.color)
      box(which = "plot", lty = "solid")


      for(i in 1:ceiling((Indexcount-2)/2)){

        plot (Indices[,4+i],-Indices[,2],
                 type = General$Line.style,
                 axes = F,
                 col  = Parameters[1+i,4]
        )
        title(main=Parameters[1+i,1], line=2, outer=F)
        if(is.even(i)){axis(side=1, cex.axis=Acex, outer=T)
                       magaxis(side=c(1), prettybase=10, labels=FALSE)
        }else{
        axis(side=3, cex.axis=Acex, outer=F)
        magaxis(side=c(3), prettybase=10, labels=FALSE)
        }
        abline(v=mean(Indices[,4+i], na.rm=TRUE), untf = FALSE, col=General$Mean.color)
        abline(v=mean(Indices[,4+i], na.rm=TRUE) - sd(Indices[,4+i], na.rm=TRUE),
               untf = FALSE, col=General$Sd.color)
        abline(v=mean(Indices[,4+i], na.rm=TRUE) + sd(Indices[,4+i], na.rm=TRUE),
               untf = FALSE, col=General$Sd.color)
        box(which = "plot", lty = "solid")
      }
      if(General$output=="svg"){
        plot(runif(nsub),Sediment.Y,type="n",axes=FALSE)
      }else{
      plot(Photo.m[round(dims[3]*(1-reduce.m)):round(dims[4]*(1-reduce.m)),(Tiff.x.m[1]:Tiff.x.m[2])])
      }
      abline(v=(General$Bsub[1]*(1-reduce.m)), untf=FALSE,col="red", lwd="1.5")
      abline(v=(General$Bsub[2]*(1-reduce.m)), untf=FALSE,col="red", lwd="1.5")
      title(main=paste(General$Tiffmode[1]," image",sep=""), line=2, outer=F)
      box(which = "plot", lty = "solid")

      if((Indexcount-2)<(i+1)) {
        plot (Indices[,ncol(Indices)],-Indices[,2],
            type=General$Line.style,
            axes=F,
            xlab=General$X.label.m,
            ylab="",
            col =Parameters[Indexcount,4]
      )
      title(main=Parameters[Indexcount,1], line=2, outer=F)
      axis(side=1, cex.axis=Acex, outer=F)
      axis(side=4, outer=T,labels=yt2,at=-yt2, line=0, cex.axis=Acex)
      magaxis(majorn=(length(yt)*2), minorn=5, side=c(4), prettybase=prettyb, labels=FALSE)
      magaxis(side=c(1), prettybase=10, labels=FALSE)
      mtext(General$Y.label2.m, side=4, line=2.5, adj=0.5, outer=T, cex=Acex, las=0)

      abline(v=mean(Indices[,ncol(Indices)], na.rm=TRUE), untf = FALSE, col=General$Mean.color)
      abline(v=mean(Indices[,ncol(Indices)], na.rm=TRUE) - sd(Indices[,ncol(Indices)], na.rm=TRUE),
             untf = FALSE, col=General$Sd.color)
      abline(v=mean(Indices[,ncol(Indices)], na.rm=TRUE) + sd(Indices[,ncol(Indices)], na.rm=TRUE),
             untf = FALSE, col=General$Sd.color)
      box(which = "plot", lty = "solid")

      }else{
        for(j in (i+1):(Indexcount-2)){

          plot (Indices[,4+j],-Indices[,2],
                type=General$Line.style,
                axes=F,
                xlab=General$X.label.m,
                ylab="",
                col=Parameters[1+j,4]
          )
          title(main=Parameters[1+j,1], line=2, outer=F)
          if(is.even(j)){axis(side=1, cex.axis=Acex, outer=T)
                         magaxis(side=c(1), prettybase=10, labels=FALSE)
          }else{
          axis(side=3, cex.axis=Acex, outer=F)
          magaxis(side=c(3), prettybase=10, labels=FALSE)
          }
          abline(v=mean(Indices[,4+j], na.rm=TRUE), untf = FALSE, col=General$Mean.color)
          abline(v=mean(Indices[,4+j], na.rm=TRUE) - sd(Indices[,4+j], na.rm=TRUE),
                 untf = FALSE, col=General$Sd.color)
          abline(v=mean(Indices[,4+j], na.rm=TRUE) + sd(Indices[,4+j], na.rm=TRUE),
                 untf = FALSE, col=General$Sd.color)
          box(which = "plot", lty = "solid")
        }

        plot (Indices[,ncol(Indices)],-Indices[,2],
              type=General$Line.style,
              axes=F,
              xlab=General$X.label.m,
              ylab="",
              col =Parameters[Indexcount,4]
        )
        title(main=Parameters[Indexcount,1], line=2, outer=F)
        if(is.even(j)){axis(side=3, cex.axis=Acex, outer=F)
                       magaxis(side=c(3), prettybase=10, labels=FALSE)
          }else{
          axis(side=1, cex.axis=Acex, outer=T)
          magaxis(side=c(1), prettybase=10, labels=FALSE)
          }
        axis(side=4, outer=T,labels=yt2,at=-yt2, line=0, cex.axis=Acex)
        magaxis(majorn=(length(yt)*2), minorn=5, side=c(4), prettybase=prettyb, labels=FALSE)


        abline(v=mean(Indices[,ncol(Indices)], na.rm=TRUE), untf = FALSE, col=General$Mean.color)
        abline(v=mean(Indices[,ncol(Indices)], na.rm=TRUE) - sd(Indices[,ncol(Indices)], na.rm=TRUE),
               untf = FALSE, col=General$Sd.color)
        abline(v=mean(Indices[,ncol(Indices)], na.rm=TRUE) + sd(Indices[,ncol(Indices)], na.rm=TRUE),
               untf = FALSE, col=General$Sd.color)
        mtext(General$Y.label2, side=4, line=0, adj=0.5, outer=T,cex=Acex,las=0)
        box(which = "plot", lty = "solid")
      }

      mtext(General$Main.title.m,side=3, adj=0.5,line=3, outer=T, cex=Tcex)
      mtext(Sub.Main.title.m[l],side=3, adj=0.5, line=1, outer=T, cex=(Tcex-0.3))
      mtext(paste("x-axes = ",General$X.label.m, sep=""), side=1, line=3, adj=0.5, outer=T, cex=Acex)
      mtext(General$Sub.title.m, side=1, line=5.5, adj=0.5, outer=T, cex=1.2, font=2)
      mtext(General$Lake.name[1], side=1, line=8, adj=0.5, outer=T, cex=0.8, font=2)


      mtext(paste("n = ", nsub, sep=""), side=1, line=4, adj=1, outer=T)
      mtext(paste("length [mm] = ", round(Subset.Length, digits=2), sep=""), side=1, line=7.5,
            adj=1, outer=T)
      mtext(paste("Start = ", round(Sediment.Start, digits=2)," mm", sep=""),
            side=1, line=4, adj=0, outer=T)
      mtext(paste("End = ", round(Sediment.Stop, digits=2)," mm", sep=""),
            side=1, line=7.5, adj=0, outer=T)
      mtext(current.date, side=3, line=4, adj=1, outer=T)
      box(which = "plot", lty = "solid")
      #z <- par()
      dev.off()
      # Progress bar update
      info <- sprintf("%d%% done", round(((nrow(Parameters)*4)+l)/((nrow(Parameters)*4)+2)*100))
      setWinProgressBar(.pb, round(((nrow(Parameters)*4)+l)/((nrow(Parameters)*4)+2)*100), label=info)
    }
    }else{print("Multiplot is not supported for svg output")}#concludes is "png"?
    }#concludes graphout=TRUE
    }#concludes if Parameters>1
  if(General$output=="svg"){
    if(Highres==TRUE){ext <- c("_photo_hires.png")}else{ext <- c("_photo.png")}
    Cairo(file=file.path(PATH$Index,"Results\\",General$Core.name,ext, fsep=""),
          width=     Tiff.x[2]-Tiff.x[1]+1,
          height=    round(nsub*(1-reduce)),
          units=     "px",
          type=      c("png"),
          bg=        General$Background,
          res=       Resolution
        )
        par(mar     =c(0,0,0,0),
            omi     =c(0,0,0,0))
        plot (Photo[round(dims[3]*(1-reduce)):round(dims[4]*(1-reduce)),(Tiff.x[1]:Tiff.x[2])])
        dev.off()
  }

  close(.pb)
}
