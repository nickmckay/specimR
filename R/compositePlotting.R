
#' Title
#'
#' @param coreNames
#' @param append
#'
#' @return
#' @export
#'
#' @examples
getCompositeName <- function(coreNames,append = "Composite"){
compName <- coreNames[1]

for(i in 2:length(coreNames)){
  ta <- drop(attr(adist(compName, coreNames[i], counts=TRUE), "trafos"))
  compName <- stringr::str_sub(compName, stringr::str_locate(ta,"M+"))

}

return(stringr::str_c(compName,append))
}



#' Title
#'
#' @param dirPath
#'
#' @return
#' @export
#'
#' @examples
getPathsForCompositing <- function(dirPath=NA){
  if(is.na(dirPath)){
    cat(crayon::bold("Select the `processing_metadata.txt` within the Products directory of the core you want\n\n"))
    Sys.sleep(0.5)
    metaPath <- file.choose()
    dirPath <-  dirname(metaPath)
  }

  #load metadata
  meta <- readr::read_lines(metaPath)


  #try to find the overview, capture, white and dark ref paths
  paths <- list()

  paths$corename <- coreName <- meta[1]

  #normalized output RData
  op <- file.path(dirPath,"normalized.RData")

  if(file.exists(op)){
    paths$normOut <- op
  }else{
    cat(crayon::bold(crayon::red("Choose the 'normalized.RData' file\n\n")))
    paths$overview <-  file.choose()
  }

  #depth table
  ca <- file.path(dirPath,"depthTable.csv")

  if(file.exists(ca)){
    paths$depthTable <- ca
  }else{
    cat(crayon::bold(crayon::red("Choose the depthTable .csv file\n\n")))
    paths$depthTable <-  file.choose()
  }

  #index table
  si <- file.path(dirPath,"spectralIndices.csv")

  if(file.exists(ca)){
    paths$spectralIndices <- si
  }else{
    cat(crayon::bold(crayon::red("Choose the spectralIndicies .csv file\n\n")))
    paths$spectralIndices <-  file.choose()
  }

  #index table
  ph <- file.path(dirPath,"photos")

  if(dir.exists(ph)){
    paths$processedImageDir <- ph
  }else{
    cat(crayon::bold(crayon::red("Choose a file within the processedImage directory\n\n")))
    paths$processedImageDir <-  dirpath(file.choose())
  }

  return(paths)

}

#' Title
#'
#' @param dirs
#' @param out.dir
#' @inheritDotParams plotCompositeSpectralDashboard
#'
#' @return
#' @export
#'
#' @examples
compositeSections <- function(dirs = NA,out.dir = NA,composite.indices = c("RABD615","RABD660670","RABD845","R570R630","R590R690"),...){
  if(is.na(dirs)){
    nCores <- as.numeric(readline(prompt = cat(crayon::bold("How many sections do you want to composite?"))))
    if(nCores < 2){stop("No reason to composite fewer than 2 cores")}

    cat(crayon::red(glue::glue("Select the cores you want to composite IN ORDER FROM TOP TO BOTTOM...\n\n")))


    dirs <- vector(mode = "list", length = nCores)


    for(i in 1:nCores){
      dirs[[i]] <- getPathsForCompositing()
      if(i<nCores){
        cat(crayon::green(glue::glue("Core {i} of {nCores} selected. Moving on to next core...\n\n")))
      }else{
        cat(crayon::green(glue::glue("Core selection complete!\n\n")))
      }
      Sys.sleep(0.5)
    }
  }

  nCores <- length(dirs)
  allInd <- allNorm <- allDepth <-  vector(mode = "list",length = nCores)
  coreNames <- processedImageDirs <- c()
  #load in all data
  for(i in 1:nCores){
    load(dirs[[i]]$normOut)
    allNorm[[i]] <- normalizationOutput

    allInd[[i]] <- readr::read_csv(dirs[[i]]$spectralIndices)

    allDepth[[i]] <- readr::read_csv(dirs[[i]]$depthTable)

    coreNames[i] <- dirs[[i]]$corename

    processedImageDirs[i] <- file.path(dirs[[i]]$processedImageDir)

  }

  names(allDepth) <- coreNames

  #Create the core table
    coreTable <- createCoreDepthTable(allDepth)

  #Create the composite
    compName <- getCompositeName(coreNames)

    if(is.na(out.dir)){
      out.dir <- file.path(dirname(dirname(dirname(dirs[[1]]$normOut))),compName)
    }

    if(!dir.exists(out.dir)){
      dir.create(out.dir)
    }
    readr::write_csv(coreTable, file.path(out.dir,paste0(compName,"-core-depth-table.csv")))

#plot all the comp.indices

    allPlot <- plotCompositeSpectralDashboard(normList = allNorm,
                                   coreTable = coreTable,
                                   plot.width = 20,
                                   indices = allInd,
                                   index.name = composite.indices,
                                   output.file.path = file.path(out.dir,paste0(compName,"-allIndices-compositePlot.png")),
                                   processed.image.dirs = processedImageDirs,
                                   ...)

    if(length(composite.indices) > 1){#plot individuals
      for(ind in 1:length(composite.indices)){
      indPlot <- plotCompositeSpectralDashboard(normList = allNorm,
                                                coreTable = coreTable,
                                                indices = allInd,

                                                index.name = composite.indices[ind],
                                                output.file.path = file.path(out.dir,paste0(compName,"-",composite.indices[ind],"-compositePlot.png")),
                                                processed.image.dirs = processedImageDirs,
                                                ...)

    }
}
}


#' Plot composite spectral dashboard
#'
#' @param index.name
#' @param depth.label
#' @param plot.width
#' @param tol
#' @param normList a list of the normalized output lists from normalize()
#' @param coreTable core depth table
#' @param indices a list of spectralIndices tibbles
#' @param core.width
#' @param page.width
#' @param y.tick.interval
#' @param page.units
#' @param output.file.path
#' @param output.dpi
#'
#' @return
#' @export
#'
#' @examples
plotCompositeSpectralDashboard <- function(normList,
                                           coreTable,
                                           indices = NA,
                                           processed.image.dirs = NA,
                                           index.name = c("RABD615","RABD660670","RABD845","R570R630","R590R690"),
                                           depth.label = "Depth (cm)",
                                           core.width = 4,
                                           plot.width = 8,
                                           page.width = 10,
                                           y.tick.interval = 5,
                                           page.units = "cm",
                                           tol = 1,
                                           output.file.path = NA,
                                           output.dpi = 600){
  #make a composite plot

  #imagery first

  #get ROI info!
  for(ni in 1:length(normList)){

    normalized <- normList[[ni]]
    processed.image.dir = processed.image.dirs[ni]

    #get the big ROI
    load(file.path(processed.image.dir,"bigRoi.Rdata"))

    roi <- normalized$roi

    #decide how to crop it.
    xOffset <- min(bigRoi@xmin,roi@xmin)
    yOffset <- roi@ymin #use ROI exactly
    rightPos <- max(bigRoi@xmax,roi@xmax)
    topPos <- roi@ymax #use ROI exactly
    width <- rightPos-xOffset
    height <- topPos-yOffset

    #crop it based on the roi
    roi <- normalized$roi#roi relative to the whole scan

    #adjust for compositee depths!
    #get roi boundaries in cm
    cmRoiTibRow <- tibble::tibble(
      x = 0,
      y = 0,
      xmin = max(roi@xmin - xOffset + 1,1)*normalized$cmPerPixel,
      xmax = min(roi@xmax - xOffset + 1,rightPos)*normalized$cmPerPixel,
      ymin =  max(roi@ymin - yOffset + 1,1)*normalized$cmPerPixel+coreTable$compositeRoiTopDepth[ni],
      ymax = min(roi@ymax - yOffset + 1,topPos)*normalized$cmPerPixel+coreTable$compositeRoiTopDepth[ni])

    if(ni == 1){
      cmRoiTib <- cmRoiTibRow
    }else{
      cmRoiTib <- dplyr::bind_rows(cmRoiTib,cmRoiTibRow)
    }
  }

  for(ni in 1:length(normList)){

    normalized <- normList[[ni]]
    processed.image.dir =     processed.image.dir = processed.image.dirs[ni]
    #get the processed image path (want full png with scale so that ROI is in right spot)
    fullPath <- list.files(path = processed.image.dir,pattern = "fullImage*",full.names = TRUE)
    img <- magick::image_read(fullPath)

    #get the big ROI
    load(file.path(processed.image.dir,"bigRoi.Rdata"))

    roi <- normalized$roi

    #decide how to crop it.
    xOffset <- min(bigRoi@xmin,roi@xmin)
    yOffset <- roi@ymin #use ROI exactly
    rightPos <- max(bigRoi@xmax,roi@xmax)
    topPos <- roi@ymax #use ROI exactly
    width <- rightPos-xOffset
    height <- topPos-yOffset

    #crop it based on the roi
    roi <- normalized$roi#roi relative to the whole scan


  #now plot it!
  c.height.scale <- max(coreTable$compositeRoiBottomDepth)

  depth.ticks <- seq(0,c.height.scale,by = y.tick.interval)

    iroi <- magick::geometry_area(width = width,height = height, x_off = xOffset,y_off = yOffset)

    cimg <- magick::image_crop(img,geometry = iroi,gravity = "SouthWest")


    cinfo <- magick::image_info(cimg)
    c.width <- cinfo$width*normalized$cmPerPixel


    if(ni == 1){
      ggimg <- ggplot2::ggplot(cmRoiTib, ggplot2::aes_string("x","y")) +
        ggplot2::geom_blank() +
        ggplot2::coord_fixed(expand = FALSE, xlim = c(0, c.width),ylim = c(-c.height.scale,0)) +
        ggplot2::scale_y_continuous(depth.label,labels = rev(depth.ticks),breaks = -rev(depth.ticks))+
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank())
    }

    ggimg <- ggimg+
      ggplot2::annotation_raster(cimg, 0, c.width, -coreTable$compositeRoiBottomDepth[ni], -coreTable$compositeRoiTopDepth[ni], interpolate = FALSE)

  }

  ggimg <- ggimg +
    geom_rect(aes(xmin = xmin,
                  xmax = xmax,
                  ymin = -ymin,
                  ymax = -ymax),
              color = "red",
              fill = NA)



  # Plot indices! -----------------------------------------------------------

  for(ni in 1:length(normList)){
    normalized <- normList[[ni]]
    if(is.na(indices)){
    thisIndex <- readr::read_csv(file.path(normalized$outputDir,"spectralIndices.csv"))
    }else{
    thisIndex <- indices[[ni]]
    }
    #adjust depth if
    if(ni==1){
      indexTable <- thisIndex
    }else{
      thisIndex$depth <- thisIndex$depth+coreTable$compositeRoiTopDepth[ni]
      #add a row of NAs
      narow <- thisIndex[1,]
      narow[,] <- NA
      indexTable <- dplyr::bind_rows(indexTable,narow)
      indexTable <- dplyr::bind_rows(indexTable,thisIndex)
    }
  }
  ind <- indexTable

  plots <- vector(mode = "list",length = length(index.name)+1)
  plots[[1]] <- ggimg
  for(i in 1:length(index.name)){
    #get colors by index
    cols <- getColorsByIndex(index.name[i])
    # make a line plot
    # line plot
    plots[[i+1]] <- plotVerticalIndex(ind,index.name = index.name[i],line.color = cols$smooth,smooth.color = cols$smooth,smooth.width = 0)+scale_x_continuous(sec.axis = dup_axis())

    if(i<length(index.name)){
      plots[[i+1]] <- plots[[i+1]] +   theme(axis.title.y=element_blank(),
                                             axis.text.y=element_blank(),
                                             axis.ticks.y=element_blank())
    }else{
      plots[[i+1]] <- plots[[i+1]] +
        scale_y_reverse("Depth (cm)",position = "right",expand = c(0,0),breaks = rev(depth.ticks))+
        theme(axis.title.y.right = element_text(angle = 90))
    }
  }
  # #make a heatmap ------ None for now

  # for(ni in 1:length(normList)){
  #   normalized <- normList[[ni]]
  #   secDepth <- section2compositeDepth(coreTable = coreTable,core.section = names(normList)[ni],depths = normalized$scaleY)
  #   thisHeatMap <- makeHeatmap(normalized, index = index.name[i],tol = tol) %>%
  #     plotHeatmap(depthScale = secDepth,cmPerPixel = normalized$cmPerPixel,palette = cols$palette) +
  #     theme(axis.title.y=element_blank(),
  #           axis.text.y=element_blank(),
  #           axis.ticks.y=element_blank(),
  #           panel.background = element_blank(),
  #           plot.margin=unit(c(1,-.5,1,-0.5), "cm"))
  #   if(ni == 1){
  #     bigHeatMap <- thisHeatMap
  #   }else{
  #     bigHeatMap <- bigHeatMap + thisHeatMap
  #   }
  #
  # }

  rel.widths <- c(core.width,rep(plot.width,times = length(index.name)))
  widths <- unit(rel.widths/sum(rel.widths)*page.width,units = page.units)
  page.length <- rel.widths[1]/sum(rel.widths)*page.width*c.height.scale/c.width

  #egg
  outplot <- egg::ggarrange(plots = plots,nrow = 1,widths = widths,padding = 0,draw = FALSE,clip = "on")

  if(!is.na(output.file.path)){
    ggsave(plot = outplot,
           filename = output.file.path,
           width = page.width*1.8,
           height = page.length*1.8,
           units = page.units,dpi = output.dpi,
           limitsize = FALSE)
  }

  return(outplot)

}
