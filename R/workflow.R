
#' Wrapper function that goes through standardization, index calculation and visualization
#'
#' @param indices
#' @param overall.width
#' @param individual.width
#' @param width.mult
#' @inheritDotParams normalize
#'
#' @return
#' @export
#'
#' @examples
spectralWorkflow <- function(directory = NA,
                             indices = c("RABD615","RABD660670","RABD845","R570R630","R590R690"),
                             overall.page.width = 30,
                             individual.page.width = 10,
                             plot.width = 8,
                             core.width = 4,
                             image.wavelengths = c(630,532,465),
                             imageRoi = NA,
                             output.dir = NA,
                             clickDepths = NA,
                             overall.width = NA,
                             individual.width = NA,
                             width.mult = NA,
                             ...){
  indicesString <- paste0("indices = c(",paste(paste0('"',indices,'"'),collapse = ','),")")
  owString <- glue::glue("overall.page.width = {overall.page.width}")
  iwString <- glue::glue("individual.page.width = {individual.page.width}")
  wmString <- glue::glue("core.width = {core.width}")
  pwString <- glue::glue("plot.width = {plot.width}")


  if(all(is.na(clickDepths))){
    assign("clickDepths",NULL,envir = .GlobalEnv)
  }else{
    assign("clickDepths",clickDepths,envir = .GlobalEnv)
  }
  # #print that you need to pick it.
  # if(is.na(directory)){
  #   cat(crayon::bold("Choose a file within the Specim core directory\n\n"))
  #   Sys.sleep(1)
  # }

  #get the appropriate paths
  paths <- getPaths(dirPath = directory)
  directory <- dirname(paths$overview)


  #output directory handling
  if(is.na(output.dir)){
    output.dir <- file.path(dirname(paths$overview),"products")
  }


  if(is.na(imageRoi)){
    overviewPng <- imager::load.image(paths$overview)

    gs <- imager::grayscale(overviewPng) %>% as.matrix()

    across <- apply(gs,1,mean)
    down <- apply(gs,2,mean)

    cropVert <- findCropEdges(rev(down))
    cropHor <- findCropEdges(across)

    bigRoiTry <- raster::extent(cropHor[1],cropHor[2],cropVert[1],cropVert[2])

    #check to see if the big ROI is good (new shiny app)
    overview <- raster::brick(paths$overview)
    imageRoi <- pick_big_roi_shiny(overview,bigRoiTry, zh = nrow(overview)/5)

    imageRoi@xmin <- ceiling(imageRoi@xmin)
    imageRoi@ymin <- ceiling(imageRoi@ymin)
    imageRoi@xmax <- floor(imageRoi@xmax)
    imageRoi@ymax <- floor(imageRoi@ymax)
  }

  irs <- glue::glue("imageRoi = raster::extent(matrix(c({imageRoi@xmin},{imageRoi@xmax},{imageRoi@ymin},{imageRoi@ymax}),nrow = 2,byrow = T))")

  normList <- normalize(directory = directory,output.dir = output.dir,...)

  #create images
  cat(crayon::bold(glue::glue("Loading data to creating images...\n\n")))

  image.dir <- file.path(output.dir,"photos")

  createImages(directory = directory,
               wavelengths = image.wavelengths,
               image.output.dir = image.dir,
               bigRoi = imageRoi)

  cat(crayon::bold(glue::glue("Creating figures...\n\n")))

  #loop through ROIs
  for(n in 1:length(normList)){
  normalized <- normList[[n]]
  #calculate indices
  indexTable <- calculateIndices(normalized,indices = indices)

  #write indices to a csv file
  readr::write_csv(indexTable,file.path(normalized$outputDir,"spectralIndices.csv"))


  #plot dashboards
  overall <- suppressMessages(plotSpectralDashboard(normalized,
                                   indexTable,
                                   processed.image.dir = image.dir,
                                   index.name = indices,
                                   core.width = core.width,
                                   page.width = overall.page.width,
                                   plot.width = plot.width,
                                   output.file.path = file.path(normalized$outputDir,"allIndices.png"),
                                   output.dpi = 600))

  #individual indices
  for(i in indices){
    #plot dashboards
    this <- suppressMessages(plotSpectralDashboard(normalized,
                                  indexTable,
                                  index.name = i,
                                  core.width = core.width,
                                  processed.image.dir = image.dir,
                                  page.width = individual.page.width,
                                  plot.width = plot.width,
                                  output.file.path = file.path(normalized$outputDir,paste0(i,".png")),
                                  output.dpi = 600))
  }

  #write command to reproduce this
  rep.command <- glue::glue("specimR::spectralWorkflow({indicesString},
                          {normParams},
                            {irs},
                          {owString},
                          {iwString},
                            {pwString},
                          {wmString})")

  readr::write_file(rep.command,file.path(output.dir,"reprocess.R"))
  }
  rm("clickDepths",envir = .GlobalEnv)

}


#' Plot spectral output
#'
#' @param normalized The output of normalize()
#' @param indices Which indices to plot? see ?calculateIndices for options
#' @param file.type Export file type (default = "png")
#' @param fig.width Width of the overview plot in fig.units (individual plots will be half as wide) (default = 20)
#' @param fig.length Length of the overview plot in the fig.units, if NA, will use core length (default = NA)
#' @param fig.units Units for fig dimensions (default = "in"s)
#' @inheritDotParams plotSpectralDashboard
#' @export
plotSpectralOutput <- function(normalized,
                               indices = c("RABD615","RABD660670","RABD845","R570R630","R590R690"),
                               file.type = "png",
                               fig.width = 20,
                               fig.length = NA,
                               fig.units = "in",
                               ...){


  #calculate indices
  indexTable <- calculateIndices(normalized,indices = indices)


  #plot dashboards
  overall <- plotSpectralDashboard(normalized,indexTable,index.name = indices,...)

  if(is.na(fig.length)){#use the depth
    totalDepth <- max(normalized$scaleY)
  }else{
    totalDepth <- fig.length
  }

  #create file
  ggsave(plot = overall,
         filename = file.path(normalized$outputDir,paste0("allIndices.",file.type)),
         width = fig.width,
         height = totalDepth,
         units = fig.units,
         limitsize = FALSE)

  #individual indices
  for(i in indices){
    #plot dashboards
    this <- plotSpectralDashboard(normalized,indexTable,index.name = i,...)

    #png
    ggsave(plot = this,
           filename = file.path(normalized$outputDir,paste0(i,".",file.type)),
           width = fig.width/2,
           height = totalDepth,
           units = fig.units,
           limitsize = FALSE)
  }

}
