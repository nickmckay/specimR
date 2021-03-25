
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
spectralWorkflow <- function(indices = c("RABD615","RABD660670","RABD845","R570R630","R590R690"),
                             overall.width = 5,
                             individual.width = 30,
                             width.mult = 4,
                             image.wavelengths = c(630,532,465),
                             imageRoi = NA,
                             ...){
  indicesString <- paste0("indices = c(",paste(paste0('"',indices,'"'),collapse = ','),")")
  owString <- glue::glue("overall.width = {overall.width}")
  iwString <- glue::glue("individual.width = {individual.width}")
  wmString <- glue::glue("width.mult = {width.mult}")

  normList <- normalize(...)

  cat(crayon::bold(glue::glue("Creating images...")))

  #check for the multi ROI
if(length(normList)==1){
  image.dir <- file.path(normList[[1]]$outputDir,"photos")
}else{
  image.dir <- file.path(dirname(normList[[1]]$outputDir),"photos")
}
  #create images


  createImages(directory = normList[[1]]$inputDir,
               wavelengths = image.wavelengths,
               image.output.dir = file.path(normList[[1]]$outputDir,"photos"),
               bigRoi = imageRoi)

  cat(crayon::bold(glue::glue("Creating figures...")))

  #loop through ROIs
  for(n in 1:length(normList)){
  normalized <- normList[[n]]
  #calculate indices
  indexTable <- calculateIndices(normalized,indices = indices)

  #write indices to a csv file
  readr::write_csv(indexTable,file.path(normalized$outputDir,"spectralIndices.csv"))


  #plot dashboards
  overall <- plotSpectralDashboard(normalized,
                                   indexTable,
                                   processed.image.dir = image.dir,
                                   index.name = indices,
                                   width.mult = width.mult,
                                   plot.width = overall.width)

  totalDepth <- max(indexTable$depth)

  #create png
  ggsave(plot = overall,
         filename = file.path(normalized$outputDir,"allIndices.png"),
         width = 20,
         height = totalDepth,
         units = "cm",
         limitsize = FALSE)

  #individual indices
  for(i in indices){
    #plot dashboards
    this <- plotSpectralDashboard(normalized,indexTable,index.name = i,width.mult = width.mult,plot.width = individual.width,processed.image.dir = image.dir)

    #png
    ggsave(plot = this,
           filename = file.path(normalized$outputDir,paste0(i,".png")),
           width = 10,
           height = totalDepth,
           units = "in",limitsize = FALSE)
  }

  #write command to reproduce this
  rep.command <- glue::glue("specimR::spectralWorkflow({indicesString},
                          {normParams},
                          {owString},
                          {iwString},
                          {wmString})")

  readr::write_file(rep.command,file.path(normalized$outputDir,"reprocess.R"))
  }
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
