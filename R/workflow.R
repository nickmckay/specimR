
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
                             overall.width = 3,
                             individual.width = 2,
                             width.mult = 2,
                             ...){
  indicesString <- paste0("indices = c(",paste(paste0('"',indices,'"'),collapse = ','),")")
  owString <- glue::glue("overall.width = {overall.width}")
  iwString <- glue::glue("individual.width = {individual.width}")
  wmString <- glue::glue("width.mult = {width.mult}")

  normalized <- normalize(...)


  #calculate indices
  indexTable <- calculateIndices(normalized,indices = indices)

  #write indices to a csv file
  readr::write_csv(indexTable,file.path(normalized$outputDir,"spectralIndices.csv"))

  #plot dashboards
  overall <- plotSpectralDashboard(normalized,indexTable,index.name = indices,width.mult = width.mult,plot.width = overall.width)

  totalDepth <- max(indexTable$depth)

  #create pdf
  ggsave(plot = overall,
         filename = file.path(normalized$outputDir,"allIndices.pdf"),
         width = 20,
         height = totalDepth,
         units = "in",
         limitsize = FALSE)

  #create png
  ggsave(plot = overall,
         filename = file.path(normalized$outputDir,"allIndices.png"),
         width = 20,
         height = totalDepth,
         units = "in",
         limitsize = FALSE)

  #individual indices
  for(i in indices){
    #plot dashboards
    this <- plotSpectralDashboard(normalized,indexTable,index.name = i,width.mult = width.mult,plot.width = individual.width)
#pdf
    ggsave(plot = this,
           filename = file.path(normalized$outputDir,paste0(i,".pdf")),
           width = 10,
           height = totalDepth,
           units = "in",limitsize = FALSE)

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
