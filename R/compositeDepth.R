# coredepths <- read_csv("~/Downloads/testNorm/roi-1/depthTable.csv")
#
# core1 <- core2 <- core3 <- core4 <- coredepths
#
#
# cores <- list()
# cores[[1]] <- core1
# cores[[2]] <- core2
# cores[[3]] <- core3
# cores[[4]] <- core4
#
# names(cores) <- c("one","two","three","four")

#' Create a core-depth table for depth compositing
#'
#' @param cores A named list of coreTables export from normalize()
#'
#' @return
#' @export
#'
#' @examples
createCoreDepthTable <- function(cores){


  for(i in 1:length(cores)){
    core <- cores[[i]] %>%
      dplyr::select(position,cm)

    clT <- dplyr::filter(core,position == "coreLinerTop")$cm
    clB <- dplyr::filter(core,position == "coreLinerBottom")$cm
    roiT <- dplyr::filter(core,position == "roiTop")$cm
    roiB <- dplyr::filter(core,position == "roiBottom")$cm

    #adjust everything to make clT 0

    clT <- clT - clT
    clB <- clB - clT
    roiT <- roiT - clT
    roiB <- roiB - clT

    #remove top gap on first core
    if(i == 1){
      prevBottomCompositeDepth <- clT-roiT
    }

    #create affine table row
    sectionRow = tibble::tibble(section = names(cores)[i],
                                coreTopGap = roiT-clT,
                                coreBotGap = clB-roiB,
                                sedThickness = roiB-roiT,
                                totalLinerLength = clB-clT,
                                compositeDepthAtTopCoreliner = prevBottomCompositeDepth,
                                compositeDepthAtBottomCoreliner = compositeDepthAtTopCoreliner+totalLinerLength,
                                compositeRoiTopDepth = compositeDepthAtTopCoreliner+roiT,
                                compositeRoiBottomDepth = compositeDepthAtTopCoreliner+roiB)

    prevBottomCompositeDepth <- sectionRow$compositeDepthAtTopCoreliner+sectionRow$totalLinerLength

    if(i == 1){
      coreTable <- sectionRow
    }else{
      coreTable <- dplyr::bind_rows(coreTable,sectionRow)
    }

  }
  return(coreTable)
}

section2compositeDepth <- function(coreTable,core.section,depths){
  #get relevant core
  relCore <- dplyr::filter(coreTable,section == core.section)

  compDepths <- depths + relCore$compositeDepthAtTopCoreliner

  if(any(compDepths > relCore$compositeDepthAtBottomCoreliner)){
    stop("uh oh, you have some depths that appear to be too deep for this core")
  }

  return(compDepths)

}


