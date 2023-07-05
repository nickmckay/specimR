# shiny app

# choose capture file
# choose if it needs normalization - TRUE/FALSE (old/new specim)
# if needs normalization then choose if different times for capture and reference - TRUE/FALSE (needs two times/doesn't need)
# choose proxies
# normalize entire core or rois only (computation speed) -> depending on future needs (like full core classification?)
# should I write to script?
# draw and store large roi to crop raster
# draw and store small rois to actually extract data from
# select tube top and tube depth and read depths
# normalize data if needed
# extract rois
# normalize core or rois only
# calculate indices in rois
# smooth data
# recalculate depths
# write data-frames
# write plots with maps
# write entire core pseudocolor rgb
# write script

# showModal(modalDialog(
#   title = "odd",
#   "odd",
#   easyClose = TRUE,
#   footer = NULL
# ))


#' run shiny app to set up core image analysis
#'
#' @return
#' @export
#'
run_core <- function(){

  library(shiny)
  allParams <- list()

  runApp(
  shinyApp(
  ui = shiny::fluidPage(
    tags$head(tags$style(HTML('* {font-family: "Georgia"};'))),
    tags$style(type = "text/css", ".irs-grid-pol.small {height: 0px;}"),
    tags$script(HTML("
          $(document).ready(function() {setTimeout(function() {
            supElement = document.getElementById('scalermarkerPointSize').parentElement;
            $(supElement).find('span.irs-max, span.irs-min, span.irs-single, span.irs-from, span.irs-to').remove();
          }, 50);})
        ")),
    titlePanel("specimR", windowTitle = "specimR"),
        # Output: Tabset w/ plot, summary, and table ----
        tabsetPanel(type = "tabs",
                    id = "tabset1",
                    tabPanel("Select Data",
                             shiny::br(),
                             shiny::br(),
                             shiny::fluidRow(
                               align="center",
                               shinyFiles::shinyDirButton("file_dir", "Select directory with captured data", title = "Select directory"),
                               shiny::actionButton("file_dir_example", "Use example data"),
                               shiny::br(),
                               shiny::br(),
                             ),
                             shiny::fluidRow(

                               shiny::column(
                                 12,
                                 "Selected core directory",
                                 shiny::br(),
                                 shiny::verbatimTextOutput("core_dir_show"),
                                 shiny::br(),
                                 "Raster files in the directory",
                                 shiny::br(),
                                 shiny::verbatimTextOutput("core_dir"),
                                 shiny::br(),
                                 shiny::actionButton("proceed_with_data", "Proceed with selected data")
                               ),
                             )
                    ),
                    tabPanel("Crop Image",
                               align="center",
                               shiny::br(),
                               headerPanel("crop viable region of image"),
                               shiny::br(),
                               shiny::br(),
                               shiny::column(
                                 6,
                                 align="left",
                                 actionButton("resetPlot", "Reset selection"),
                               ),
                               shiny::column(
                                 6,
                                 align="right",
                                 actionButton("selectPlotRegion", "Accept crop"),
                               ),
                               shiny::br(),
                               shiny::br(),
                                shinycssloaders::withSpinner(shiny::plotOutput(outputId = "core_plot",
                                                                            width = "100%",
                                                                            brush = brushOpts(
                                                                              id = "plotBrush",
                                                                              delay = 5000,
                                                                              fill = "black",
                                                                              stroke = "black",
                                                                              opacity = 0.4
                                                                            )
                                                                            )
                                                             ),
                             ),
                    tabPanel("Select Analysis Regions",
                             sidebarLayout(
                               sidebarPanel(
                                 style = "position:fixed; margin-top:150px;",
                                 width=2,
                             align="center",
                             actionButton("skipSelectAnalysisRegion", "Analyze full image (skip region selection step)"),
                             shiny::br(),
                             shiny::br(),
                             actionButton("selectAnalysisRegion", "Add selected region"),
                             shiny::br(),
                             shiny::br(),
                             shiny::fluidRow(
                               shiny::column(
                                 6,
                                 "xmin",
                                 shiny::br(),
                                 shiny::verbatimTextOutput("xmin"),
                                 "ymin",
                                 shiny::verbatimTextOutput("ymin"),
                               ),
                               shiny::column(
                                 6,
                                 "xmax",
                                 shiny::br(),
                                 shiny::verbatimTextOutput("xmax"),
                                 "ymax",
                                 shiny::verbatimTextOutput("ymax"),
                               ),
                             ),
                             shiny::fluidRow(
                               align="center",
                               shiny::br(),
                               "Analysis Regions",
                               shiny::br(),
                               shiny::tableOutput("analysisRegions"),
                             ),
                             shiny::br(),
                             actionButton("acceptAnalysisRegions", "Accept all selections"),
                               ),
                             mainPanel(
                               align="right",
                             shiny::br(),
                             shiny::br(),
                             shinycssloaders::withSpinner(shiny::plotOutput(outputId = "cropped_plot",
                                                                            width = "100%",
                                                                            brush = brushOpts(
                                                                              id = "plotBrush",
                                                                              delay = 5000,
                                                                              fill = "black",
                                                                              stroke = "black",
                                                                              opacity = 0.4
                                                                            )
                             )
                             ),
                             ),
                    ),),
                    tabPanel("Distance Calibration",
                             align="center",
                             shiny::br(),
                             shiny::fluidRow(
                                 align="center",
                                 style = "text-align:center; font-weight:100;",
                                 shiny::column(
                                   3,
                             sliderInput(
                               inputId="scalermarkerPointSize",
                               label="Size of marker points",
                               min=1,
                               max=5,
                               value=3,
                               step = 0.1,
                               round = FALSE,
                               ticks = FALSE,
                               animate = FALSE,
                               width = NULL,
                               sep = ",",
                               pre = NULL,
                               post = NULL,
                               timeFormat = NULL,
                               timezone = NULL,
                               dragRange = TRUE
                               ),
                             ),
                             shiny::column(
                               1,
                               style = "margin-top: 10px;",
                               "point A (x,y)",
                               shiny::verbatimTextOutput("distCoordA")
                             ),
                             shiny::column(
                               1,
                               style = "margin-top: 10px;",
                               "point B (x,y)",
                               shiny::verbatimTextOutput("distCoordB")
                             ),
                             shiny::column(
                               1,
                               style = "margin-top: 10px;",
                               "distance along scale",
                               shiny::verbatimTextOutput("scaleDist")
                             ),
                             shiny::column(
                               1,
                               style = "margin-top: 10px;",
                               "distance along y axis",
                               shiny::verbatimTextOutput("coreDist")
                             ),
                             shiny::column(
                               2,
                               style = "margin-top: 10px;",
                               "distance along scale bar in mm",
                               numericInput(inputId = "scaleLength",
                                            step = 1,
                                            min = 1,
                                            label=NULL,
                                            max=10000,
                                            value = 1500)
                             ),
                             shiny::column(
                               2,
                               style = "margin-top: 10px;",
                               "mm / pixel",
                               shiny::verbatimTextOutput("pixelRatio")
                             ),
                             ),
                             shiny::fluidRow(
                               align="center",
                               actionButton("acceptCalibration", "Accept distance calibration", style = "margin-right: 10px; margin-top:10px;"),
                             ),
                             shiny::br(),
                             shiny::plotOutput(outputId = "core_plot2",width = "100%", click="plot_click")
                    ),
                    tabPanel("Analysis",
                             shiny::fluidRow(
                               shiny::column(
                                 4,
                                 shiny::radioButtons("choice_normalize", "Do you need to normalize the data?", choices = list(Yes = "Yes", No = "No"))
                               ),
                               shiny::column(
                                 4,
                                 shiny::radioButtons("choice_integration", "Is your white reference scanned with different settings than core?", choices = list(Yes = "Yes", No = "No"), selected = "No")
                               ),
                               shiny::column(
                                 4,
                                 shiny::selectInput("choice_proxies", "Choose proxies to calculate", choices = list(Rmean = "Rmean", RABD615 = "RABD615", RABD660670 = "RABD660-670", RABD845 = "RABD845", RABD710730 = "RABD710-730", R570R630 = "R570R630", R590R690 = "R590R690"), multiple = TRUE)
                               )
                             ),
                             shiny::fluidRow(
                             style = "text-align:center; font-weight:100;",
                             actionButton('ok','Save and Exit')
                             )
                             )
                    )
  ),

  server = function(input, output, session) {
    countRegions <- reactiveValues(count = 0)
    clickCounter <- reactiveValues(count=1)
    volumes <- c(shinyFiles::getVolumes()())
    shinyFiles::shinyDirChoose(input, "file_dir", roots = volumes)


    #capture user directory
    user_dir <- reactive({
      shinyFiles::parseDirPath(volumes, selection = input$file_dir)
      #"C:/Users/dce25/Downloads/STL14_1A_28C_top_2022-11-11_16-30-51"
    })

    user_dir2 <- eventReactive(input$file_dir_example, {
      "C:/Users/dce25/Downloads/STL14_1A_28C_top_2022-11-11_16-30-51"
    })

    observeEvent(input$proceed_with_data, {
      updateTabsetPanel(session=session,
                        "tabset1",
                        selected = "Crop Image")
    })

    #print directory
    output$core_dir_show <- renderPrint({
      if (length(user_dir()) != 0){
        user_dir()
      }
    })

    # List raster files in the selected directory
    rasters <- reactive({
      if (length(user_dir()) != 0){
        user_dir() |>
          fs::dir_ls(type = "file", regexp = ".raw", recurse = TRUE)
      }
      else if (length(user_dir2()) != 0){
        user_dir2() |>
          fs::dir_ls(type = "file", regexp = ".raw", recurse = TRUE)
      }
      })

    # Print raster files
    output$core_dir <- renderPrint(rasters())

    #make plot
    plot1 <- reactive({
      if (length(user_dir()) != 0){
        terra::plotRGB(x = terra::rast(rasters()[2]), r = 50, g = 75, b = 100, stretch = "hist")
      }
      else if (length(user_dir2()) != 0){
        terra::plotRGB(x = terra::rast(rasters()[2]), r = 50, g = 75, b = 100, stretch = "hist")
      }
    })

    #render plot
    output$core_plot <- renderPlot(plot1(), height = 3000, width = 800, res = 20)

    source_coords <- reactiveValues(xy=data.frame(x=c(1,1),  y=c(1,1)))

    observeEvent(input$plot_click, {
      clickCounter$count <- clickCounter$count + 1
      if (ceiling(clickCounter$count/2) == clickCounter$count/2){
        source_coords$xy[2,] <- c(round(input$plot_click$x), round(input$plot_click$y))
      }else{
        source_coords$xy[1,] <- c(round(input$plot_click$x), round(input$plot_click$y))
      }
    })

    #measure scale bar
    distTot <- reactive({
      round(sum(abs(source_coords$xy[1,]-source_coords$xy[2,])))
    })

    distY <- reactive({
      round(abs(source_coords$xy[1,2]-source_coords$xy[2,2]))
    })

    pointA <- reactive({
      which(max(source_coords$xy[,2]) == source_coords$xy[,2])
    })

    pointB <- reactive({
      which(min(source_coords$xy[,2]) == source_coords$xy[,2])
    })

    output$distCoordA <- renderText(paste0("(", round(unlist(source_coords$xy[pointA(),])[1]), ", ", round(nrow(terra::rast(rasters()[2])[[1]]) - unlist(source_coords$xy[pointA(),])[2]), ")"))
    output$distCoordB <- renderText(paste0("(", round(unlist(source_coords$xy[pointB(),])[1]), ", ", round(nrow(terra::rast(rasters()[2])[[1]]) - unlist(source_coords$xy[pointB(),])[2]), ")"))

    output$coreDist <- renderText(distY())

    output$scaleDist <- renderText(distTot())

    output$pixelRatio <- renderText(input$scaleLength/distTot())

    #plot box selection

    x_range <- function(e) {
      if(is.null(e)) return(c(0,0))
      c(round(e$xmin, 0), round(e$xmax, 0))
    }

    y_range <- function(e) {
      if(is.null(e)) return(c(0,0))
      c(round(e$ymin, 0), round(e$ymax, 0))
    }

    #output$xmin <- reactive({x_range(input$plot_brush)[1]})

    output$xmin <- reactive({x_range(input$plotBrush)[1]})
    output$xmax <- reactive({x_range(input$plotBrush)[2]})

    #output$ymin <- reactive({y_range(input$plot_brush)[1]})

    output$ymin <- reactive({y_range(input$plotBrush)[1]})
    output$ymax <- reactive({y_range(input$plotBrush)[2]})

    brush <- NULL
    makeReactiveBinding("brush")

    observeEvent(input$plotBrush, {
      brush <<- input$plotBrush
    })

    observeEvent(input$clearBrush, {
      session$resetBrush("plotBrush")
    })

    observeEvent(input$resetPlot, {
      session$resetBrush("plotBrush")
      brush <<- NULL
    })

    observeEvent(input$selectPlotRegion, {
      allParams$cropImage <<- c(x_range(input$plotBrush)[1], x_range(input$plotBrush)[2],
                               y_range(input$plotBrush)[1], y_range(input$plotBrush)[2])
    output$cropped_plot <- renderPlot({
      terra::plotRGB(x = terra::rast(rasters()[2]), r = 50, g = 75, b = 100, stretch = "hist",
                     ext=terra::ext(allParams$cropImage))
      if (sum(complete.cases(analysisRegions$DT))>0){
        for (i in 1:nrow(analysisRegions$DT)){
          polygon(x=c(analysisRegions$DT[i,1], analysisRegions$DT[i,2], analysisRegions$DT[i,2], analysisRegions$DT[i,1]),
                  y=c(analysisRegions$DT[i,4], analysisRegions$DT[i,4], analysisRegions$DT[i,3], analysisRegions$DT[i,3]),
                  col = rgb(red = 0.5, green = 0.5, blue = 0.5, alpha = 0.5), lwd=3)
        }
      }

        # points(y=source_coords$xy[,2], x=source_coords$xy[,1], cex=input$scalermarkerPointSize, pch=19)

        #points( source_coords$xy[1,1], source_coords$xy[1,2], cex=3, pch=intToUtf8(8962))
        #text(source_coords$xy[2,1], source_coords$xy[2,2], paste0("Distance=", dist1), cex=3)
      },
      height = 3000,
      width = 800
      )

    updateTabsetPanel(session=session,
                      "tabset1",
                      selected = "Select Analysis Regions")

    session$resetBrush("plotBrush")
    brush <<- NULL
    })

    analysisRegions <- reactiveValues()
    analysisRegions$DT <- data.frame("xmin"=NA,
                                     "xmax"=NA,
                                     "ymin"=NA,
                                     "ymax"=NA)
    output$analysisRegions <- renderTable(analysisRegions$DT)
    # colnames(analysisRegions$DT) <- c("xmin", "xmax", "ymin", "ymax")


    observeEvent(input$selectAnalysisRegion, {

      #add new set of bounds to saved set
      countRegions$count <- countRegions$count + 1
      # if (countRegions$count == 1){
      #   analysisRegions$DT <- data.frame(matrix(nrow = 1, ncol = 4))
      #
      # }

      analysisRegions$DT <- rbind(analysisRegions$DT, c(x_range(input$plotBrush)[1], x_range(input$plotBrush)[2],
                                                                        y_range(input$plotBrush)[1], y_range(input$plotBrush)[2]))

      if (countRegions$count == 1){
        analysisRegions$DT <- analysisRegions$DT[-1,]
        colnames(analysisRegions$DT) <- c("xmin", "xmax", "ymin", "ymax")
      }

      #allParams$analysisRegions <- analysisRegions()

      output$analysisRegions <- renderTable(analysisRegions$DT)
      #reset brush
      session$resetBrush("plotBrush")
      brush <<- NULL

    })

    observeEvent(input$skipSelectAnalysisRegion, {
      countRegions$count <- 0
      analysisRegions$DT <- data.frame("xmin"=NA,
                                       "xmax"=NA,
                                       "ymin"=NA,
                                       "ymax"=NA)
      session$resetBrush("plotBrush")
      brush <<- NULL

      updateTabsetPanel(session=session,
                        "tabset1",
                        selected = "Distance Calibration")

    })

    observeEvent(input$acceptCalibration, {
      updateTabsetPanel(session=session,
                        "tabset1",
                        selected = "Analysis")
    })

    observeEvent(input$ok, {
      distances <- list()
      distances$pointA <- round(unlist(source_coords$xy[pointA(),]))
      distances$pointB <- round(unlist(source_coords$xy[pointB(),]))
      distances$coreDist <- distY()
      distances$scaleDist <- distTot()
      distances$scaleDistmm <- input$scaleLength
      distances$pixelRatio <- input$scaleLength/distTot()

      analysisOptions <- list()
      analysisOptions$normalize <- input$choice_normalize
      analysisOptions$integration <- input$choice_integration
      analysisOptions$proxies <- input$choice_proxies


      if (length(user_dir()) != 0){
        allParams$directory <<- user_dir()
      }else{
        allParams$directory <<- user_dir2()
      }

      allParams$analysisRegions <<- analysisRegions$DT
      allParams$distances <<- distances
      allParams$analysis <<- analysisOptions

      stopApp(allParams)
    })

    observeEvent(input$acceptAnalysisRegions, {
      output$core_plot2 <- renderPlot({
        terra::plotRGB(x = terra::rast(rasters()[2]), r = 50, g = 75, b = 100, stretch = "hist")
        points(y=source_coords$xy[,2], x=source_coords$xy[,1], cex=input$scalermarkerPointSize, pch=19)
        points(y=source_coords$xy[,2], x=source_coords$xy[,1], cex=input$scalermarkerPointSize/3, pch=19, col="white")
        #points( source_coords$xy[1,1], source_coords$xy[1,2], cex=3, pch=intToUtf8(8962))
        #text(source_coords$xy[2,1], source_coords$xy[2,2], paste0("Distance=", dist1), cex=3)
      },
      height = 3000,
      width = 800
      )
      ## Source


      ## Destination
      ##

      updateTabsetPanel(session=session,
                        "tabset1",
                        selected = "Distance Calibration")
    })

  }
  )
  )
}

# run_core <- function() {shiny::shinyApp(ui, server, options = list(launch.browser = TRUE))}
