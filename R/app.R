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
allParams <- list()


ui <- shiny::fluidPage(
  tags$head(tags$style(HTML('* {font-family: "Georgia"};'))),
  titlePanel("specimR", windowTitle = "specimR"),
  sidebarLayout(
  sidebarPanel(
  shiny::fluidRow(
    align="center",
    shinyFiles::shinyDirButton("file_dir", "Select directory with captured data", title = "Select directory"),
    shiny::br(),
    shiny::br(),
  ),
  shiny::fluidRow(
    shiny::column(
      4,
      shiny::radioButtons("choice_normalize", "Do you need to normalize the data?", choices = list(Yes = "Yes", No = "No"))
    ),
    shiny::column(
      4,
      shiny::radioButtons("choice_integration", "Is your white reference scanned with different settings than core?", choices = list(Yes = "Yes", No = "No"))
    ),
    shiny::column(
      4,
      shiny::checkboxGroupInput("choice_proxies", "Choose proxies to calculate", choices = list(Rmean = "Rmean", RABD615 = "RABD615", RABD660670 = "RABD660-670", RABD845 = "RABD845", RABD710730 = "RABD710-730", R570R630 = "R570R630", R590R690 = "R590R690"))
    )
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
    "Current Selection",
    shiny::br(),
    shiny::column(
      4,
      "xmin",
      shiny::br(),
      shiny::verbatimTextOutput("xmin"),
      "xmax",
      shiny::verbatimTextOutput("xmax"),
    ),
    shiny::column(
      4,
      "ymin",
      shiny::br(),
      shiny::verbatimTextOutput("ymin"),
      "ymax",
      shiny::verbatimTextOutput("ymax"),
    ),
    ),
    shiny::column(
      8,
    shiny::br(),
    shiny::br(),
      "Analysis Regions",
    shiny::br(),
    shiny::tableOutput("analysisRegions"),

)
  )),
    # Main panel for displaying outputs ----
    mainPanel(

      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
                  id = "tabset1",
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
                           align="center",
                           shiny::br(),
                           headerPanel("select regions to analyze"),
                           shiny::br(),
                           shiny::br(),
                           shiny::fluidRow(
                           shiny::column(
                             4,
                             align="left",
                             actionButton("resetPlot", "Reset selection"),
                           ),
                           shiny::column(
                             4,
                             align="center",
                             actionButton("selectAnalysisRegion", "Proceed with selected region"),
                           ),
                           shiny::column(
                             4,
                             align="right",
                             actionButton("acceptAnalysisRegions", "Accept and analyze"),
                           )),
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
                  tabPanel("Distance Calibration",
                           align="center",
                           shiny::br(),
                           sliderInput(
                             inputId="scalermarkerPointSize",
                             label="Size of marker points",
                             min=0.1,
                             max=10,
                             value=3,
                             step = 0.1,
                             round = FALSE,
                             ticks = TRUE,
                             animate = FALSE,
                             width = NULL,
                             sep = ",",
                             pre = NULL,
                             post = NULL,
                             timeFormat = NULL,
                             timezone = NULL,
                             dragRange = TRUE
                           ),
                           shiny::column(
                             3,
                             "distance in pixels",
                             shiny::verbatimTextOutput("scaleDist")
                           ),
                           shiny::column(
                             3,
                             "length of scale bar in mm",
                             numericInput(inputId = "scaleLength",
                                          step = 1,
                                          min = 1,
                                          label=NULL,
                                          max=10000,
                                          value = 1500)
                           ),
                           shiny::column(
                             3,
                             "pixels per mm",
                             shiny::verbatimTextOutput("pixelRatio")
                           ),
                           shiny::br(),
                           shiny::plotOutput(outputId = "core_plot2",width = "100%", click="plot_click")
                  ),
                  tabPanel("Analysis",
                          "download options...")
                  )
      )
  ),
)

server <- function(input, output, session) {
  countRegions <- reactiveValues(count = 0)
  clickCounter <- reactiveValues(count=1)
  volumes <- c(shinyFiles::getVolumes()())
  shinyFiles::shinyDirChoose(input, "file_dir", roots = volumes)

  #capture user directory
  user_dir <- reactive({
    shinyFiles::parseDirPath(volumes, selection = input$file_dir)
    #shinyFiles::parseDirPath(volumes, selection = "C:/Users/dce25/Downloads/STL14_1A_28C_top_2022-11-11_16-30-51")
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
    })

  # Print raster files
  output$core_dir <- renderPrint(rasters())

  #make plot
  plot1 <- reactive({
    if (length(user_dir()) != 0){
      terra::plotRGB(x = terra::rast(rasters()[2]), r = 50, g = 75, b = 100, stretch = "hist")
    }
  })

  #render plot
  output$core_plot <- renderPlot(plot1(), height = 4000, width = 600, res = 20)

  source_coords <- reactiveValues(xy=data.frame(x=c(1,1),  y=c(1,1)))

  observeEvent(input$plot_click, {
    clickCounter$count <- clickCounter$count + 1
    if (ceiling(clickCounter$count/2) == clickCounter$count/2){
      source_coords$xy[2,] <- c(input$plot_click$x, input$plot_click$y)
    }else{
      source_coords$xy[1,] <- c(input$plot_click$x, input$plot_click$y)
    }
  })

  #measure scale bar
  dist1 <- reactive({
    sum(abs(source_coords$xy[1,]-source_coords$xy[2,]))
  })

  output$scaleDist <- renderText(dist1())

  output$pixelRatio <- renderText(dist1()/input$scaleLength)

  #plot box selection

  x_range <- function(e) {
    if(is.null(e)) return(c(0,0))
    c(round(e$xmin, 1), round(e$xmax, 1))
  }

  y_range <- function(e) {
    if(is.null(e)) return(c(0,0))
    c(round(e$ymin, 1), round(e$ymax, 1))
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
    allParams$cropWhole <- c(x_range(input$plotBrush)[1], x_range(input$plotBrush)[2],
                             y_range(input$plotBrush)[1], y_range(input$plotBrush)[2])
  output$cropped_plot <- renderPlot({
    terra::plotRGB(x = terra::rast(rasters()[2]), r = 50, g = 75, b = 100, stretch = "hist",
                   ext=terra::ext(allParams$cropWhole))
      points(y=source_coords$xy[,2], x=source_coords$xy[,1], cex=input$scalermarkerPointSize, pch=19)
      #points( source_coords$xy[1,1], source_coords$xy[1,2], cex=3, pch=intToUtf8(8962))
      #text(source_coords$xy[2,1], source_coords$xy[2,2], paste0("Distance=", dist1), cex=3)
    },
    height = 4000,
    width = 600
    )

  updateTabsetPanel(session=session,
                    "tabset1",
                    selected = "Select Analysis Regions")
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

  observeEvent(input$acceptAnalysisRegions, {
    output$core_plot2 <- renderPlot({
      terra::plotRGB(x = terra::rast(rasters()[2]), r = 50, g = 75, b = 100, stretch = "hist")
      points(y=source_coords$xy[,2], x=source_coords$xy[,1], cex=input$scalermarkerPointSize, pch=19)
      #points( source_coords$xy[1,1], source_coords$xy[1,2], cex=3, pch=intToUtf8(8962))
      #text(source_coords$xy[2,1], source_coords$xy[2,2], paste0("Distance=", dist1), cex=3)
    },
    height = 4000,
    width = 600
    )
    ## Source


    ## Destination
    ##

    updateTabsetPanel(session=session,
                      "tabset1",
                      selected = "Distance Calibration")
  })

}

shiny::shinyApp(ui, server, options = list(launch.browser = TRUE))
