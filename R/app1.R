# shiny app

library(shiny)
library(shinyFiles)
library(bslib)

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


ui <- shiny::fluidPage(
  theme = bs_theme(version = 4, bootswatch = "flatly"),
  titlePanel("specimR", windowTitle = "specimR"),
  shiny::br(),
  shiny::br(),
  shiny::fluidRow(
    shiny::column(
      4,
      shiny::radioButtons("choice_normalize", "Do you need to normalize the data?", choices = list(Yes = "Yes", No = "No")),
      shiny::radioButtons("choice_integration", "Is your white reference scanned with different settings than core?", choices = list(Yes = "Yes", No = "No"))
    ),
    shiny::mainPanel(
      br(),
      "You selected this core directory",
      br(),
      shiny::verbatimTextOutput("core_dir_show"),
      br(),
      "Found these raster files in the directory",
      br(),
      shiny::verbatimTextOutput("core_dir"),
      br(),
      shiny::plotOutput("core_plot")
    )
  ),
  shiny::fluidRow(

    shiny::column(
      8,
      strong("Select core directory"),
      br(),
      shinyFiles::shinyDirButton("file_dir", "Select core directory", title = "Select core directory", buttonType = "primary"),
    shiny::br(),
    shiny::br(),
    "Selected core directory",
    shiny::br(),
    shiny::br(),
    shiny::verbatimTextOutput("core_dir_show"),
    shiny::br(),
    shiny::br(),
    "Raster files in the directory",
    shiny::br(),
    shiny::br(),
    shiny::verbatimTextOutput("core_dir"),
    shiny::br(),
    shiny::br(),
    shiny::column(
      2,
      "xmin",
      shiny::br(),
      shiny::br(),
      shiny::verbatimTextOutput("xmin"),
      "xmax",
      shiny::verbatimTextOutput("xmax"),
    ),
    shiny::column(
      2,
      "ymin",
      shiny::br(),
      shiny::br(),
      shiny::verbatimTextOutput("ymin"),
      "ymax",
      shiny::verbatimTextOutput("ymax"),
    ),
    ),
    shiny::column(
    4,
    align="center",
    actionButton("resetPlot", "Reset plot"),
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
    )),
    )
  ),
)

server <- function(input, output, session) {
  volumes <- c(shinyFiles::getVolumes()())
  shinyFiles::shinyDirChoose(input, "file_dir", roots = volumes)

  #capture user directory
  user_dir <- reactive({
    shinyFiles::parseDirPath(volumes, selection = input$file_dir)
  })

  #print directory
  output$core_dir_show <- renderPrint({
    if (length(user_dir()) != 0){
      user_dir()
    }
  })

  # List raster files in the selected directory
  rasters <- reactive({
<<<<<<< HEAD
    if (length(user_dir()) != 0){
      user_dir() |>
        fs::dir_ls(type = "file", regexp = ".raw", recurse = TRUE)
    }
    })
=======
    parseDirPath(volumes, selection = input$file_dir) |>
      fs::dir_ls(type = "file", regexp = ".raw", recurse = TRUE)
  })
>>>>>>> 0afaa7fc5973115a53f3332eaadfb4d8e3a57cd1

  # Print raster files
  output$core_dir <- renderPrint(rasters())

<<<<<<< HEAD
  #make plot
  plot1 <- reactive({
    if (length(user_dir()) != 0){
      terra::plotRGB(x = terra::rast(rasters()[2]), r = 50, g = 75, b = 100, stretch = "hist")
    }
  })

  #render plot
  output$core_plot <- renderPlot(plot1(), height = 1200, width = 150, res = 20)



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
=======
  # Create terra raster from capture
  raster <- reactive({
    terra::rast(rasters()[1])
  })

  # Create terra RGB plot of capture
  output$core_plot <- renderPlot(terra::plotRGB(x = raster(), r = 50, g = 75, b = 100, stretch = "hist"), width = 300, res = 96)
>>>>>>> 0afaa7fc5973115a53f3332eaadfb4d8e3a57cd1
}

shiny::shinyApp(ui, server, options = list(launch.browser = TRUE))
