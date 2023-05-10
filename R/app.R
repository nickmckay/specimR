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


ui <- fluidPage(
  fluidRow(
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
    shinyFiles::shinyDirButton("file_dir", "Select directory with captured data", title = "Select directory")
  ),
  br(),
  "Selected core directory",
  br(),
  shiny::verbatimTextOutput("core_dir_show"),
  br(),
  "Raster files in the directory",
  br(),
  shiny::verbatimTextOutput("core_dir"),
  br(),
  shiny::plotOutput("core_plot")
)

server <- function(input, output, session) {
  volumes <- c(getVolumes()())
  shinyDirChoose(input, "file_dir", roots = volumes)

  output$core_dir_show <- renderPrint({
    if (is.integer(input$file_dir)) {
      cat("No directory has been selected (shinyDirChoose)")
    } else {
      parseDirPath(volumes, selection = input$file_dir)
    }
  })

  # List raster files in the selected directory
  rasters <- reactive({parseDirPath(volumes, selection = input$file_dir) |>
    fs::dir_ls(type = "file", regexp = ".raw", recurse = TRUE)})

  # List raster files
  output$core_dir <- renderPrint(rasters())

  # Create terra raster from capture
  raster <- reactive({terra::rast(rasters()[1])})

  # Create terra RGB plot of capture
  output$core_plot <- renderPlot(terra::plotRGB(x = raster(), r = 50, g = 75, b = 100, stretch = "hist"), width = 300, height = 500, res = 96)
}

shinyApp(ui, server)
