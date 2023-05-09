# Simple ROI picking function
pick_roi_simple <- function(image) {
  terra:plotRGB(image, axes = TRUE, stretch = "hist", main = "Raw image")

  crop_ext <- terra::draw()

  return(crop_ext)
}

# Function to view crop
crop_view_fun <- function(click, dblclick) {
  xmin <- min(click$x, dblclick$x)
  xmax <- max(click$x, dblclick$x)
  ymin <- min(click$y, dblclick$y)
  ymax <- max(click$y, dblclick$y)

  xy <- data.frame(x = c(xmin, xmax, xmax, xmin), y = c(ymin, ymin, ymax, ymax))

  polygon(xy, col = NA, border = "red")

  clickpoints <- rbind(c(click$x, click$y), c(dblclick$x, dblclick$y))

  points(clickpoints, cex = 2)

  points(clickpoints, pch = 3, col = "green")
}

# Function to view roi
roi_view_fun <- function(im, click, dblclick) {
  xmin <- min(click$x, dblclick$x)
  xmax <- max(click$x, dblclick$x)
  ymin <- min(click$y, dblclick$y)
  ymax <- max(click$y, dblclick$y)

  xy <- data.frame(x = c(xmin, xmax, xmax, xmin), y = c(ymin, ymin, ymax, ymax))

  plot(im, axes = FALSE, interpolate = TRUE)

  polygon(xy, col = NA, border = "red")

  clickpoints <- rbind(c(click$x, click$y), c(dblclick$x, dblclick$y))

  points(clickpoints, cex = 2)

  points(clickpoints, pch = 3, col = "green")
}

# Shiny server
server <- function(input, output, session) {

  image <- get("image", envir = specimEnv)
  crpout <<- list() # why?
  # im <- get("im",envir = specimEnv)

  brushExtent <- reactiveValues()
  click <- reactiveValues()
  dblclick <- reactiveValues()
  rois <- reactiveValues()
  nroi <- reactiveVal(0) # rv <- reactiveValues(value = 0)
  output$nroi <- renderText({
    nroi() # rv$value
  })

  click$x <- 0
  click$y <- 0
  dblclick$x <- 0
  dblclick$y <- 0

  init.extent <- terra::ext(image)
  init.extent[3] <- init.extent[4] - 2000 # why?

  brushExtent$extent <- init.extent

  output$plot2 <- renderPlot(
    {
      terra::plotRGB(image, axes = FALSE, stretch = "hist", addfun = crop_view_fun(click, dblclick), maxpixels = prod(dim(image)[1:2]) * (input$slider / 100))
    },
    width = 500
  )

  output$plot3 <- renderPlot({
    terra::plotRGB(image, axes = TRUE, stretch = "hist", main = "Select Crop Corners", ext = brushExtent$extent)
  })

  # If so, zoom to the brush bounds
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      brushExtent$extent <- terra::ext(brush$xmin, brush$xmax, brush$ymin, brush$ymax)
    } else {
      brushExtent$extent <- terra::ext(image)
    }



    brushExtent$brushBox <- data.frame(
      x = c(brush$xmin, brush$xmax, brush$xmax, brush$xmin),
      y = c(brush$ymin, brush$ymin, brush$ymax, brush$ymax)
    )
  })

  observeEvent(input$image_click, {
    click$x <- input$image_click$x
    click$y <- input$image_click$y
    updateNumericInput(session, "x1", value = click$x)
    updateNumericInput(session, "y1", value = click$y)
  })

  observeEvent(input$image_dblclick, {
    dblclick$x <- input$image_dblclick$x
    dblclick$y <- input$image_dblclick$y
    updateNumericInput(session, "x2", value = dblclick$x)
    updateNumericInput(session, "y2", value = dblclick$y)
  })

  # output$click_info <- renderPrint({
  #   cat("click:\n")
  #   str(c(click$x,click$y))
  # })
  #
  # output$dblclick_info <- renderPrint({
  #   cat("dblclick:\n")
  #   str(c(dblclick$x,dblclick$y))
  # })

  output$nrois <- renderText({
    str(c(rois$n))
  })

  observeEvent(input$applyWidth, {
    if (click$x >= dblclick$x) {
      click$x <- dblclick$x + input$width
    } else if (dblclick$x > click$x) {
      dblclick$x <- click$x + input$width
    }
  })

  observeEvent(input$center, {
    width <- abs(click$x - dblclick$x)
    re <- terra::ext(image)
    totalWidth <- re[2] - re[1]
    mid <- round(re[2] / 2)
    if (click$x >= dblclick$x) {
      click$x <- round(mid + width / 2)
      dblclick$x <- round(mid - width / 2)
    } else if (dblclick$x > click$x) {
      dblclick$x <- round(mid + width / 2)
      click$x <- round(mid - width / 2)
    }
  })

  observeEvent(input$record, {
    newRoi <- nroi() + 1
    nroi(newRoi)
    xmin <- min(click$x, dblclick$x)
    xmax <- max(click$x, dblclick$x)
    ymin <- min(click$y, dblclick$y)
    ymax <- max(click$y, dblclick$y)
    crp <- terra::ext(c(xmin, xmax, ymin, ymax))
    crpout[[newRoi]] <<- crp # why?
  })

  observeEvent(input$stopselecting, {
    if (nroi() == 0) { # if none are selected, select this one.
      newRoi <- nroi() + 1
      nroi(newRoi)
      xmin <- min(click$x, dblclick$x)
      xmax <- max(click$x, dblclick$x)
      ymin <- min(click$y, dblclick$y)
      ymax <- max(click$y, dblclick$y)
      crp <- terra::ext(c(xmin, xmax, ymin, ymax))
      crpout[[newRoi]] <<- crp # why?
    }
    shiny::stopApp()
  })
}

