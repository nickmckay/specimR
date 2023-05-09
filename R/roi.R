pick_roi_simple <- function(image) {
  terra:plotRGB(image, axes = TRUE, stretch = "hist", main = "Raw Image")
  cropC <- terra::draw()
  return(cropC)
}

cropViewFun <- function(click, dblclick) {
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

roiViewFun <- function(im, click, dblclick) {
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



server <- function(input, output, session) {
  image <- get("image", envir = specimEnv)
  crpout <<- list()
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
  init.extent[3] <- init.extent[4] - 2000

  brushExtent$extent <- init.extent

  output$plot2 <- renderPlot(
    {
      terra::plotRGB(image, axes = FALSE, stretch = "hist", addfun = cropViewFun(click, dblclick), maxpixels = prod(dim(image)[1:2]) * (input$slider / 100))
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
    crpout[[newRoi]] <<- crp
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
      crpout[[newRoi]] <<- crp
    }
    shiny::stopApp()
  })
}


pick_roi_shiny <- function(image, zh = 5000) {
  # assign image into specimEnv
  assign("image", image, envir = specimEnv)

  ui <- shiny::fluidPage(
    # Some custom CSS for a smaller font for preformatted text
    tags$head(
      tags$style(HTML("
      pre, table.table {
        font-size: smaller;
      }
    "))
    ),
    # textOutput("value"),

    column(
      width = 12,
      h2("Region of interest (ROI) selector"),
      shiny::fluidRow(
        column(
          width = 8, style = "overflow-y:scroll; max-height: 600px",
          plotOutput("plot2",
            height = zh,
            brush = brushOpts(
              id = "plot2_brush",
              resetOnNew = FALSE
            )
          )
        ),
        column(
          width = 4,
          plotOutput("plot3",
            height = 500,
            click = "image_click",
            dblclick = dblclickOpts(
              id = "image_dblclick"
            )
          )
        )
      ),
      shiny::fluidRow(
        column(
          width = 3,
          sliderInput("slider", h4("Left image resolution (% of max)"),
            min = 1, max = 100, step = 5,
            value = 1
          )
        ),
        column(
          width = 3,
          numericInput("x1",
            h4("Click x"),
            value = 0
          ),
          numericInput("y1",
            h4("Click y"),
            value = 0
          )
        ),
        column(
          width = 3,
          numericInput("x2",
            h4("Dbl-Click x"),
            value = 0
          ),
          numericInput("y2",
            h4("Dbl-Click y"),
            value = 0
          )
        ),
        column(
          3,
          numericInput("width",
            h4("Specify width"),
            value = 200
          ),
          actionButton("applyWidth", "Apply width"),
          actionButton("center", "Center ROI"),
        )
      ),
      shiny::fluidRow(
        column(
          3,
          h4("ROIs selected:"),
          textOutput("nroi"),
          actionButton("record", "Record ROI")
        ),
        column(
          3,
          actionButton("stopselecting", "Stop selecting ROI(s)")
        )
      )
    )
  )



  ts <- shiny::shinyApp(ui, server)
  runApp(ts)
  return(crpout)
}


big_roi_server <- function(input, output, session) {
  image <- get("image", envir = specimEnv)
  bigRoiTry <- get("bigRoiTry", envir = specimEnv)

  # im <- get("im",envir = specimEnv)

  brushExtent <- reactiveValues()
  click <- reactiveValues()
  dblclick <- reactiveValues()
  rois <- reactiveValues()
  nroi <- reactiveVal(0) # rv <- reactiveValues(value = 0)
  output$nroi <- renderText({
    nroi() # rv$value
  })

  click$x <- bigRoiTry@xmin
  click$y <- bigRoiTry@ymin
  dblclick$x <- bigRoiTry@xmax
  dblclick$y <- bigRoiTry@ymax

  init.extent <- bigRoiTry

  brushExtent$extent <- init.extent

  output$plot2 <- renderPlot(
    {
      terra::plotRGB(image, axes = FALSE, stretch = "hist", addfun = cropViewFun(click, dblclick), maxpixels = prod(dim(image)[1:2]) * (input$slider / 100))
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

  # observeEvent(input$record, {
  #
  #   newRoi <- nroi()+1
  #   nroi(newRoi)
  #   xmin <- min(click$x,dblclick$x)
  #   xmax <- max(click$x,dblclick$x)
  #   ymin <- min(click$y,dblclick$y)
  #   ymax <- max(click$y,dblclick$y)
  #   crp <- raster::extent(c(xmin,xmax,ymin,ymax))
  #   crpout[[newRoi]] <<- crp
  #
  # })

  observeEvent(input$stopselecting, {
    if (nroi() == 0) { # if none are selected, select this one.
      newRoi <- nroi() + 1
      nroi(newRoi)
      xmin <- min(click$x, dblclick$x)
      xmax <- max(click$x, dblclick$x)
      ymin <- min(click$y, dblclick$y)
      ymax <- max(click$y, dblclick$y)
      crp <- terra::ext(c(xmin, xmax, ymin, ymax))
      bigcropout <<- crp
    }
    shiny::stopApp()
  })
}


pick_big_roi_shiny <- function(image, bigRoiTry = terra::ext(image), zh = 5000) {
  # assign image into specimEnv
  assign("image", image, envir = specimEnv)
  assign("bigRoiTry", bigRoiTry, envir = specimEnv)

  ui <- shiny::fluidPage(
    # Some custom CSS for a smaller font for preformatted text
    tags$head(
      tags$style(HTML("
      pre, table.table {
        font-size: smaller;
      }
    "))
    ),
    # textOutput("value"),

    column(
      width = 12,
      h2("Select large ROI (the largest box you can draw that selects only mud)"),
      shiny::fluidRow(
        column(
          width = 8, style = "overflow-y:scroll; max-height: 600px",
          plotOutput("plot2",
            height = zh,
            click = "image_click",
            dblclick = dblclickOpts(
              id = "image_dblclick"
            )
          )
        ),
      ),
      shiny::fluidRow(
        column(
          width = 3,
          sliderInput("slider", h4("Left image resolution (% of max)"),
            min = 1, max = 100, step = 5,
            value = 1
          )
        ),
        column(
          width = 3,
          numericInput("x1",
            h4("Click x"),
            value = bigRoiTry@xmin
          ),
          numericInput("y1",
            h4("Click y"),
            value = bigRoiTry@ymin
          )
        ),
        column(
          width = 3,
          numericInput("x2",
            h4("Dbl-Click x"),
            value = bigRoiTry@xmax
          ),
          numericInput("y2",
            h4("Dbl-Click y"),
            value = bigRoiTry@ymax
          )
        ),
        column(
          3,
          numericInput("width",
            h4("Specify width"),
            value = 200
          ),
          actionButton("applyWidth", "Apply width"),
          actionButton("center", "Center ROI"),
        )
      ),
      shiny::fluidRow(
        column(
          3,
          actionButton("stopselecting", "Confirm ROI")
        )
      )
    )
  )



  ts <- shiny::shinyApp(ui, big_roi_server)
  runApp(ts)
  return(bigcropout)
}
