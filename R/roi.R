pick_roi_simple <- function(image){
  raster::plotRGB(image, axes=TRUE, stretch="hist", main="Raw Image")
  cropC <- raster::drawExtent()
  return(cropC)
}

cropViewFun <- function(click,dblclick){
  xmin <- min(click$x,dblclick$x)
  xmax <- max(click$x,dblclick$x)
  ymin <- min(click$y,dblclick$y)
  ymax <- max(click$y,dblclick$y)

  xy <- data.frame(x = c(xmin,xmax,xmax,xmin),y = c(ymin,ymin,ymax,ymax))
  polygon(xy,col=NA,border = "red")
  clickpoints <- rbind(c(click$x,click$y),c(dblclick$x,dblclick$y))
  points(clickpoints,cex = 2)
  points(clickpoints, pch=3, col='green')
}

server <- function(input, output, session){

  brushExtent <- reactiveValues()
  click <- reactiveValues()
  dblclick <- reactiveValues()

  click$x <- 0
  click$y <- 0
  dblclick$x <- 0
  dblclick$y <- 0

  init.extent <- raster::extent(image)
  init.extent[3] <- init.extent[4]-2000

  brushExtent$extent <- init.extent

  # output$plot1 <- renderPlot({
  #   raster::plotRGB(image, axes=TRUE, stretch="hist", main="Overview")
  # })

  output$plot2 <- renderPlot({
    raster::plotRGB(image, axes=TRUE, stretch="hist", main="Overview/Zoom Control",addfun = cropViewFun(click,dblclick))
  })

  output$plot3 <- renderPlot({
    raster::plotRGB(image, axes=TRUE, stretch="hist", main="Select Crop Corners",ext = brushExtent$extent)
  })

  # If so, zoom to the brush bounds
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      brushExtent$extent <- raster::extent(brush$xmin, brush$xmax,brush$ymin, brush$ymax)

    } else {
      brushExtent$extent <- raster::extent(image)
    }

    brushExtent$brushBox <- data.frame(x = c(brush$xmin, brush$xmax,brush$xmax, brush$xmin),
                           y = c(brush$ymin, brush$ymin,brush$ymax, brush$ymax))




  })

  observeEvent(input$image_click,{
    click$x <- input$image_click$x
    click$y <- input$image_click$y
    updateNumericInput(session, "x1", value = click$x)
    updateNumericInput(session, "y1", value = click$y)
  }
  )

  observeEvent(input$image_dblclick,{
    dblclick$x <- input$image_dblclick$x
    dblclick$y <- input$image_dblclick$y
    updateNumericInput(session, "x2", value = dblclick$x)
    updateNumericInput(session, "y2", value = dblclick$y)
  }
  )

  output$click_info <- renderPrint({
    cat("click:\n")
    str(c(click$x,click$y))
  })

  output$dblclick_info <- renderPrint({
    cat("dblclick:\n")
    str(c(dblclick$x,dblclick$y))
  })

  observeEvent(input$applyWidth, {
    if(click$x >= dblclick$x){
      click$x <- dblclick$x+input$width
    }else if(dblclick$x > click$x){
      dblclick$x <- click$x+input$width
    }
  })

  observeEvent(input$center, {
    width <- abs(click$x - dblclick$x)
    re <- raster::extent(image)
    totalWidth <- re[2]-re[1]
    mid <- round(re[2]/2)
    if(click$x >= dblclick$x){
      click$x <- round(mid+width/2)
      dblclick$x <- round(mid-width/2)
    }else if(dblclick$x > click$x){
      dblclick$x <- round(mid+width/2)
      click$x <- round(mid-width/2)    }
  })

  observeEvent(input$record, {

    xmin <- min(click$x,dblclick$x)
    xmax <- max(click$x,dblclick$x)
    ymin <- min(click$y,dblclick$y)
    ymax <- max(click$y,dblclick$y)
    crp <- raster::extent(c(xmin,xmax,ymin,ymax))
    crpout <<- crp
    shiny::stopApp()


  })

}


pick_roi_shiny <- function(image){

  #assign image into Global (hack for now)
  assign("image",image,envir = .GlobalEnv)


  ui <- fluidPage(
    # Some custom CSS for a smaller font for preformatted text
    tags$head(
      tags$style(HTML("
      pre, table.table {
        font-size: smaller;
      }
    "))
    ),

    column(width = 12, class = "well",
           h2("Region of interest (ROI) selector"),
           fluidRow(
             column(width = 4,
                    plotOutput("plot2", height = 500,
                               brush = brushOpts(
                                 id = "plot2_brush",
                                 resetOnNew = FALSE
                               )
                    )
             ),
             column(width = 8,
                    plotOutput("plot3",
                               height = 500,
                               click = "image_click",
                               dblclick = dblclickOpts(
                                 id = "image_dblclick"
                               )
                    )
             )
           ),
           fluidRow(
             column(width = 3,
                    numericInput("x1",
                                 h4("Click x"),
                                 value = 0),
                    numericInput("y1",
                                 h4("Click y"),
                                 value = 0)
             ),
             column(width = 3,
                    numericInput("x2",
                                 h4("Dbl-Click x"),
                                 value = 0),
                    numericInput("y2",
                                 h4("Dbl-Click y"),
                                 value = 0)
             ),

             column(3,
                    numericInput("width",
                                 h4("Specify width"),
                                 value = 200),
                    actionButton("applyWidth", "Apply width"),
                    actionButton("center", "Center ROI"),
             )

           )

    ),
    actionButton("record", "Record ROI")
  )



ts <- shinyApp(ui, server)
runApp(ts)
return(crpout)
}
