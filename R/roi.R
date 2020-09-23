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

server <- function(input, output){

  brushExtent <- reactiveValues()
  click <- reactiveValues()
  dblclick <- reactiveValues()

  click$x <- 0
  click$y <- 0
  dblclick$x <- 0
  dblclick$y <- 0

  brushExtent$extent <- raster::extent(image)

  output$plot1 <- renderPlot({
    raster::plotRGB(image, axes=TRUE, stretch="hist", main="Overview",addfun = cropViewFun(click,dblclick))
  })

  output$plot2 <- renderPlot({
    raster::plotRGB(image, axes=TRUE, stretch="hist", main="Zoom Control")
  })

  output$plot3 <- renderPlot({
    raster::plotRGB(image, axes=TRUE, stretch="hist", main="Select Crop Corners",ext = brushExtent$extent)
  })

  # If so, zoom to the brush bounds; if not, reset the zoom.
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
  }
  )

  observeEvent(input$image_dblclick,{
    dblclick$x <- input$image_dblclick$x
    dblclick$y <- input$image_dblclick$y
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
                    plotOutput("plot1", height = 600)
             ),
             column(width = 4,
                    plotOutput("plot2", height = 600,
                               brush = brushOpts(
                                 id = "plot2_brush",
                                 resetOnNew = TRUE
                               )
                    )
             ),
             column(width = 4,
                    plotOutput("plot3",
                               height = 600,
                               click = "image_click",
                               dblclick = dblclickOpts(
                                 id = "image_dblclick"
                               )
                    )
             )
           ),
           fluidRow(
             column(width = 6,
                    verbatimTextOutput("click_info")
             ),
             column(width = 6,
                    verbatimTextOutput("dblclick_info")
             )
           )


    ),
    actionButton("record", "Record ROI")
  )



ts <- shinyApp(ui, server)
runApp(ts)
return(crpout)
}
