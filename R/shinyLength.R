
lengthserver <- function(input, output, session){
  tr.image <- get("tr.image",envir = specimEnv)
  br.image <- get("br.image",envir = specimEnv)
  image.ext <- get("image.ext",envir = specimEnv)

  click <- reactiveValues()
  dblclick <- reactiveValues()
  depth1 <- reactiveValues()
  depth2 <- reactiveValues()


  click$x <- 0
  click$y <- 0
  dblclick$x <- 0
  dblclick$y <- 0
  init.extent <- image.ext
  init.extent[3] <- init.extent[4]-2000


  # output$plot1 <- renderPlot({
  #   raster::plotRGB(image, axes=TRUE, stretch="hist", main="Overview")
  # })

  output$plot2 <- renderPlot({
    raster::plotRGB(tr.image, axes=TRUE, stretch="hist", main="Click on depth marker near top of core")
  })

  output$plot3 <- renderPlot({
    raster::plotRGB(br.image, axes=TRUE, stretch="hist", main="Double Click on depth marker near top of core")
  })

  observeEvent(input$image_click,{
    click$x <- input$image_click$x
    click$y <- input$image_click$y
#    updateNumericInput(session, "x1", value = click$x)
    updateNumericInput(session, "y1", value = click$y)
  }
  )

  observeEvent(input$image_dblclick,{
    dblclick$x <- input$image_dblclick$x
    dblclick$y <- input$image_dblclick$y
#    updateNumericInput(session, "x2", value = dblclick$x)
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




  observeEvent(input$record, {

    ymin <- min(click$y,dblclick$y)
    ymax <- max(click$y,dblclick$y)
    pixLen <- ymax-ymin
    depth1 <- input$depth1
    depth2 <- input$depth2
    cmLen <- abs(depth2-depth1)
    cmPerPix <- cmLen/pixLen
    cmPerPix <<- cmPerPix
    shiny::stopApp()


  })

}


pick_length_shiny <- function(tr.image,br.image,image.ext){

  #assign image into Global (hack for now)
  assign("tr.image",tr.image,envir = specimEnv)
  assign("br.image",br.image,envir = specimEnv)
  assign("image.ext",image.ext,envir = specimEnv)


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
           h2("Depth selector"),
           fluidRow(
             column(width = 4,
                    plotOutput("plot2", height = 800,
                               click = "image_click"
                    )
             ),
             column(width = 4,
                    plotOutput("plot3", height = 800,
                               click = "image_dblclick"
                    )
             )
           ),
           fluidRow(
             column(width = 3,
                    #numericInput("x1",
                    #             h4("Click x"),
                    #             value = 0),
                    numericInput("y1",
                                 h4("Top Click y"),
                                 value = 0),
                    numericInput("depth1",
                                 h4("Top depth in cm"),
                                 value = 0)
             ),
             column(width = 3,
                    #numericInput("x2",
                    #             h4("Dbl-Click x"),
                    #             value = 0),
                    numericInput("y2",
                                 h4("Bottom Click y"),
                                 value = 0),
                    numericInput("depth2",
                                 h4("Bottom depth in cm"),
                                 value = 0)
             ),
           )

    ),
    actionButton("record", "Record length")
  )



  ts <- shinyApp(ui, lengthserver)
  runApp(ts)
  return(cmPerPix)
}
