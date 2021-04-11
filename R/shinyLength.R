
lengthserver <- function(input, output, session){
  image <- get("image",envir = specimEnv)

  click <- reactiveValues()
  dblclick <- reactiveValues()
  depth1 <- reactiveValues()
  depth2 <- reactiveValues()


  click$x <- 0
  click$y <- 0
  dblclick$x <- 0
  dblclick$y <- 0


  output$plot2 <- renderPlot({
    raster::plotRGB(image, axes=FALSE, stretch="hist", maxpixels = prod(dim(image)[1:2])*(input$slider/100))},
    width = 500
  )


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

  # output$click_info <- renderPrint({
  #   cat("click:\n")
  #   str(c(click$x,click$y))
  # })
  #
  # output$dblclick_info <- renderPrint({
  #   cat("dblclick:\n")
  #   str(c(dblclick$x,dblclick$y))
  # })




  observeEvent(input$record, {

    ymin <- min(click$y,dblclick$y)
    ymax <- max(click$y,dblclick$y)
    pixLen <- ymax-ymin
    depth1 <- min(c(input$depth1,input$depth2))
    depth2 <- max(c(input$depth1,input$depth2))

    clickDepths <<- tibble::tibble(position = c("coreLinerTop","coreLinerBottom"),
                                  pixel = c(ymax,ymin),
                                  cm = c(depth1, depth2))

    cmLen <- abs(depth2-depth1)
    cmPerPix <- cmLen/pixLen

    cmPerPix <<- cmPerPix
    shiny::stopApp()


  })

}


pick_length_shiny <- function(image,zh = 5000){

  #assign image into Global (hack for now)
  assign("image",image,envir = specimEnv)


  ui <- fluidPage(
    # Some custom CSS for a smaller font for preformatted text
    tags$head(
      tags$style(HTML("
      pre, table.table {
        font-size: smaller;
      }
    "))
    ),

    column(width = 12,
           h2("Depth selector"),
           shiny::fluidRow(
             column(width = 8, style = "overflow-y:scroll; max-height: 600px",
                    plotOutput("plot2", height = zh,
                               click = "image_click",
                               dblclick = dblclickOpts(
                                 id = "image_dblclick"
                               ))
             )
           ),
           fluidRow(
             column(width = 3,
                    sliderInput("slider", h4("Left image resolution (% of max)"),
                                min = 1, max = 100,step = 5,
                                value = 80)
             ),
             column(width = 3,
                    #numericInput("x1",
                    #             h4("Click x"),
                    #             value = 0),
                    numericInput("y1",
                                 h4("Core liner top Click y"),
                                 value = 0),
                    numericInput("depth1",
                                 h4("Core liner top depth in cm"),
                                 value = 0)
             ),
             column(width = 3,
                    #numericInput("x2",
                    #             h4("Dbl-Click x"),
                    #             value = 0),
                    numericInput("y2",
                                 h4("Core liner bottom Dbl-Click y"),
                                 value = 0),
                    numericInput("depth2",
                                 h4("Core liner bottom depth in cm"),
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
