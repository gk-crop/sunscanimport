source("global.R")
source("helpers.R")
library(DT)

ui <- fluidPage(
  titlePanel("Convert Sunscan file"),
  sidebarLayout(

    sidebarPanel(
      fileInput(inputId= "sunscanfile",
                label ="Chose sunscan file",
                accept=".TXT",
      ),

      uiOutput("dataselector"),
      fileInput(inputId= "plotidfile",
                label ="Chose plot_id file (optional)",
                accept=".TXT",
      ),
      fileInput(inputId= "measureidfile",
                label ="Chose measure_id file (optional)",
                accept=".TXT",
      ),
      fileInput(inputId= "gridfile",
                        label ="Chose grid file (optional)",
                        accept=".TXT",
      ),

      downloadButton("download", "Download converted data"),
      br(), br(),
      h4("Download template files"),
      downloadButton("plotiddownload", "PlotNr↔ID template"),
      downloadButton("measureiddownload", "MeasureNr↔ID template"),
      downloadButton("griddownload", "Grid↔ID template")
    ),

    mainPanel(
      tabsetPanel(
        tabPanel("Overview",
                 h4("Metadata"),
                 tableOutput("meta"),
                 h4("Measurement Count"),
                 tableOutput("measurementcount"),
                 h4("Boxplot of LAI by PlotID & Date"),
                 plotOutput("boxplot"),
                 h4("LAI on Grid and Measurement Path"),
                 plotOutput("raster")),
        tabPanel("Data",
                 dataTableOutput("converted")),
        tabPanel("Summary",
                 dataTableOutput("summary")),
        tabPanel("Measurement",
                 dataTableOutput("measurement")),
        tabPanel("TimeGraphs",
                 numericInput("stripes","Number of colours: ",7,1,12),
                 h4("Measurement Nr / PlotNr vs. Measurement Time"),
                 plotOutput("timeplot"),
                 h4("LAI vs.  Measurement Time"),
                 plotOutput("timeplotLAI")),
        tabPanel("PlotNr↔ID",
                 dataTableOutput("plotid"),
                 actionButton("createplotid","Create PlotNr↔ID")),
        tabPanel("Measurement↔ID",
                 dataTableOutput("measureid"),
                 br(),
                 wellPanel(
                   fluidRow(
                     column(5, actionButton("createmeasureid","Create Measurement↔ID")),
                     column(4, checkboxInput("useinterval","Use Time Interval",FALSE),
                            helpText("Increment PlotID if the time difference to previous measurement is bigger than interval.")),
                     column(3, numericInput("interval",label="Min. Seconds between Plots",value=25,min=0))
                     )
                   )
                 ),
        tabPanel("Grid↔ID",
                 plotOutput("gridplot"),
                 dataTableOutput("grid"),
                 br(),
                 wellPanel(
                   fluidRow(
                     column(3,numericInput("gridrows","Rows", value=1)),
                     column(3,checkboxInput("gridrowwise","Row-wise", value=FALSE)),
                     column(3,actionButton("creategrid","Create Grid↔ID"))
                   )
                 )
                )
           )
    )

  )

)

server <- function(input, output, session) {


  val <- reactiveValues()
  val$gridchanged <- FALSE
  val$grid <- NULL
  val$plotdatachanged <- FALSE
  val$plotdata <- NULL
  val$measuredatachanged <- FALSE
  val$measuredata <- NULL

  val$datasets <- 1


  fname <- reactive({
    ds<-ifelse(val$datasets > 1,paste0("_", input$dataset), '')
    paste0(stripFileExtension(filenameFromInput(input$sunscanfile)), ds, ".TXT")
  })



  datalines <- reactive({
    validate(need(input$sunscanfile!="", "Please load a sunscan file."))
    lns <- getLinesFromFile(req(input$sunscanfile))
    nr <- length(lns)
    val$datasets <- nr
    titles <- sapply(lns, function(l) getSmallHeader(l)['Title'])
    ch <- 1:nr
    names(ch) <- paste0(ch,'. ', titles)
    if(nr>0) {
      output$dataselector <-  renderUI(selectInput("dataset","Select Dataset", choices = ch, selected=1,multiple=FALSE))
    }
    else {
      output$dataselector <- renderUI(helpText("No valid dataset found"))
    }
    lns
  })

  meta <- reactive({
    dl <- datalines()
    i<- as.integer(req(input$dataset))
    if(i>0 && i <=length(dl))
    {
      m<-getSmallHeader(dl[[i]])
      data.frame(Property=names(m), Value=m)
    }
  })


  df <- reactive(getDataFromFile(datalines(), req(input$dataset)))

  plotdata <- reactive({
    if(!val$plotdatachanged) {
      val$plotdata<-getIdFromFile(input$plotidfile)
    }
    val$plotdata
  })

  measuredata <- reactive({
    if(!val$measuredatachanged) {
      val$measuredata <- getIdFromFile(input$measureidfile)
    }
    val$measuredata
  })

  griddata <- reactive({
    if(!val$gridchanged) {
      val$grid <- getIdFromFile(input$gridfile)
    }
    validate(need(!is.null(val$grid), "Please load gridfile or create griddata"))
    val$grid
  })

  df_id <- reactive(mergeID(df(),plotdata(), measuredata()))

  observeEvent(input$plotidfile, {
    val$plotdatachanged <-FALSE
    val$measuredatachanged <- FALSE
  })

  observeEvent(input$measureidfile, {
    val$measuredatachanged <-FALSE
  })

  observeEvent(input$gridfile, {
    val$gridchanged <-FALSE
  })

  observeEvent(input$plotid_cell_edit, {
    val$plotdata <- editData(val$plotdata, input$plotid_cell_edit, 'plotid')
    val$plotdatachanged <-TRUE
    val$measuredatachanged <-TRUE
    val$measuredata <-NULL
  })

  observeEvent(input$measureid_cell_edit, {
    val$measuredata <- editData(val$measuredata, input$measureid_cell_edit, 'measureid')
    val$measuredatachanged <-TRUE
  })

  observeEvent(input$grid_cell_edit, {
    val$grid <- editData(val$grid, input$grid_cell_edit, 'grid')
    val$gridchanged <-TRUE
  })

  observeEvent(input$createplotid, {
    val$plotdata <- generateSamplePlotIdData(df_id())
    val$plotdatachanged <-TRUE
  })

  observeEvent(input$createmeasureid, {
    val$measuredata <- generateSampleMeasurementIdData(df_id(),if(input$useinterval){input$interval}else{0})
    val$measuredatachanged <-TRUE
  })

  observeEvent(input$creategrid, {
    val$grid <- generateInitialGridData(df_id(), input$gridrows, input$gridrowwise)
    val$gridchanged <-TRUE
  })


  output$meta <- renderTable(meta())
  output$measurementcount <- renderTable(countMeasurements(df_id()))
  output$converted <- renderDataTable(df_id())
  output$summary <- renderDataTable(createSummary(df_id()))
  output$measurement <- renderDataTable(createSeriesInfo(df_id()))
  output$plotid <- renderDataTable((plotdata()), editable=list(target="column", disable=list(columns=1)))
  output$measureid <- renderDataTable((measuredata()), editable=list(target="column", disable=list(columns=1)))
  output$grid <- renderDataTable(griddata(), editable=list(target="column"))


  output$boxplot <- renderPlot(createBoxplot(df_id()))
  output$raster <- renderPlot(createGridPlotLAI(df_id(), griddata()))
  output$timeplot <- renderPlot(createTimePlot(df_id(), input$stripes))
  output$timeplotLAI <- renderPlot(createTimePlotLAI(df_id(), input$stripes))
  output$gridplot <- renderPlot(createGridPlot(griddata()))


  output$download <- downloadHandler(filename=function() paste0("converted_",fname()),
                                    function(file) {
                                      write.table(df_id(), file, row.names=FALSE,sep="\t",quote=FALSE)
                                    })
  output$plotiddownload <- downloadHandler(filename=function() paste0("plotid_",fname()),
                                    function(file) {
                                      write.table(generateSamplePlotIdData(df_id()), file, row.names=FALSE, sep="\t", quote=FALSE)
                                    })
  output$measureiddownload <- downloadHandler(filename=function() paste0("measureid_",fname()),
                                    function(file) {
                                      write.table(generateSampleMeasurementIdData(df_id()), file,row.names=FALSE, sep="\t", quote=FALSE)
                                    })
  output$griddownload <- downloadHandler(filename=function() paste0("grid_",fname()),
                                    function(file) {
                                      if(is.null(val$grid)) {
                                        val$grid<-generateInitialGridData(df_id(), input$gridrows, input$gridrowwise)
                                      }
                                      write.table(val$grid, file, row.names=FALSE, sep="\t", quote=FALSE)
                                    })
}


shinyApp(ui, server)
