source("global.R")
source("helpers.R")

sample_grids <- c("file",basename(list.files(system.file("grids", package="sunscanimport"))))
names(sample_grids) <- c("[From File]",stripFileExtension(sample_grids[-1]))

tableOptions <- list(
  lengthMenu = list(c(10,25,100,200,-1),c("10","25","100","200","All")),
  pageLength=25
)

tableOptionsEdit <-tableOptions
tableOptionsEdit$pageLength=200

ui <- fluidPage(
  titlePanel("Convert Sunscan file"),
  sidebarLayout(

    sidebarPanel(
      fileInput(inputId= "sunscanfile",
                label ="Choose sunscan file",
                accept=".TXT",
      ),

      uiOutput("dataselector"),
      fileInput(inputId= "plotidfile",
                label ="Choose plot_id file (optional)",
                accept=".TXT",
      ),
      fileInput(inputId= "measureidfile",
                label ="Choose measure_id file (optional)",
                accept=".TXT",
      ),

      selectInput(inputId="gridfileinternal",
                  label="Choose plot arrangement grid (optional)",
                  choices = sample_grids
      ),
      conditionalPanel(
        condition = "input.gridfileinternal == 'file'",
        fileInput(inputId= "gridfile",
                  label ="Load grid from file",
                  accept=".TXT",
        )
      ),

      downloadButton("zipdownload", "Download all files as Zip"),
      br(), br(),
      h4("Download Data"),
      downloadButton("download", "Download converted data"),br(),
      downloadButton("downloadsummary", "Summary"),
      downloadButton("downloadmeta", "Meta-Data"),
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
  val$gridfileinternal <- FALSE
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
      m<-c(
        getSmallHeader(dl[[i]])
      )
      data.frame(Property=names(m), Value=m)
    }
  })

  metafull <- reactive({
    dl <- datalines()
    i<- as.integer(req(input$dataset))
    if(i>0 && i <=length(dl))
    {
      m <- c(
        getSmallHeader(dl[[i]]),
        OriginalFileName = req(input$sunscanfile$name),
        OriginalMD5Hash = digest::digest(file=req(input$sunscanfile$datapath),algo="md5"),
        ConversionDate = as.character(Sys.time()),
        ConversionTool = paste0("Package sunscanimport - version ",packageVersion("sunscanimport")," - (c) Gunther Krauss")
      )
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
      file <-
      if(val$gridfileinternal && input$gridfileinternal!="file")
      {
        file <- system.file("grids", input$gridfileinternal,package="sunscanimport")
        val$grid <- readIdData(file)
      }
      else
      {
        val$grid <- getIdFromFile(input$gridfile)
      }

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
    val$gridfileinternal <- FALSE
  })

  observeEvent(input$gridfileinternal, {
    val$gridchanged <-FALSE
    val$gridfileinternal <- TRUE
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
  output$converted <- renderDataTable(df_id(), options=tableOptions)
  output$summary <- renderDataTable(createSummary(df_id()), options=tableOptions)
  output$measurement <- renderDataTable(createSeriesInfo(df_id()), options=tableOptions)
  output$plotid <- renderDataTable((plotdata()), editable=list(target="column", disable=list(columns=1)), options=tableOptionsEdit)
  output$measureid <- renderDataTable((measuredata()), editable=list(target="column", disable=list(columns=1)), options=tableOptionsEdit)
  output$grid <- renderDataTable(griddata(), editable=list(target="column"), options=tableOptionsEdit)


  output$boxplot <- renderPlot(createBoxplot(df_id()))
  output$raster <- renderPlot(createGridPlotLAI(df_id(), griddata()))
  output$timeplot <- renderPlot(createTimePlot(df_id(), input$stripes))
  output$timeplotLAI <- renderPlot(createTimePlotLAI(df_id(), input$stripes))
  output$gridplot <- renderPlot(createGridPlot(griddata()))


  output$download <- downloadHandler(
    filename=function() paste0("data_",fname()),
    content = function(file) {
      write.table(df_id(), file, row.names=FALSE,sep="\t",quote=FALSE)
    })
  output$downloadsummary <- downloadHandler(
    filename=function() paste0("summary_",fname()),
    content = function(file) {
      write.table(createSummary(df_id()), file, row.names=FALSE,sep="\t",quote=FALSE)
    })
  output$downloadmeta <- downloadHandler(
    filename=function() paste0("meta_",fname()),
    content = function(file) {
      write.table(metafull(), file, row.names=FALSE,sep="\t",quote=FALSE)
    })
  output$plotiddownload <- downloadHandler(
    filename=function() paste0("plotid_",fname()),
    content = function(file) {
      write.table(generateSamplePlotIdData(df_id()), file, row.names=FALSE, sep="\t", quote=FALSE)
    })
  output$measureiddownload <- downloadHandler(
    filename=function() paste0("measureid_",fname()),
    content = function(file) {
      write.table(generateSampleMeasurementIdData(df_id()), file,row.names=FALSE, sep="\t", quote=FALSE)
    })
  output$griddownload <- downloadHandler(
    filename=function() paste0("grid_",fname()),
    content = function(file) {
      if(is.null(val$grid)) {
        val$grid<-generateInitialGridData(df_id(), input$gridrows, input$gridrowwise)
      }
      write.table(val$grid, file, row.names=FALSE, sep="\t", quote=FALSE)
    })

  
  output$zipdownload <- downloadHandler(
    filename=function(){paste0(stripFileExtension(fname()),'.zip')},
    content = function(file) {
      of <- fname()
      mt <-metafull()
      tmpdir <- paste0(tempdir(),'/',stripFileExtension(of))
      dir.create(tmpdir,showWarnings=FALSE)
      dir.create(paste0(tmpdir,'/original'), showWarnings=FALSE)
      dir.create(paste0(tmpdir,'/proceeding'), showWarnings=FALSE)
      dir.create(paste0(tmpdir,'/converted'), showWarnings=FALSE)
      
      fn <- c(
        'original' = paste0(tmpdir,'/original/',input$sunscanfile$name),
        'meta' = paste0(tmpdir,'/converted/meta_',of),
        'plotid' = paste0(tmpdir,'/proceeding/plotid_',of),
        'measureid' = paste0(tmpdir,'/proceeding/measureid_',of),
        'gridid' = paste0(tmpdir,'/proceeding/grid_',of)
      )
      file.copy(input$sunscanfile$datapath,fn['original'])
      
      if(mt[mt$Property=='MeasuredVariable','Value']=='LAI')
      {
        fn <- c(fn,
                'data' = paste0(tmpdir,'/converted/data_',of),
                'summary' = paste0(tmpdir,'/converted/summary_',of)
        )
        write.table(df_id(), fn['data'], row.names=FALSE,sep="\t",quote=FALSE)
        write.table(createSummary(df_id()), fn['summary'], row.names=FALSE,sep="\t",quote=FALSE)
      }
      else {
        fn <- c(
          fn,
          'data' = paste0(tmpdir,'/converted/pardata_',of)
        )
        write.table(df_id(), fn['data'], row.names=FALSE,sep="\t",quote=FALSE)
        
      }
      write.table(metafull(), fn['meta'], row.names=FALSE,sep="\t",quote=FALSE)
      write.table(generateSamplePlotIdData(df_id()), fn['plotid'], row.names=FALSE, sep="\t", quote=FALSE)
      write.table(generateSampleMeasurementIdData(df_id()), fn['measureid'],row.names=FALSE, sep="\t", quote=FALSE)
      if(is.null(val$grid)) {
        val$grid<-generateInitialGridData(df_id(), input$gridrows, input$gridrowwise)
      }
      write.table(val$grid, fn['gridid'], row.names=FALSE, sep="\t", quote=FALSE)
      zip::zip(zipfile=file, files=c('original','converted','proceeding'), recurse=TRUE, root = tmpdir ,mode="cherry-pick")
    },
    contentType = "application/zip")
  
}


shinyApp(ui, server)
