#'---
#' title: "Lai Measurements with SunScan: `r params$file`"
#' author: ""
#'---
#'

#+ echo=FALSE, message=FALSE
library(sunscanimport)

if(exists('params')) {
  file <- params$file
  inputfolder <- params$inputfolder
  outputfolder <- params$outputfolder
  } else {
  file <- "paulinenaue-210907.TXT"
  inputfolder <- "data/LAI-Data/Dakis/Paulinenaue/2021/210908/original"
  outputfolder <- "data/LAI-Data/Dakis/Paulinenaue/2021/210908/"
  }
alldata <- readConvertedFiles(file, outputfolder)

#' 
#' # Metadata of converted file
#' 
#+ echo=FALSE
knitr::kable(alldata$header)
mydata <- alldata$data
if(is.null(mydata))
{
  mydata <- alldata$pardata
}

#' 
#' # First Rows of data
#' 
#+ echo=FALSE
try(
  knitr::kable(head(mydata))
)

#'
#' # Graphs LAI
#'
#' ## Boxplot
#' 
#' The graph is split to subgraphs in order to wrap it.
#+ echo=FALSE, fig.width=10
try(
  createBoxplot(mydata)
)



#' ## Visualise LAI spatially
#' 
#' The path indicates the order in which the plots were measured.
#' 
#+ echo=FALSE, fig.width=10, message=FALSE

npath <- gsub("/original","/proceeding",inputfolder)
grid <- paste0(npath,"/grid_",stripFileExtension(file),".TXT")

suppressPackageStartupMessages(library(dplyr))


if (file.exists(grid)) {
  
  griddata <- readr::read_delim(grid, delim="\t", show_col_types = FALSE)
  createGridPlotLAI(mydata, griddata)
} else {
  cat('No grid information available')
}


#'
#' # Measurement times and LAI
#'
#+ echo=FALSE, fig.width=10, message=FALSE

createTimePlot(mydata)
createTimePlotLAI(mydata)

#'
#' # Check number of measurement series on a plot
#'

#+ echo=FALSE
series <- NA
try({

  series <- createSeriesInfo(mydata)
  
  
  
  knitr::kable(series)
  
})


