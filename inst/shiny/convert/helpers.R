filenameFromInput <- function(inp) if(!is.null(inp)){inp[1,"name"]}else{''}

pathFromInput <- function(inp) ifelse(!is.null(inp),inp[1,"datapath"],'')

getLinesFromFile <- function(fileinput)
{
  linelist <- list()
  path <- pathFromInput(fileinput)
  if(file.exists(path) && isSunscanFile(path) ) {
    
    alllines <- readLines(path)
    linelist <- splitLines(alllines)
    print(paste("Read lines",path))
  }
  linelist
}

getDataFromFile <- function(datalines, nr){
  df <- NULL
  if(length(datalines)>=nr && nr>0)
  {
    lines <- datalines[[as.integer(nr)]]
    mt <- getSmallHeader(lines)
    if(length(lines)>13 )
    {        
      if(grepl("Trans-",lines[[12]],fixed=TRUE)){
        df <- getData(lines,mt['Date'])
      }
      else {
        df <- getParData(lines,mt['Date'])
      }
    }
  }
  df
}

getIdFromFile <- function(fileinput) {
  path = pathFromInput(fileinput)
  if(path!='')  print(paste("Read id file",path))
  readIdData(path)
}

mergeID <- function(df, plotdata, measuredata) {
  reorderDataColumns(addPlotIDFromData(df, plotdata, measuredata))
}
