#' Checks if given file is a valid SunScan file
#'
#' Checks if file starts with 'Created by SunData'
#' and if the files has at least 14 lines
#'
#' @param path path to the file
#' @return TRUE if valid SunScan file
#'
#' @export
isSunscanFile <- function(path) {
  lines <- readLines(path,n=15)
  checkString <- "Created by SunData"
  length(lines)>=14 && startsWith(lines[[1]],checkString)
}


#' Checks if the line is a data line
#'
#' Data lines begin with a date
#'
#' @param line a line of text
#'
#' @return TRUE if data line
#' @export

isDataLine <- function(line) {
  length(line)>=10 && substr(line[1],1,1) %in% c(0,1,2)
}


#' Checks if the line is a title line
#'
#' A title line starts a new metatdata section in the
#' sunscan data file.
#'
#' @param line a line of text
#' @return TRUE if title line
#' @export

isTitleLine <- function(line) {
  checkString <- "Title"
  startsWith(line, checkString)
}

#' Strips the file extension
#'
#' @param file filename
#' @return file name without path and extension
#' @export

stripFileExtension <- function(file) {
  gsub(pattern = "\\.[a-zA-Z0-7]+$", "", basename(file))
}


#' Checks if the data file has been moved to subfolder 'original'
#'
#' @param file filename
#' @param inputfolder input folder
#' @param target target folder
#' @return TRUE if file has been moved to original folder
#' @export

isMovedFile <- function(file, inputfolder, target="original") {
  folder <- dirname(paste0(inputfolder,file))
  marker <- paste0("/", target)
  endsWith(folder, marker)
}


#' Returns path for the data file moved to 'original' subfolder
#'
#' @param file filename
#' @param inputfolder input folder
#' @param target target folder
#'
#' @return path to data file
#' @export

movedFileName <- function(file, inputfolder, target="original") {
  if(!isMovedFile(file, inputfolder, target)) {
    path <- paste0(inputfolder, file)
    paste0(dirname(path),"/",target,"/", basename(file))
  }
  else {
    paste0(inputfolder,file)
  }
}


#' Moves data file to subfolder 'original'
#'
#' @param file filename
#' @param inputfolder input folder
#' @param target target folder
#' @export

moveOriginalFile <- function(file, inputfolder, target="original") {
  if(!isMovedFile(file, inputfolder, target))
  {
    path <- paste0(inputfolder,file)
    if(file.exists(path)) {
      file.rename(path,
                movedFileName(file, inputfolder,target))
    }
  }
}


#' Moves data file to subfolder 'original'
#'
#' @param file filename
#' @param inputfolder input folder
#' @param target target folder
#' @export

moveFilesToSubfolders <- function(file, inputfolder, target="original") {
  moveOriginalFile(file, inputfolder, target)
  plotid <- paste0("plotid_",basename(file))
  moveOriginalFile(plotid, inputfolder, "proceeding")
  measureid <- paste0("measureid_",basename(file))
  moveOriginalFile(measureid, inputfolder, "proceeding")
  grid <- paste0("grid_",basename(file))
  moveOriginalFile(grid, inputfolder, "proceeding")
}



#' Creates (sub)folders for converted data and reports
#'
#' @param file filename
#' @param inputfolder input folder
#' @param outputfolder output folder
#' @export

createFolders <- function(file, inputfolder, outputfolder) {
  convfolder <- paste0(outputfolder,"converted")
  reportfolder <- paste0(outputfolder,"report")
  if(!dir.exists(convfolder)) {
    print(paste("Creating folder",convfolder))
    dir.create(convfolder, recursive=TRUE)
  }
  if(!dir.exists(reportfolder)) {
    print(paste("Creating folder",reportfolder))
    dir.create(reportfolder, recursive=TRUE)
  }
  path <- paste0(inputfolder,file)
  if(!isMovedFile(file, inputfolder, "original")) {
    movedata <- paste0(dirname(path),"/","original")
    if(!dir.exists(movedata)) {
      print(paste("Creating folder",movedata))
      dir.create(movedata, recursive=FALSE)
    }
    moveproceedings <- paste0(dirname(path),"/","proceeding")
    if(!dir.exists(moveproceedings)) {
      print(paste("Creating folder",moveproceedings))
      dir.create(moveproceedings, recursive=FALSE)
    }

  }
}


#' Gets meta data from the file header
#'
#' @param lines vector of lines read from the data file
#'
#' @return named vector with meta data
#' @export

getSmallHeader <- function(lines) {

  hl <- lapply(lines[1:13],function(line) scan(text=line, sep="\t", what=character(), quiet=TRUE))

  meta <- c(
    Title = hl[[3]][2],
    Location = hl[[4]][2],
    Latitude = hl[[5]][2],
    Longitude = hl[[5]][4],
    Date = hl[[6]][1],
    Timezone = hl[[6]][3],
    LeafAngleDistnParameter = hl[[9]][7],
    LeafAbsorption = hl[[9]][10],
    ExternalSensor = hl[[9]][1],
    Device = hl[[1]][1],
    Firmware = hl[[7]][1],
    MeasuredVariable = ifelse(hl[[12]][9]=="LAI","LAI",hl[[12]][4])
  )
  meta
}



#' Gets meta data from the file header
#'
#' @param lines vector of lines read from the data file
#' @param path path of the file
#'
#' @return named vector with meta data
#' @export

getHeader <- function(lines,path) {
  meta <- c(
    getSmallHeader(lines),
    OriginalFileName = basename(path),
    OriginalPath = path.expand(movedFileName(basename(path),dirname(path),"original")),
    OriginalMD5Hash = digest::digest(file=path,algo="md5"),
    OriginalLastModified = as.character(file.info(path)[1,'mtime']),
    ConversionDate = as.character(Sys.time()),
    ConversionTool = paste0("Package sunscanimport - version ",packageVersion("sunscanimport")," - (c) Gunther Krauss")
  )
  meta
}


#' Extracts LAI data from the file lines
#'
#' @param lines vector of lines read from the data file
#' @param date start date of measurements from meta data
#'
#' @return data.frame with LAI measurements
#' @export

getData <- function(lines,date) {
  # split lines into vector at tabs
  dl <- lapply(lines[14:length(lines)],function(line) scan(text=line, sep="\t", what=character(), quiet=TRUE))
  lasttime <- "00:00:01"
  lastdate <- date

  # create empty dataframe
  dfs <- data.frame(
    Time = character(0),
    PlotNr = integer(0),
    Measurement = integer(0),
    Transmitted = numeric(0),
    Spread=numeric(0),
    Incident=numeric(0),
    BeamFrac=numeric(0),
    ZenithAngle=numeric(0),
    LAI = numeric(0),
    Notes=character(0),
    Date = character(0)

  )
  names <- names(dfs)

  # initialise data.frame and values for incident data lines
  # when no BFS sensor available
  dfincident <- NULL
  incident <- NA
  beamfrac <- NA
  lastPlotNr <- NA

  # process all lines
  for(l in dl)
  {
    if(isDataLine(l)){

      # increment date if time jumps back
      if(l[1]<lasttime)
      {
        lastdate <- as.character(as.Date(lastdate,format="%Y-%m-%d")+1)
      }

      # convert and reorder data to a data.frame row
      nl <- c(l[1],as.list(as.numeric(l[2:9])),l[10],lastdate)
      dfn <- as.data.frame(nl)
      names(dfn) <- names

      # if there is no PlotNr, it is treated as incident measurement
      if(is.na(dfn$PlotNr)) {
        # reset mean incident and beamfrac, add to incidend sub-dataframe
        incident <- NA
        beamfrac <- NA
        dfincident <- rbind(dfincident,dfn)
      }
      else {
        # if there are incident measurements, add PlotNr to
        # them, bind them to the data frame and calculate means
        if(!is.null(dfincident) && nrow(dfincident)>0)
        {
          dfincident$PlotNr <- dfn$PlotNr
          incident <- dplyr::last(dfincident$Incident)
          beamfrac <- dplyr::last(dfincident$BeamFrac)
          dfs <- rbind(dfs, dfincident)
          dfincident <- NULL
        }
        # if there is no incident measure, take the mean from
        # the last incident measures
        if(!is.na(incident)) {
          dfn$Incident <- incident
        }
        # if(!is.na(beamfrac)) {
        #   dfn$BeamFrac <- beamfrac
        # }
        # add normal measurements to the result data frame
        # and keep the last PlotNr
        dfs <- rbind(dfs, dfn)
        lastPlotNr <- dfn$PlotNr
      }
      lasttime <- l[1]
    }
    # when no more data line then add PlotNr to possible
    # incident measurements, attach them to the result data frame
    # and reset incident values
    else {
      if(!is.null(dfincident) && nrow(dfincident)>0)
      {
        dfincident$PlotNr <- lastPlotNr
        dfs <- rbind(dfs, dfincident)
      }
      dfincident <- NULL
      incident <- NA
      beamfrac <- NA
    }
  }

  # add columns for transmitted fraction and datetime
  dfs$TransmittedFrac <- dfs$Transmitted/dfs$Incident
  dfs$DateTime <- as.POSIXct(paste(dfs$Date,dfs$Time))
  dfs$Date <- as.Date(dfs$Date)
  dfs
}


#' Extracts PAR data from the file lines
#'
#' @param lines vector of lines read from the data file
#' @param date start date of measurements from meta data
#'
#' @return data.frame with PAR measurements
#' @export

getParData <- function(lines,date) {
  dl <- lapply(lines[14:length(lines)],function(line) scan(text=line, sep="\t", what=character(), quiet=TRUE))
  lasttime <- "00:00:01"
  lastdate <- date
  dfs <- NULL
  for(l in dl)
  {
    if(isDataLine(l)){
      if(l[1]<lasttime)
      {
        lastdate <- as.character(as.Date(lastdate,format="%Y-%m-%d")+1)
      }
      nl <- c(lastdate,as.list(l))
      dfn <- as.data.frame(nl)
      names(dfn)<-c("Date","Time","PlotNr","Measurement","Par","Spread","Total","Diffuse","Empty1","Empty2","Notes",
                                      paste0("S_",1:63),"Spin_64")

      dfs <- rbind(dfs, dfn)
      lasttime <- l[1]
    }
  }
  if(!is.null(dfs))
  {
    dfs$DateTime <- as.POSIXct(paste(dfs$Date,dfs$Time))
    dfs$Date <- as.Date(dfs$Date)
  }
  dfs
}

#' Read dataframe with PlotID column
#'
#' @param file filename
#' @return data frame with the ids
#' @export

readIdData <- function(file) {
  ids <- NULL
  if(file.exists(file))
  {
    ids <- read.delim(file,sep="\t", colClasses=c(PlotID="character"))
  }
  if("Remarks" %in% names(ids)) {
    ids$Remarks <- as.character(ids$Remarks)
  }
  ids
}


#' Adds PlotID column to LAI data
#'
#' Information about PlotID is given via additional
#' tab delimited files.
#'
#' @param data data.frame with LAI data
#' @param ids dataframe with plotids
#' @param mids dataframe with measurementids
#'
#' @return data.frame with PlotID column
#' @export

addPlotIDFromData <- function(data, ids, mids)
{
  if(!is.null(data) && nrow(data)>0)
  {
    data$PlotID <- NA_character_
    data$Remarks <- NA_character_
    data$Delete <- 0

    # get PlotId for PlotNr
    if(!is.null(ids) && nrow(ids)>0)
    {
      keep <- data |>
        dplyr::anti_join(ids,  by=c("PlotNr"="PlotNr"))
      modify <- data |>
        dplyr::select(-PlotID, -Remarks, -Delete) |>
        dplyr::inner_join(ids, by=c("PlotNr"="PlotNr"))
      if(!("Delete" %in% names(modify))) {
        modify$Delete <- 0
      }
      data <- dplyr::bind_rows(keep, modify) |>
        dplyr::arrange(PlotNr, DateTime)
    }
    else {
      data$Remarks <- "No PlotID file given"
      data$PlotID <- as.character(formatC(data$PlotNr,width=3,format="d",flag="0"))
    }

    # get PlotID for specific measurements
    if(!is.null(mids) && nrow(mids)>0)
    {
      keep <- data |>
        dplyr::anti_join(mids,  by=c("PlotNr"="PlotNr", "Measurement"="Measurement"))
      modify <- data |>
        dplyr::select(-PlotID, -Remarks, -Delete) |>
        dplyr::inner_join(mids, by=c("PlotNr"="PlotNr", "Measurement"="Measurement"))
      if(!("Delete" %in% names(modify))) {
        modify$Delete <- 0
      }
      data <- dplyr::bind_rows(keep, modify) |>
        dplyr::arrange(PlotNr, DateTime)
    }
  }
  data
}


#' Adds PlotID column to LAI data
#'
#' Information about PlotID is given via additional
#' tab delimited files.
#'
#' @param data data.frame with LAI data
#' @param path of the original data
#'
#' @return data.frame with PlotID column
#' @export

addPlotID <- function (data,path) {

  npath <- gsub("/original","/proceeding",dirname(path))
  plotid <- paste0(npath,"/plotid_",stripFileExtension(path),".TXT")
  measid <- paste0(npath,"/measureid_",stripFileExtension(path),".TXT")

  ids <- readIdData(plotid)
  mids <- readIdData(measid)


  addPlotIDFromData(data, ids, mids)

}


#' Reorders columns in converted data
#'
#' @param data imported sunscan data
#' @return dataframe with reordered columns
#' @export
reorderDataColumns <- function(data) {
  if("LAI" %in% names(data))
  {
    data[,c(
    "PlotNr",
    "PlotID",
    "Measurement",
    "DateTime",
    "Date",
    "Time",
    "Transmitted",
    "Spread",
    "Incident",
    "TransmittedFrac",
    "BeamFrac",
    "ZenithAngle",
    "LAI",
    "Notes",
    "Remarks",
    "Delete")]
  }
  else data

}

#' Counts measurements
#' @param data imported sunscan data
#' @return dataframe with number of measurements
#' @export
countMeasurements <- function(data) {
  var <- ifelse("LAI" %in% names(data),"LAI", "Par")
  data |> dplyr::group_by(PlotNr) |>
    dplyr::summarise(MeasNr = dplyr::n(),
              NaMeasNr = sum(is.na(.data[[var]])),
              Delete=sum(Delete),.groups="drop") |>
    dplyr::summarise(Plots=dplyr::n(),
              TotalMeas=sum(MeasNr),
              Min = min(MeasNr),
              Max = max(MeasNr),
              Median = median(MeasNr),
              TotalNA = sum(NaMeasNr),
              MaxNA = max(NaMeasNr),
              Delete = sum(Delete))
}


#' Summarises data for each PlotID
#'
#' @param data data.frame with LAI data
#' @param deleted include measurements marked as deleted
#' @return data.frame summary information
#' @export

createSummary <- function(data, deleted=FALSE) {
  if(!is.null(data) && nrow(data)>0 && 'LAI' %in% names(data))
  {
    if(!deleted && "Delete" %in% names(data)) {
      data <- dplyr::filter(data, Delete==0)
    }
    data |> dplyr::group_by(PlotNr,Date,PlotID) |>
      dplyr::summarise(
        LAI_mean = mean(LAI, na.rm=TRUE),
        LAI_median = median(LAI, na.rm=TRUE),
        LAI_min = min(LAI, na.rm=TRUE),
        LAI_max = max(LAI, na.rm=TRUE),
        LAI_sd = sd(LAI, na.rm=TRUE),
        LAI_realmeasurements = sum(!is.na(LAI)),
        Measurements = dplyr::n(),
        Transmitted = mean(Transmitted, na.rm=TRUE),
        Spread = mean(Spread, na.rm=TRUE),
        Incindent = mean(Incident, na.rm=TRUE),
        ZenithAngle = mean(ZenithAngle, na.rm=TRUE),
        TransmittedFrac = mean(TransmittedFrac, na.rm=TRUE),
        Start = min(DateTime),
        End = max(DateTime),
        Duration = max(DateTime) - min(DateTime),
        Remarks = dplyr::first(Remarks),
        .groups = "drop"
      )
  }
}

#' Summarise information about measurement series
#' @param data imported sunscan data
#' @return dataframe
#' @export
createSeriesInfo <- function(data) {
  data |>
    dplyr::select(PlotID, PlotNr, Measurement, Date, Delete) |>
    dplyr::group_by(PlotID, Date,PlotNr) |>
    dplyr::summarise(Measurements=sum(!is.na(Measurement)),
              NA_Measurements = sum(is.na(Measurement)),
              TotalMeasurements = dplyr::n(),
              ToDelete = sum(Delete),
              .groups="drop") |>
    dplyr::group_by(PlotID, Date) |>
    dplyr::summarise(MeasurementSeries= dplyr::n(),
              Measurements=sum(Measurements),
              NA_Measurements=sum(NA_Measurements),
              TotalMeasurements=sum(TotalMeasurements),
              ToDelete = sum(ToDelete),
              .groups="drop") |>
    dplyr::arrange(dplyr::desc(MeasurementSeries),dplyr::desc(Measurements),dplyr::desc(TotalMeasurements))
}

#' Creates a boxplot for LAI data
#'
#' @param data data.frame with LAI data
#' @param deleted include measurements marked as deleted
#' @return ggplot graph with boxplot
#' @export

createBoxplot <- function(data, deleted=FALSE) {
  if("LAI" %in% names(data))
  {
    if(!deleted && "Delete" %in% names(data)) {
      data <- dplyr::filter(data, Delete==0)
    }
    plotids <- sort(unique(data$PlotID))
    plots <- length(plotids)
    rows <- ceiling(plots/40)
    divisions <- rep(1:rows,40)[1:plots]
    pl <- ggplot2::ggplot(data |> dplyr::left_join(data.frame(PlotID=plotids,div=divisions),by=c("PlotID"="PlotID"))|> dplyr::mutate(Plot=as.factor(PlotID),Date=as.factor(Date))) +
      ggplot2::geom_boxplot(ggplot2::aes(x=Plot, y=LAI,fill=Date),na.rm=TRUE)
    if(plots>50)
    {
      pl <- pl +
        ggplot2::facet_wrap(.~div,scales = "free_x",nrow=rows)
    }
    pl
  }
}

#' Plots LAI on a grid
#' @param data imported sunscan data
#' @param griddata grid data
#' @param deleted include measurements marked as deleted
#' @export

createGridPlotLAI <- function(data, griddata, deleted=FALSE) {
  if (!is.null(griddata) && nrow(griddata)>0 && !is.null(data) && nrow(data)>0 && 'LAI' %in% names(data)) {
    if(!deleted && "Delete" %in% names(data)) {
      data <- dplyr::filter(data, Delete==0)
    }
    summariseddata <- data |>
      dplyr::filter(!is.na(PlotID) & !is.na(LAI)) |>
      dplyr::inner_join(griddata, by=c("PlotID"="PlotID")) |>
      dplyr::group_by(PlotNr, PlotID, Col, Row, Date) |>
      dplyr::summarise(LAI = mean(LAI,na.rm=TRUE), .groups="drop") |>
      dplyr::arrange(PlotNr)
    if(nrow(summariseddata)>0)
    {
    ggplot2::ggplot(summariseddata) +
      ggplot2::geom_raster(ggplot2::aes(y=-Row, x=-Col, fill=LAI)) +
      ggplot2::facet_wrap(.~Date) +
      ggplot2::geom_path(ggplot2::aes(y=-Row, x=-Col) ,colour="#333333", position=ggplot2::position_jitter(.15),
                         arrow=ggplot2::arrow(length = ggplot2::unit(.09,"inches"),type = "closed"),
                         lineend = "square") +
      ggplot2::geom_text(ggplot2::aes(label=PlotID,y=-Row,x=-Col)) +
      ggplot2::scale_fill_gradient(low="#33ff33", high="#008800")
    }
  }
}

#' Plots the PlotNr arranged in a grid
#' @param griddata grid data
#' @export

createGridPlot <- function(griddata) {
  if (!is.null(griddata) && nrow(griddata)>0) {
    ggplot2::ggplot(griddata) +
      ggplot2::geom_raster(ggplot2::aes(y=-Row, x=-Col, fill=Col%%2 + Row %% 2)) +
      ggplot2::geom_text(ggplot2::aes(label=PlotID,y=-Row,x=-Col)) +
      ggplot2::scale_fill_gradient(low="#aaaaaa", high="#eeeeee") +
      ggplot2::guides(fill="none") +
      ggplot2::theme_void()
  }
}

#' Plots measurements over time
#' @param data imported sunscan data
#' @param stripes number of color stripes
#' @export

createTimePlot <- function(data, stripes=11) {
  if(!is.null(data) && nrow(data)>0) {
    ggplot2::ggplot(data) +
      ggplot2::geom_point(ggplot2::aes(x=DateTime,y=Measurement,shape=as.factor(Delete), colour=as.factor(as.integer(as.factor(PlotID))%%stripes))) +
      ggplot2::facet_wrap(Date~., scales = "free_x", ncol=1)  +
      ggplot2::scale_colour_brewer(palette = "Paired") +
      ggplot2::scale_shape_manual(values = c(16, 13)) +
      ggplot2::guides(colour="none", shape="none")
  }
}

#' Plots measurements over time
#' @param data imported sunscan data
#' @param stripes number of color stripes
#' @export
#' @export

createTimePlotLAI <- function(data, stripes=11) {
  if("LAI" %in% names(data))
  {
    ggplot2::ggplot(data) +
      ggplot2::geom_point(ggplot2::aes(x=DateTime,y=LAI,shape=as.factor(Delete), colour=as.factor(as.integer(as.factor(PlotID))%%stripes)),na.rm=TRUE) +
      ggplot2::facet_wrap(Date~., scales = "free_x", ncol=1) +
      ggplot2::scale_colour_brewer(palette = "Paired") +
      ggplot2::scale_shape_manual(values = c(16, 13)) +
      ggplot2::guides(colour="none", shape="none")
  }
}





#' File name for the metadata file
#'
#' @param file filename of data file
#' @param outputfolder output folder
#'
#' @return filename
#' @export

headerFileName <- function(file, outputfolder) {
  paste0(outputfolder,"converted/meta_",stripFileExtension(file),".txt")
}


#' File name for the LAI data file
#'
#' @rdname headerFileName
#' @export

dataFileName <- function(file, outputfolder) {
  paste0(outputfolder,"converted/data_",stripFileExtension(file),".txt")
}


#' File name for the PAR data file
#'
#' @rdname headerFileName
#' @export

pardataFileName <- function(file, outputfolder) {
  paste0(outputfolder,"converted/pardata_",stripFileExtension(file),".txt")
}


#' File name for the summary file
#'
#' @rdname headerFileName
#' @export

summaryFileName <- function(file, outputfolder) {
  paste0(outputfolder,"converted/summary_",stripFileExtension(file),".txt")
}


#' File name for the boxplot image
#'
#' @rdname headerFileName
#' @export

boxplotFileName <- function(file, outputfolder) {
  paste0(outputfolder,"report/boxplot_",stripFileExtension(file),".png")
}

#' File name for the report
#'
#' @rdname headerFileName
#' @export

reportFileName <- function(file, outputfolder) {
  paste0(outputfolder,"report/",stripFileExtension(file),".html")
}

#' Saves metadata to file
#'
#' @param data data frame
#' @param file filename of original file
#' @param outputfolder output folder
#' @export

saveHeader <- function(data, file, outputfolder) {
  df <- data.frame(Property=names(data),Value=data)
  write.table(df,headerFileName(file, outputfolder),
              row.names = FALSE, quote = FALSE, sep="\t")
}

#' Saves LAI data
#'
#' @rdname saveHeader
#' @export
saveData <- function(data, file, outputfolder) {
  write.table(data,dataFileName(file, outputfolder),
              row.names = FALSE, quote = FALSE, sep="\t")
}


#' Saves PAR data
#'
#' @rdname saveHeader
#' @export

saveParData <- function(data, file, outputfolder) {
  write.table(data,pardataFileName(file, outputfolder),
              row.names = FALSE, quote = FALSE, sep="\t")
}


#' Saves summary
#'
#' @rdname saveHeader
#'
saveSummary <- function(data, file, outputfolder) {
  write.table(data,summaryFileName(file, outputfolder),
              row.names = FALSE, quote = FALSE, sep="\t")
}


#' Saves boxplot
#'
#' @rdname saveHeader
#' @export

saveBoxplot <- function(data, file, outputfolder) {
  x <- createBoxplot(data)
  ggplot2::ggsave(boxplotFileName(file, outputfolder),x,width = 14, height = 14)

}


#' Splits the data when file contains multiple headers
#'
#' @param lines vector of lines from the data file
#'
#' @return list of line vectors
#' @export

splitLines <- function(lines)
{
  start <- which(isTitleLine(lines))
  end <- c(start[-1]-1,length(lines))
  linelist <- list()
  for(i in 1:length(start))
  {
    linelist[[i]] <- c(lines[1:2],lines[start[i]:end[i]])
  }
  linelist
}


#' Fetches recursively all possible data files from a directory
#'
#' @param directory to search for data files
#' @param prefix of files to take into accoung
#' @param extension of files to take into account
#' @param excludedir subdirectory name to exclude
#'
#' @return vector of potential data files
#' @export

getFileList <- function(directory, prefix="", extension=".txt", excludedir="converted") {
  allfiles <- list.files(directory,
                         recursive=TRUE,
                         full.names=TRUE,
                         pattern=paste0("(",extension,"|",toupper(extension),")$")
  )

  isValidFile <- function(file) {
    (prefix=="" | substr(basename(tolower(file)),1,nchar(prefix)) ==  tolower(prefix)) &
      (excludedir=="" | !grepl(paste0(tolower(excludedir),"/"),tolower(file),fixed=TRUE)) &
      !grepl("converted/",tolower(file),fixed=TRUE) &
      !grepl("proceeding/",tolower(file),fixed=TRUE)
  }

  allfiles[isValidFile(allfiles)]
}

#' Converts a SunScan data file
#'
#' @param file name of file
#' @param inputfolder input folder
#' @param outputfolder output folder
#'
#' @return vector with filenames of converted data
#' @export

convertSunscanFile <- function(file, inputfolder, outputfolder) {

  path <- paste0(inputfolder,file)
  writtenfiles <- c()
  if(file.exists(path) && isSunscanFile(path) ) {
    createFolders(file, inputfolder, outputfolder)
    moveFilesToSubfolders(file, inputfolder, "original")
    path <- movedFileName(file, inputfolder,"original")
    print(paste("Started conversion of",path))
    alllines <- readLines(path)
    linelist <- splitLines(alllines)
    i <- 1
    lfile <- stripFileExtension(file)
    for(lines in linelist)
    {
      mt <- getHeader(lines,path)
      if(length(linelist)>1)
      {
        lfile <- paste0(stripFileExtension(file),"_",i)
      }
      if(length(lines)>13 )
      {
        if(mt['MeasuredVariable']=='LAI'){
          df <- getData(lines,mt['Date'])
          df <- reorderDataColumns(addPlotID(df,path))
          saveData(df, lfile, outputfolder)
          dfsummary <- createSummary(df)
          saveSummary(dfsummary, lfile, outputfolder)
          try(saveBoxplot(df, lfile, outputfolder))
          mt['ConvertedFileName'] <- paste0("data_",lfile,".txt")
          print(paste("Written converted LAI data for file",lfile))
        }
        else {
          print(paste("start par file",lfile))
          df <- getParData(lines,mt['Date'])
          df <- addPlotID(df,path)
          saveParData(df,lfile,outputfolder)
          mt['ConvertedFileName'] <- paste0("pardata_",lfile,".txt")
          print(paste("Written converted PAR data for file",lfile))
        }
      }
      saveHeader(mt, lfile, outputfolder)
      writtenfiles <- c(writtenfiles, lfile)
       i <- i + 1
    }
    print(paste0("File ", path, " converted."))
  }
  else {
    print(paste0("File ", path, " does not exists or is not a valid SunScan file."))
  }
  writtenfiles
}


#' Converts all SunScan data files in a directory (and it's subdirectories)
#'
#' @param directory to search for data files
#' @param prefix of files to take into accoung
#' @param extension of files to take into account
#' @param reportscript R script that is used to generate a report for each converted file
#' @export

convertSunScanDirectory <- function(directory, prefix="", extension=".txt", reportscript="") {
    files <- getFileList(directory, prefix, extension)
    outcount <- 0
    incount <- 0
    for(fullfile in files) {
      file <- basename(fullfile)
      folder <- dirname(fullfile)
      inputfolder <- paste0(folder,"/")
      if(isMovedFile(fullfile,"","original")) {
        nfolder <- substr(folder,1,nchar(folder)-nchar("original")-1)
        outputfolder <- paste0(nfolder,"/")
      }
      else {
        outputfolder <- paste0(folder,"/")
      }
      written <- convertSunscanFile(file, inputfolder, outputfolder)

      if(reportscript!="")
      {
        for(lfile in written)
        {
          generateReport(lfile, inputfolder, outputfolder, reportscript)
        }
      }
      incount <- incount + (length(written)>0)
      outcount <- outcount + length(written)
      print("")
    }
    print(paste("Finished: Read ",incount," files. Written ",outcount," data files"))
}


#' Reads converted data
#'
#' @param file filename
#' @param outputfolder output folder
#' @return dataframe with converted data
#' @export
readConvertedHeader <- function(file, outputfolder) {
  readr::read_delim(headerFileName(file, outputfolder), quote = "", delim="\t", show_col_types = FALSE)
}

#' @rdname readConvertedHeader
#' @export
readConvertedData <- function(file, outputfolder) {
  path <- dataFileName(file, outputfolder)
  if(file.exists(path)) {
    readr::read_delim(path, quote = "", delim="\t", show_col_types = FALSE)
  }

}

#' @rdname readConvertedHeader
#' @export
readConvertedParData <- function(file, outputfolder) {
  path <- pardataFileName(file, outputfolder)
  if(file.exists(path)) {
    readr::read_delim(path, quote = "", delim="\t", show_col_types = FALSE)
  }
}

#' @rdname readConvertedHeader
#' @export
readConvertedSummary <- function(file, outputfolder) {
  path <- summaryFileName(file, outputfolder)
  if(file.exists(path)) {
    readr::read_delim(path, quote = "", delim="\t", show_col_types = FALSE)
  }

}

#' @rdname readConvertedHeader
#' @export
readConvertedFiles <-function(file, outputfolder) {
  list('header'=readConvertedHeader(file, outputfolder),
       'data' = readConvertedData(file, outputfolder),
       'pardata' = readConvertedParData(file, outputfolder),
       'summary' = readConvertedSummary(file, outputfolder)
       )
}

#' Generates sample ID data
#'
#' Generates sample data to relate PlotNr
#' to grids, PlotIDs or MeasurementIDs
#'
#' @param data LAI-Data from SunscanFile
#' @return dataframe with IDs
#' @export
generateSamplePlotIdData <- function(data) {
  data |>
    dplyr::select(PlotNr, PlotID, Remarks, Delete) |>
    dplyr::mutate(Remarks = dplyr::if_else(Remarks=="No PlotID file given","",Remarks)) |>
    unique() |>
    dplyr::arrange(PlotNr)

}

#' Creates Sample MeasurementID data
#'
#' @param data converted sunscan data
#' @param interval time interval after which a new PlotNR is assumed
#' @return data frame with ids
#' @export

generateSampleMeasurementIdData <- function(data, interval=0) {
  md <- data |>
    dplyr::select(PlotNr, Measurement, PlotID, Remarks, Delete) |>
    dplyr::mutate(Remarks = dplyr::if_else(Remarks=="No PlotID file given","",Remarks))
  if(interval>0) {
    t0 <- data$DateTime[1]
    i<-0
    r <- character(nrow(data))
    for(n in seq_along(data$DateTime)) {
      dif <- data$DateTime[n] - t0
      if(dif>as.difftime(interval,units="secs")){
        i<- i+1
      }
      t0<-data$DateTime[n];
      r[n] <- i
    }
    md$PlotID <- r
  }
  md|>
    unique() |>
    dplyr::arrange(PlotNr, Measurement)

}

#' @rdname generateSamplePlotIdData
#' @export
generateInitialPlotIdData <- function(data) {
  data |>
    dplyr::select(PlotNr) |>
    dplyr::mutate(PlotID=formatC(PlotNr,width=3,format="d",flag="0"), Remarks = "No PlotID file given", Delete=0) |>
    unique() |>
    dplyr::arrange(PlotNr)

}

#' @rdname generateSamplePlotIdData
#' @export
generateInitialMeasurementIdData <- function(data) {
  md <- data |>
    dplyr::select(PlotNr,PlotID, Measurement, Remarks, Delete) |>
    unique() |>
    dplyr::arrange(PlotNr, Measurement)
}


#' Creates initial grid data
#'
#' @param data converted sunscan data
#' @param rows number of rows of the field
#' @param rowwise TRUE if the numbering is rowwise
#' @return data frame with grids (col/row)
#' @export
generateInitialGridData <- function(data, rows=1, rowwise = FALSE) {


  if(!is.null(data) && nrow(data)>0) {
    PlotID <- sort(unique(data[,"PlotID"]))
    n <- length(PlotID)
    cols <- ceiling(n/rows)
    if(rowwise) {
      Row <- rep(1:rows, each=cols)[1:n]
      Col <- rep(1:cols, rows)[1:n]

    }
    else {
      Row <- rep(1:rows, cols)[1:n]
      Col <- rep(1:cols, each=rows)[1:n]
    }
    data.frame(
      PlotID,
      Col,
      Row
      )
  }
}

#' Generates report concerning converted data
#'
#' @param file filename
#' @param inputfolder input folder
#' @param outputfolder output folder
#' @param reportscript script to generate the report
#' @export
generateReport <- function(file, inputfolder, outputfolder, reportscript) {
  params <- list("file"=file, "inputfolder"=inputfolder, "outputfolder"=outputfolder)
  outf <- reportFileName(file, outputfolder)
  try({
    rmarkdown::render(reportscript,params = params,
                      output_dir = dirname(outf),
                      output_file = basename(outf),
                      quiet=TRUE,
                      envir = new.env())
    print(paste0("Generated report: ",basename(outf)))
  })
}
