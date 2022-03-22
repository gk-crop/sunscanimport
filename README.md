[![sunscanimport status badge](https://gk-crop.r-universe.dev/badges/sunscanimport)](https://gk-crop.r-universe.dev)

# sunscanimport <img src="man/figures/logo.svg" align="right" height="139" />

Functions to import and organize LAI measurements from Sunscan device

## Installation 



```
install.packages('sunscanimport',
  repos = c(
    'https://gk-crop.r-universe.dev',
    'https://cloud.r-project.org'
  )
)
```

## Run ShinyApp

```
sunscanimport::runSunscanApp()
```

## Converting a file

```
library(sunscanimport)

file <- "paulinenaue.TXT"
inputfolder <-  "data/210908/original/"
outputfolder <- "data/210908/"

convfile <- convertSunscanFile(file,inputfolder, outputfolder)
generateReport(convfile, inputfolder, outputfolder)
```

## Converting a directory

Searches the directory recursively for Sunscan files and converts them.

```
library(sunscanimport)
convertSunScanDirectory("data/")
```

## Transforming converted data

### Create summary

```
library(sunscanimport)
data <- readr::read_delim("data/210908/converted/data_paulinenaue.txt", delim="\t")
data_summary <- createSummary(data)
```

### Transform to wide format

```
library(sunscanimport)
data <- readr::read_delim("data/210908/converted/data_paulinenaue.txt", delim="\t")
data_wide <- transformToWideFormat(data)
```

## Folder structure

Folder structure after conversion of sunscan file `name.TXT`. Optional files 
added by user are marked with *.

```
folder
  + original
  |-- name.TXT
  |-- name.CFG *
  |-- ReadMe.TXT *
  + proceeding
  |-- plotid_name.TXT *
  |-- measureid_name.TXT *
  |-- grid_name.TXT *
  + converted
  |-- data_name.TXT
  |-- summary_name.TXT
  |-- meta_name.TXT
  + report
  |-- name.html *
```
