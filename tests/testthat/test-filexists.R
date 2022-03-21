test_that("Sample file exist", {
  expect_equal(file.exists(system.file('sunscan/original/cka.TXT', package="sunscanimport")),TRUE)
  expect_equal(file.exists(system.file('sunscan/original/grossmutz.TXT', package="sunscanimport")),TRUE)
  expect_equal(file.exists(system.file('sunscan/original/paulinenaue.TXT', package="sunscanimport")),TRUE)
})

test_that("Line check functions", {
  expect_equal(isTitleLine("Title text"),TRUE)
  expect_equal(isDataLine(scan(text="16:43:46\t7\t1\t7.8\t0.84\t515.3\t0.54\t64.3\t5.1\t\t", sep="\t", what=character(), quiet=TRUE)),TRUE)
})

test_that("Sunscan files",{
  expect_equal(isSunscanFile(system.file('sunscan/original/cka.TXT', package="sunscanimport")),TRUE)
  expect_equal(isSunscanFile(system.file('sunscan/original/grossmutz.TXT', package="sunscanimport")),TRUE)
  expect_equal(isSunscanFile(system.file('sunscan/original/paulinenaue.TXT', package="sunscanimport")),TRUE)
  expect_equal(isSunscanFile(system.file('sunscan/proceeding/plotid_paulinenaue.TXT', package="sunscanimport")),FALSE)
  
})

test_that("Structure",{
  file <- system.file('sunscan/original/cka.TXT', package="sunscanimport")
  alllines <- readLines(file)
  linelist <- splitLines(alllines)
  expect_equal(length(linelist),3)
  
  headerlist <- lapply(linelist,getHeader,path=file)
  expect_equal(headerlist[[1]]['MeasuredVariable'],c('MeasuredVariable'='PAR'))
  expect_equal(headerlist[[3]]['MeasuredVariable'],c('MeasuredVariable'='LAI'))
  
})