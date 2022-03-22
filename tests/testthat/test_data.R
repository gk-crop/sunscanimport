test_that("Convert Data",{
  file <- system.file('sunscan/original/cka.TXT', package="sunscanimport")
  alllines <- readLines(file)
  linelist <- splitLines(alllines)

  pdata <- getParData(linelist[[1]],"2000-01-01")
  expect_equal(nrow(pdata),1)
  expect_equal(ncol(pdata),76)

  data <- getData(linelist[[3]],"2000-01-01")
  expect_equal(nrow(data),379)
  expect_equal(ncol(data),13)


  wdata <- transformToWideFormat(dplyr::mutate(data,PlotID=PlotNr))
  expect_equal(nrow(wdata),102)
  expect_equal(ncol(wdata),39)

})
