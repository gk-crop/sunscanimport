#' Runs Shiny app
#'
#' @param ... parameters passed to shiny::runApp
#' @export

runSunscanApp <- function(...)
{
  shiny::runApp(appDir = system.file("shiny","convert", package = "sunscanimport"),...)
}
