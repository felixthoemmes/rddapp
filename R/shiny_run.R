#' Launch the Shiny App for Regression Discontinuity
#'
#' \code{shiny_run} looks for the Shiny app for rdd and launch it.
#'
#' @param app_name The name of Shiny app.
#'
#' @importFrom shiny runApp 
#'
#' @export
#'
#' @examples
#' \dontrun{
#' shiny_run()
#' shiny_run("shinyrdd")
#' }

shiny_run <- function(app_name = "shinyrdd") {
  app_path <- system.file("inst", app_name, package = "rddapp")
  if (app_path == "") {
    stop("The Shiny app does not exist in the package.")
  }

  runApp(app_path, display.mode = "normal")
}
