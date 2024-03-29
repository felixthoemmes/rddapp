#' Launch the R Shiny App for "rddapp"
#'
#' \code{shiny_run} launches the R Shiny application for "rddapp".
#'
#' @param app_name A string specifying the name of the R Shiny app. The default is \code{"shinyrdd"}.
#'
#' @importFrom shiny runApp
#' @importFrom DT dataTableOutput renderDataTable
#'
#' @export
#'
#' @examples
#' \dontrun{
#' shiny_run()
#' shiny_run("shinyrdd")
#' }

shiny_run <- function(app_name = "shinyrdd") {
  # app_path <- system.file("inst", app_name, package = "rddapp")
  app_path <- system.file(app_name, package = "rddapp")
  if (app_path == "") {
    stop("The Shiny app does not exist in the package.")
  }

  runApp(app_path, display.mode = "normal")
}
