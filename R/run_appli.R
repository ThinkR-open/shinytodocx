
#' lance l'appli my_app
#'
#' @export
#'
#' @examples
#' \dontrun{
#' run_appli()
#' }
#'
run_appli <- function() {
  appDir <- system.file("my_app", package = "shinytodocx")
  if (appDir == "") {
    stop("Could not find. Try re-installing `shinytodocx`.", call. = FALSE)
  }
  shiny::runApp(appDir, display.mode = "normal")
}
