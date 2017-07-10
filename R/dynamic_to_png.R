#' transforme un objet plotly en png
#'
#' @param objet un graphique plotly
#' @param filename nom du fichier de sortie
#' @param ... pas utilise
#'
#' @import htmlwidgets
#' @import htmltools
#' @import webshot
#' @export
#'
plotly_to_png <- function(objet, filename = "plotly.png", ...) {
  message("... plotly to png")
  html_out <- basename(tempfile('plotly', '.', '.html'))
  on.exit(unlink(html_out), add = TRUE)
  html <- htmlwidgets::saveWidget(plotly_build(objet), f)
  webshot::webshot(html_out, filename, ...)
}

#' transforme un objet dygraph en png
#'
#' @param objet un graphique dygraph
#' @param filename nom du fichier de sortie
#' @param ... pas utilise
#'
#' @import htmlwidgets
#' @import htmltools
#' @import webshot
#' @export
#'
dygraph_to_png <- function(objet, filename = "dygraph.png", ...) {
  message("... dygraphs to png")
  html_out <- basename(tempfile('dygraph', '.', '.html'))
  on.exit(unlink(html_out), add = TRUE)
  html <- htmlwidgets::saveWidget(objet, html_out)
  webshot::webshot(html_out, filename, ...)
}

#' transforme un objet rCharts en png
#'
#' @param objet un graphique plotly
#' @param filename nom du fichier de sortie
#' @param ... pas utilise
#' @import htmlwidgets
#' @import webshot
#' @export
#'

rcharts_to_png <- function(objet, filename = "rchart.png", ...) {
  message("... rcharts to png")
  html_out <- basename(tempfile('rcharts', '.', '.html'))
  on.exit(unlink(html_out), add = TRUE)
  objet$save(destfile = html_out,standalone=TRUE)
  webshot::webshot(html_out, filename)
}
