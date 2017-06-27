#' speedometer
#'
#' @param x indicateur, numeric
#' @param titre_jauge une chaine de caracteres
#'
#' @export
#' @import rCharts
#' @examples
#' speedometer(42,titre_jauge ='titre')
speedometer <- function(x, titre_jauge = "") {
  if (is.null(x))
    return(NULL)
  jauge <- rCharts::Highcharts$new()
  jauge$chart(
    type = "gauge",
    backgroundColor = "rgba(255, 255, 255, 0)",
    plotBackgroundColor = NULL,
    plotBackgroundImage = NULL,
    plotBorderWidth = 0,
    plotShadow = FALSE
  )


  jauge$title(text = titre_jauge)
  jauge$pane(
    startAngle = -150,
    endAngle = 150,
    background = list(
      list(
        backgroundColor = list(
          linearGradient = list(
            x1 = 0,
            y1 = 0,
            x2 = 0,
            y2 = 1
          ),
          stops = list(list(0, "#FFF"), list(1,
                                             "#FFF"))
        ),
        borderWidth = 0,
        outerRadius = "109%"
      ),
      list(
        backgroundColor = list(
          linearGradient = list(
            x1 = 0,
            y1 = 0,
            x2 = 0,
            y2 = 1
          ),
          stops = list(list(0, "#FFF"), list(1,
                                             "#FFF"))
        ),
        borderWidth = 1,
        outerRadius = "107%"
      ),
      list(),
      list(
        backgroundColor = "#FFF",
        borderWidth = 0,
        outerRadius = "105%",
        innerRadius = "103%"
      )
    )
  )
  jauge$yAxis(
    min = 0,
    max = 200,
    minorTickInterval = 10,
    minorTickWidth = 1,
    minorTickLength = 5,
    minorTickPosition = "inside",
    minorTickColor = "#666",
    tickPixelInterval = 30,
    tickWidth = 2,
    tickPosition = "inside",
    tickLength = 10,
    tickColor = "#666",
    labels = list(
      step = 2,
      rotation = "auto",
      formatter = "#!function() {return '<b>' + this.value + '%</b>'}!#"
    ),
    plotBands = list(
      list(
        thickness = "50%",
        from = 0,
        to = 80,
        color = "#55BF3B"
      ),
      list(
        thickness = "50%",
        from = 80,
        to = 100,
        color = "#DF9953"
      ),
      list(
        thickness = "50%",
        from = 100,
        to = 200,
        color = "#DF5353"
      )
    )
  )
  jauge$series(
    name = "Evolution",
    data = x,
    dataLabels = list(formatter = "#!function() {return this.y + ' %';}!#"),
    tooltip = list(valueSuffix = " %")
  )
  # Bloquer l'aiguille au maximum si la valeur dépasse le maximum affiché

  jauge$plotOptions(gauge = list(wrap = FALSE))
  jauge
}
