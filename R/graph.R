#' genere le graphB
#'
#'
#' @export
#' @import ggplot2
#' @import datasets
#' @examples
#' gen_graphB()

gen_graphB <- function(){
  data(iris,package = "datasets",envir = baseenv())
qplot(Sepal.Length, Petal.Length
        , data = iris, color = Species
        , size = Petal.Width, alpha = I(0.7))

}

#' genere le graphA
#'
#'
#' @param n indicateur numeric
#'
#' @export
#' @examples
#' gen_graphA(n=45)

gen_graphA<- function(n=42){
  speedometer(n,"coucou")
}
#' genere le graphC
#'
#' @export
#' @examples
#' gen_graphC()
gen_graphC<- function(){
  data(iris,package = "datasets",envir = baseenv())
  names(iris) = gsub('\\.', '', names(iris))
  rPlot(SepalLength ~ SepalWidth | Species, data = iris, type = 'point', color = 'Species')
}


#' genere le graphD
#' @export
#' @importFrom  dygraphs dygraph
#' @examples
#' #gen_graphD()
gen_graphD<- function(){
  data(co2,package = "datasets",envir = baseenv())
  data(co2)
  dygraph(co2)
  }
