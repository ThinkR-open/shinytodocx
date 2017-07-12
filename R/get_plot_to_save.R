
#' @title get_plot_to_save
#' @description renvoi les noms de graphique et le nom associé poru le rapport
#' @export

get_plot_to_save <- function() {

  list(
    "graphA"="le graphique A",
     "graphB"="graph B" ,
     "graphC" = "ici c 'est le C" ,
     "graphD" ="j aime le D"
  )
}

#' @title get_plot_to_save
#' @description renvoie le code source de la fonction qui genere le graphique demandé
#' @export

get_gen_plot <- function(graph) {

  g<-list(
    `gen_graphA` = "graphA",
    `gen_graphB` = "graphB",
    `gen_graphC` = "graphC",
    `gen_graphD` = "graphD"
  )
  # eval(parse(text=names(g[g==graph])))
  eval(as.name(names(g[g==graph])))
}

#' fabrique le graphique attendu et le met dans .GlobalEnv
#'
#' @param todo nom du graphique
#'
#' @export
#'
genere <- function(todo,...){
assign(todo,get_gen_plot(todo)(...),.GlobalEnv)
  }
