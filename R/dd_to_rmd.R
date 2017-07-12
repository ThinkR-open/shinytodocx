#' rajoute une ligne dans un Rmd
#'
#' @param text
#' @param chemin_rapport
#'
#' @return
#' @export
#'
#' @examples
add_to_rmd <- function(chemin_rapport,text){

  cat(text,append = TRUE,file = chemin_rapport)
  cat("\n",append = TRUE,file = chemin_rapport)
  invisible(chemin_rapport)
}


#' Title
#'
#' @param text
#' @param chemin_rapport
#' @param niveau
#'
#' @return
#' @export
#'
#' @examples
add_h <- function(chemin_rapport,text,niveau=1){

  chemin_rapport %>%
    add_br() %>%
  add_to_rmd(text=paste(paste(rep("#",niveau),collapse=""),text)) %>%
    add_br()
  invisible(chemin_rapport)
}


#' Title
#'
#' @param chemin_rapport
#'
#' @return
#' @export
#'
#' @examples
add_br <- function(chemin_rapport){
  add_to_rmd(text="\n",chemin_rapport = chemin_rapport)
  invisible(chemin_rapport)
}



#' Title
#'
#' @param chemin_rapport
#' @param graph
#'
#' @return
#' @export
#'
#' @examples
add_graph <- function(chemin_rapport,graph){

  # inherits(e,graph)
  if (eval(parse(text=paste("inherits(",graph,",'rCharts')")))){

    chemin_rapport %>%
      add_rcharts(graph = graph)


  }else{
  chemin_rapport %>%
    add_graph_normal(graph = graph)}




  invisible(chemin_rapport)
}

#' Title
#'
#' @param chemin_rapport
#' @param graph
#'
#' @return
#' @export
#'
#' @examples
add_image <- function(chemin_rapport,graph){
  fichier <- paste0(graph,".png")# avec un make.names
    chemin_rapport %>%
    add_to_rmd(text=paste0("![](",fichier,")"))
  invisible(chemin_rapport)
}

#' Title
#'
#' @param chemin_rapport
#' @param graph
#'
#' @return
#' @export
#'
#' @examples
add_graph_normal <- function(chemin_rapport,graph){


  # inherits(e,graph)

  chemin_rapport %>%
  add_to_rmd(text="```{r, echo=TRUE}") %>%
  add_to_rmd(text=graph) %>%
  add_to_rmd(text="```")

invisible(chemin_rapport)
}

#' Title
#'
#' @param chemin_rapport
#' @param graph
#'
#' @return
#' @export
#'
#' @examples
add_rcharts <- function(chemin_rapport,graph){


  # inherits(e,graph)

  chemin_rapport %>%
  add_to_rmd(text="```{r, echo=TRUE,eval=TRUE,results='asis'}") %>%
  add_to_rmd(text='library(rCharts)') %>%
  # add_to_rmd(text=paste0(graph,"$show('inline', include_assets = TRUE, cdn = FALSE)")) %>%
  add_to_rmd(text=paste0(graph,"$print()")) %>%
  add_to_rmd(text="```")






invisible(chemin_rapport)
}
