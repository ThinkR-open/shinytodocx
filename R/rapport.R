#' @param laliste liste des graphiques
#'
#' @title prepare_pour_rapport
#' @description ne renvoie que les truc pour le rapport
#' @export

prepare_pour_rapport <- function(laliste){
  laliste[!grepl("NORAPP",names(laliste))]# et pas laliste[-grep("NORAPP",names(laliste))]"
}

#' @param laliste_avec_dependance liste des graphiques
#'
#' @title graph_a_generer
#' @description donne la liste des graph a generer ( cad ceux quin'existent pas dans .Globalenv)
#' @export

graph_a_generer <- function(laliste_avec_dependance){
  laliste_avec_dependance[!sapply(laliste_avec_dependance,exists)]
}

#' @param a_generer liste des graphiques a generer
#'
#' @param laliste liste des graphiques
#'
#' @title genere_graph_manquant
#' @description genere les graph demandé poru un rapport s'il n'ont pas été fabriqué dans shiny
#' @export


genere_graph_manquant <- function(a_generer,laliste=laliste){
  laliste_debut <- laliste

  for ( todo in a_generer){
    message("creation de",todo)
    genere(todo)
  }

  laliste_debut
  invisible(laliste)
}

#' @param laliste_rapport liste des graphiques
#'
#' @param dossier dossier de creation du rapport
#' @param filename nom du fichier de sortie
#'
#' @title save_to_docx
#' @description fabrique le docx  a partir de laliste_rapport dans dossier
#' @import ReporteRs
#' @import assertthat
#' @export

save_to_docx <- function(laliste_rapport,dossier,filename) {

  assertthat::assert_that(has_extension(filename,"docx"))
  assertthat::assert_that(is.dir(dossier))
  assertthat::assert_that(is.writeable(dossier))

  doc <- docx(title = "Rapport"
              # ,template = file.path(find.package("MONPACKAGE"),"template","template.docx")
  ) %>%
    addParagraph("Sommaire", stylename = "TitleDoc" ) %>%
    addParagraph("") %>%
    addParagraph("") %>%
    addTOC()

#
#   # on rajoute le tableau
#   tableau_synthetique <-  iris[1:5,]
#
#   names(tableau_synthetique)
#   doc <- addFlexTable(doc,vanilla.table(tableau_synthetique)) %>%
#     addParagraph("") %>%
#     addParagraph("")

  doc <- doc %>%
    addParagraph("") %>%
    addParagraph("") %>%
  addParagraph("liste des graphiques",stylename = "Titre2")

  for (j in seq_along(laliste_rapport)) {
    print(names(laliste_rapport)[j])
    doc <- doc %>%
      addTitle( names(laliste_rapport)[j], level = 3) %>%
      addImage( filename = paste0(dossier, "/",
                                  names(laliste_rapport)[j], ".png"), height = 7.5/1.6, width = 11.5/1.6)
      }


  doc <- doc %>%
    addParagraph("un autre titre",stylename = "Titre1") %>%
    addParagraph("derniere partie",stylename = "Titre1")




  writeDoc(doc, file = file.path(dossier, filename))
  message("tentative creation pdf")
  try(system(
    paste0("libreoffice --headless --convert-to pdf --outdir ",dossier,"   ", file.path(dossier, filename))
    , intern = TRUE)
  )
}


#' @param laliste liste contenant le nom des fichiers a sauvegarder
#'
#' @param dossier dossier de sortie
#'
#' @title save_to_png
#'
#' @description fabrique les png a partir de laliste dans dossier ( ne gere pas les graph Rbase)
#' @export

save_to_png <- function(laliste,dossier){


  for (j in seq_along(laliste)) {
    print(names(laliste)[j])
    try(rm(temps_graph),silent=TRUE)
    eval(parse(text = paste0("temps_graph <- ",laliste[[j]])))
    if ( inherits(temps_graph,"ggplot")){
      ggsave(
        temps_graph,
        filename =
          gsub("NORAPP_","",paste0(dossier, "/",
                                   names(laliste)[j], ".png"))
        ,
        height = 7.5, width = 11.5)
    }


    if (inherits(temps_graph, "dygraphs")) {
      dygraph_to_png(temps_graph
                     ,
                     filename =
                       gsub("NORAPP_", "", paste0(
                         dossier, "/",
                         names(laliste)[j], ".png"
                       )))
    }

    if (inherits(temps_graph, "plotly")) {
      plotly_to_png(temps_graph
                    ,
                    filename =
                      gsub("NORAPP_", "", paste0(
                        dossier, "/",
                        names(laliste)[j], ".png"
                      )))
    }

    if (inherits(temps_graph, "rCharts")) {
      rcharts_to_png(temps_graph
                    ,
                    filename =
                      gsub("NORAPP_", "", paste0(
                        dossier, "/",
                        names(laliste)[j], ".png"
                      )))
    }


    if (!(inherits(temps_graph,"plotly") |
          inherits(temps_graph,"ggplot") |
          inherits(temps_graph,"rCharts") |
          inherits(temps_graph,"dygraphs"))){
      ggsave(
        ggplot2::ggplot()+ggplot2::ggtitle("GRAPHIQUE NON CONVERTIBLE")        ,
        filename =
          gsub("NORAPP_","",paste0(dossier, "/",
                                   names(laliste)[j], ".png"))
        ,
        height = 7.5, width = 11.5)

    }
  }
  invisible(laliste)
}

#' @param laliste liste des graphiques a exporter
#'
#' @title pimp_dependance
#' @description rajoute dans laliste les dependance qui manqueraient peut etre (exemple pour le graph 15 IL FAUT le graph 14)
#' @export
pimp_dependance <- function(laliste){

  laliste_avec_dependance <- laliste
  laliste_avec_dependance

}






#' @param laliste liste des graphiques mettre dans le rapport
#'
#' @param filename nom du fichier de sortie
#' @param open booleen ouverture du fichier apres creation
#' @param dossier chemin du dossier de creation
#'
#' @title all_ggplot_to_docx
#' @description recupere tous les ggplot in mémory pour en fare un docx et des png
#' @import ggplot2
#' @import ReporteRs
#' @export
all_ggplot_to_docx <- function(laliste = get_plot_to_save(),
                               filename = "rapport.docx",
                               open = TRUE,
                               dossier = tempdir()) {
  # incProgress(1/2)# a voir
  message(dossier)
  tmpdir <<- dossier # pour que le meme dossier soit visibible par tout shiny, pour une raison etrange tempdir() ne pointe pas tjrs vers le meme
  # browser()
  file.remove(list.files(dossier,pattern = "*.png|*.docx|*.zip|*.pdf",full.names = TRUE))# garder rs.grahics
  # browser()
  laliste %>%
    pimp_dependance() %>%
    graph_a_generer() %>%
    genere_graph_manquant(laliste=laliste) %>%
    save_to_png(dossier) %>%
    prepare_pour_rapport() %>%
    save_to_docx(dossier,filename)

  if (open) {browseURL(paste0(dossier, "/", filename))}

  invisible(list(dossier=dossier,
                 chemin_rapport=normalizePath(file.path(dossier, filename))))
}


#' @param dossier dossier à compresser
#'
#' @param nom nom du fichier zip
#'
#' @title png_to_zip
#' @description Fabrique un zip depuis le dossier de png
#' @importFrom utils browseURL data zip
#' @import assertthat
#' @export
png_to_zip <- function(dossier, nom="graphiques.zip"){
  assertthat::assert_that(has_extension(nom,"zip"))
  sortie <- file.path(dossier,nom)
  dossier %>%
    list.files(pattern = "*.png",full.names = TRUE) %>%
    zip(files=., flags = "-j",zipfile = sortie)
  invisible(sortie)
}
