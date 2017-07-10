
#' @param laliste_rapport liste des graphiques
#'
#' @param dossier dossier de creation du rapport
#' @param filename nom du fichier de sortie
#'
#' @title save_to_rmds
#' @description fabrique le docx  a partir de laliste_rapport dans dossier
#' @import ReporteRs
#' @import assertthat
#' @importFrom rmarkdown render
#' @export

save_to_rmd <- function(laliste_rapport,dossier,filename) {
  assertthat::assert_that(has_extension(filename,"docx"))
  assertthat::assert_that(is.dir(dossier))
  assertthat::assert_that(is.writeable(dossier))

  tempReport <- system.file("template","template_word.rmd",package = "shinytodocx")
  chemin_rapport <- file.path(dossier,"rapport.Rmd")
  file.copy( tempReport,chemin_rapport, overwrite = TRUE)

    for (j in seq_along(laliste_rapport)) {
    chemin_rapport %>%
      add_h(names(laliste_rapport)[j],3) %>%
      add_graph(laliste_rapport[[j]])
  }

  # browser()
  rmarkdown::render(chemin_rapport,
                    # output_file = filename,
                    output_format = "all",

                    params = list(titre="ceci est un titre"),
                    envir = new.env(parent = globalenv())
  )

invisible(chemin_rapport)
}






#' @param laliste liste des graphiques mettre dans le rapport
#'
#' @param filename nom du fichier de sortie
#' @param open booleen ouverture du fichier apres creation
#' @param dossier chemin du dossier de creation
#'
#' @title all_ggplot_to_Rmd
#' @description recupere tous les ggplot in mémory pour en fare un docx et des png
#' @import ggplot2
#' @import ReporteRs
#' @export
all_ggplot_to_rmd <- function(laliste = get_plot_to_save(),
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
    # save_to_png(dossier) %>% # pas besoin si Rmd
    prepare_pour_rapport()  %>%
    save_to_rmd(dossier,filename)

  if (open) {browseURL(paste0(dossier, "/", filename))}

  invisible(list(dossier=dossier,
                 chemin_rapport=normalizePath(file.path(dossier, filename))))
}

