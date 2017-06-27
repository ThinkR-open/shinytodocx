
require(demoshinyrapport)
require(shinydashboard)
require(shiny)

#
require(leaflet)
require(dygraphs)
require(plotly)
require(rCharts)

  header <- dashboardHeader()
  sidebar <- dashboardSidebar(
    sidebarUserPanel(
      "ThinkR",
      subtitle = a(href = "#", icon("circle", class = "text-success"), "Online"),
      image = "logoT_petit.png"
    ),
    sidebarMenu(
      # Setting id makes input$tabs give the tabName of currently-selected tab
      id = "tabs",
      menuItem(
        "Dashboard",
        tabName = "dashboard",
        icon = icon("dashboard")
      ),
      menuItem(
        "Widgets",
        icon = icon("th"),
        tabName = "widgets",
        badgeLabel = "new",
        badgeColor = "green"
      ),
      menuItem(
        "Charts",
        icon = icon("bar-chart-o"),
        menuSubItem("Sub-item 1", tabName = "subitem1"),
        menuSubItem("Sub-item 2", tabName = "subitem2")
      )
      ,
      menuItem(
        "Rapport",
        tabName = "Rapport",
        icon = icon("arrow-circle-right")
      )
    )
  )

  body <- dashboardBody(tabItems(
    tabItem(
      "dashboard",
      div(p("Dashboard tab content"))
      ,
      showOutput("graphA", lib = "highcharts")
    ),
    tabItem("widgets",
            "Widgets tab content",
            plotOutput("graphB")),
    tabItem(
      "subitem1",
      "Sub-item 1 tab content",
      showOutput("graphC", lib = "polycharts")



    ),
    tabItem("subitem2",
            "Sub-item 2 tab content",
            dygraphOutput("graphD"))
    ,
    tabItem(
      "Rapport",
      "ici on active le rapport",
      uiOutput("liste_graph"),
      actionButton("genere_rapport", label = "go rapport go!"),
      actionButton("temp","OPEN TEMP DIR")
       )
  ))





  server <- function(input, output) {
    output$graphA <- renderChart2({
      # (graphA <<- gen_graphA())
      (genere("graphA",n=33))

    })

    output$graphB <- renderPlot({
      (genere("graphB"))
    })

    output$graphC <- renderChart2({
      (genere("graphC"))
    })

    output$graphD <- renderDygraph({
      (genere("graphD"))
    })


    output$liste_graph <- renderUI({
      checkboxGroupInput("choix_plot", "choose plot to save" ,
                         choices = get_plot_to_save())
    })



    observeEvent(input$genere_rapport, {
      message("on clic sur rapport")

      laliste <- get_plot_to_save()
      # on ne garde que les graph selectionnes
      laliste <- laliste[laliste %in% input$choix_plot]
      withProgress(message = 'Chargement...', value = 0, {
        all_ggplot_to_docx(laliste = laliste, open = FALSE) %>%
          list2env(envir = .GlobalEnv)
      })
    })

    observeEvent(input$temp, {
      message("Ouverture du dossier ",tempdir())
      browseURL(tempdir())


    })


  }


  shinyApp(ui = dashboardPage(header, sidebar, body),
           server = server)

