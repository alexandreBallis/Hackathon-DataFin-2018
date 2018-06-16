# Tjs sauver les codes en UTF8, sinon erreurs

options(shiny.usecairo=T)

shinyUI(fluidPage(
  
  # Titre de l'application
  titlePanel("Informations relatives à une mission"),
  

  fluidRow(
    
    # Choix d'une année, d'une mission. Affichage en gros
    column(3,
           selectizeInput('exercice', "Sélection d'un exercice",
                          choices = annees_col_balance, 
                          selected = '', multiple = FALSE,
                          options = NULL)
           ,
           selectizeInput('mission', "Sélection d'une mission",
                          choices = NULL, multiple = FALSE)

    ),
    # tags$head(tags$style("#infos_generales {white-space: nowrap;}")),
    column(9, h2(verbatimTextOutput("infos_generales")))
    ),
  
  mainPanel(width = 12,
            # downloadButton(downloadData, label = "Download"),
            tabsetPanel(type = 'tabs',
                        tabPanel('CGE', dataTableOutput('donnees_cge')),
                        tabPanel('RAP', dataTableOutput('donnees_rap')),
                        tabPanel('PLR', dataTableOutput('donnees_plr'))
                        )
  )
))

