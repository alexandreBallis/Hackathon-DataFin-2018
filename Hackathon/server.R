library(shiny)
library(datasets)
library(grid)
library(gridExtra)
library(plyr)

shinyServer(function(input, output, session) {
  
  ### Exercice
  exercice = reactive({
    exercice = input$exercice
    return(exercice)
  })
  
  ### Mission
  mission = reactive({
    mission = input$mission
    return(mission)
  })
  
  ### Programmes de la mission.
  programmes = reactive({
    rap_encours = rap[[input$exercice]]
    code_pgm = unique(rap_encours$COD_PGM[rap_encours$Mission == mission()])
    code_pgm = code_pgm[code_pgm!= '']
    nom_pgm = unique(rap_encours$Programme[rap_encours$COD_PGM %in%code_pgm])
    return(list(code_pgm = code_pgm, nom_pgm = nom_pgm))
  })
  
  # Menu déroulant mission, adaptation après sélection de l'exercice.
  missions_choix = observe({
    exercice = input$exercice
    choix = unique(rap[[exercice]]$Mission)
    updateSelectizeInput(session, 'mission', choices =choix, server = TRUE)
    })

  # Boite avec informations
  output$infos_generales <- renderText({
    pg_df = data.frame(nom = programmes()$nom_pgm, code = programmes()$code_pgm)
    if (nrow(pg_df) == 0){
      return('Choisir une mission')    
      }
    
    pg_df$complet_pgm = paste0(pg_df$nom, ' (', pg_df$code, ')')
    if (nrow(pg_df) == 0){
      pg_df = data.frame(complet_pgm = )
    }
    paste0(paste0(pg_df$complet_pgm, collapse = '\n'))
  })
  
  # Subset des données pour affichage dans les onglet
  output$donnees_rap = renderDataTable({
    donnees_rap = rap[[exercice()]]
    donnees_rap = subset(donnees_rap, Mission == mission())
    donnees_rap
  })
  
  output$donnees_plr = renderDataTable({
    donnees_plr = plr[[exercice()]]
    code_pgm = programmes()$code_pgm
    code_pgm = gsub('P(.*)', '\\1', code_pgm)
    donnees_plr = subset(donnees_plr, Programme %in% code_pgm)
    donnees_plr
  })
  
  output$donnees_cge = renderDataTable({
    donnees_cge = cge
    
    col_cge = colnames(donnees_cge)
    col_balance = col_cge[grepl('Balance.Sortie', col_cge)]
    autres_col = col_cge[! grepl('Balance.Sortie', col_cge)]
    col_balance_encours = col_balance[grepl(exercice(), col_balance)]
    donnees_cge = donnees_cge[, c(autres_col, col_balance_encours)]
    
    #Filtrage sur le programme
    code_pgm = programmes()$code_pgm
    code_pgm = gsub('P(.*)', '\\1', code_pgm)
    donnees_cge = subset(donnees_cge, Programme %in% code_pgm)
    donnees_cge
  })
  
  # output$downloadData <- downloadHandler(
  #     filename = function() {
  #       paste('data-', Sys.Date(), '.csv', sep='')
  #     },
  #     content = function(con) {
  #       write.csv(data, con)
  #     }
  #   )
  
 
})

